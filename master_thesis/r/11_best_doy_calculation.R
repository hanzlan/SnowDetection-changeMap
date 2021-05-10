library(reshape2)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")

df_orig <- read.csv("df_nooutliers_nosnowna_doycos.csv")
df_orig <- df_orig[complete.cases(df_orig), ]
df <- df_orig
df <- df[c("variable", "date","snow","lst")]
df$date <- as.character(df$date, format= "%Y-%m-%d %H:%M:%S")
df$doy <- as.numeric(strftime(df$date, format = "%j"))
df$snow <- as.numeric(df$snow)
df$lst <- as.numeric(df$lst)

###Correlate Snow/Day of the Year
doy_correlations_snow <- cor(df$snow, df$doy, use="pairwise", method="pearson")
snowdoy_pearson <- cor.test(df$snow, df$doy, method="pearson")
snowdoy_pearson

###Calculate lagged Days of the Year
for(i in 1:365) {
  new <- ifelse(df[,ncol(df)] < 367 & df[,ncol(df)] > 1, df[,ncol(df)]-1, df[,ncol(df)]+365) # Create new column
  #new <- cos(((new/366)*360)*pi/180)
  df[ , ncol(df) + 1] <- new                  # Append new column
  colnames(df)[ncol(df)] <- paste0("doy_cos_plus_", i)  # Rename column name
}

###Calculate best day of the year before cosinus
correlations_snow <- cor(df$snow, df[, !names(df) %in% c("snow", "lst", "variable", "date")] , use="pairwise", method="pearson")

cormelt1 <- melt(correlations_snow)
cormelt1 <- cormelt1[, -1]
doy1 <- c(1:366)
cormelt1$doy <- doy1
plot(cormelt1$doy, cormelt1$value, xlab="Starting Day of Year", ylab="Correlation Coefficient")

snowdoy141_pearson <- cor.test(df$snow, df$doy_cos_plus_141, method="pearson")
snowdoy141_pearson

###Calculate Cosinus of Day of the Year
A <- function(x) cos(((x/366)*360)*pi/180)
df_cos <- cbind(df[1:4], lapply(df[5:370], A) )

###Calculate best lagged day of the year (February 12 for snow (=doy_cos_plus_42))
cos_correlations_snow <- cor(df_cos$snow, df_cos[, !names(df_cos) %in% c("snow", "lst", "variable", "date")] , use="pairwise", method="pearson")

cormelt2 <- melt(cos_correlations_snow)
cormelt2 <- cormelt2[, -1]
doy <- c(1:366)
cormelt2$doy <- doy

snowdoycos42_pearson <- cor.test(df_cos$snow, df_cos$doy_cos_plus_42, method="pearson")
snowdoycos42_pearson
plot(cormelt$doy, cormelt$value, xlab="Starting Day of Year of Cosinus", ylab="Correlation Coefficient")


###Calculate best day of the year before cosinus for LST
correlations_lst <- cor(df$lst, df[, !names(df) %in% c("snow", "lst", "variable", "date")] , use="pairwise", method="pearson")

cormelt3 <- melt(correlations_lst)
cormelt3<- cormelt3[, -1]
doy3 <- c(1:366)
cormelt3$doy <- doy3
plot(cormelt3$doy, cormelt3$value, xlab="Starting Day of Year", ylab="Correlation Coefficient")

lstdoy304_pearson <- cor.test(df$lst, df$doy_cos_plus_304, method="pearson")
lstdoy304_pearson

####Calculate best cosinus day of year for lst
cos_correlations_lst <- cor(df_cos$lst, df_cos[, !names(df_cos) %in% c("variable", "snow", "date", "lst")] , use="pairwise", method="pearson")
cormelt4 <- melt(cos_correlations_lst)
cormelt4 <- cormelt4[, -1]
doy <- c(1:366)
cormelt4$doy <- doy

lstdoycos209_pearson <- cor.test(df_cos$lst, df_cos$doy_cos_plus_209, method="pearson")
lstdoycos209_pearson
plot(cormelt4$doy, cormelt4$value, xlab="Starting Day of Year of Cosinus", ylab="Correlation Coefficient")

df_cos <- df_cos[c("variable","date", "doy_cos_plus_42", "doy_cos_plus_209")]
df_merged <- merge(x=df_orig, y=df, by=c("variable", "date"), all.x=TRUE)

df_merged <- df_merged[, !names(df_merged) %in% c("doy_cos", "doy_cos_minus_324")]

write.csv(df_merged,"df_nooutliers_nosnowna_doycos.csv", row.names = FALSE)
