library(reshape2)
library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

###load points
sppts <- readOGR(dsn = "kazbegi", layer = "randompoints500")

###load tifs
dem <-  raster("dem/dem_kazbegi_large.tif")
aspect <- terrain(dem, opt='aspect', unit='degrees')
plot(aspect)

pts_df <- as.data.frame(sppts)
pts_df$aspect <- extract(aspect, sppts)

##assign variable "Vxxx"
variable <- 1:500
variable <- paste0("V", as.character(variable))

pts_df[1:5] <- list(NULL)
pts_df <- cbind(pts_df, variable)

df_orig <- read.csv("dataframes/df_nooutliers_nosnowna_doycos.csv")
df_orig <- df_orig[complete.cases(df_orig), ]
df <- df_orig
df <- df[c("variable", "date","snow", "lst")]

df <- merge(df,pts_df,by="variable")

###Correlate Snow with Aspect
asp_correlations_snow <- cor(df$snow, df$aspect, use="pairwise", method="pearson")
snowasp_pearson <- cor.test(df$snow, df$aspect, method="pearson")
snowasp_pearson

###Correlate LST with Aspect
asp_correlations_lst <- cor(df$lst, df$aspect, use="pairwise", method="pearson")
lstasp_pearson <- cor.test(df$lst, df$aspect, method="pearson")
lstasp_pearson

for(i in 1:359) {
  new <- ifelse(df[,ncol(df)] < 360 & df[,ncol(df)] > 1, df[,ncol(df)]-1, df[,ncol(df)]+359) # Create new column
  #new <- cos(((new/366)*360)*pi/180)
  df[ , ncol(df) + 1] <- new                  # Append new column
  colnames(df)[ncol(df)] <- paste0("aspect_plus_", i)  # Rename column name
}

df$snow <- as.numeric(df$snow)
df$lst <- as.numeric(df$lst)
degrees <- df$aspect

###Calculate best aspect angle before cosinus for snow
correlations_snow <- cor(df$snow, df[, !names(df) %in% c("snow", "lst", "variable", "date", "degrees")] , use="pairwise", method="pearson")

cormelt1 <- melt(correlations_snow)
cormelt1 <- cormelt1[, -1]
aspect <- c(0:359)
cormelt1$aspect <- aspect
plot(cormelt1$aspect, cormelt1$value, xlab="Starting Angle of Aspect", ylab="Correlation Coefficient")

snowasp231_pearson <- cor.test(df$snow, df$aspect_plus_231, method="pearson")
snowasp231_pearson

###Calculate best aspect angle before cosinus for lst
correlations_lst <- cor(df$lst, df[, !names(df) %in% c("snow", "lst", "variable", "date", "degrees")] , use="pairwise", method="pearson")

cormelt2 <- melt(correlations_lst)
cormelt2 <- cormelt2[, -1]
aspect <- c(0:359)
cormelt2$aspect <- aspect
plot(cormelt2$aspect, cormelt2$value, xlab="Starting Angle of Aspect", ylab="Correlation Coefficient")

lstasp231_pearson <- cor.test(df$lst, df$aspect_plus_231, method="pearson")
lstasp231_pearson

###calculate cosinus of aspect
A <- function(x) cos(x*pi/180)
df_cos <- cbind(df[1:4], lapply(df[5:364], A) )

###calculate best cosinus of aspect for snow (338° Degrees)
correlations_snow <- cor(df_cos$snow, df_cos[, !names(df_cos) %in% c("variable", "snow", "lst", "date", "degrees")] , use="pairwise", method="pearson")
cormelt_snow <- melt(correlations_snow)
cormelt_snow <- cormelt_snow[, -1]
asp <- c(0:359)
cormelt_snow$asp <- asp

snowaspcos338_pearson <- cor.test(df_cos$snow, df_cos$aspect_plus_338, method="pearson")
snowaspcos338_pearson
plot(cormelt_snow$asp, cormelt_snow$value, xlab="Starting Cosinus Angle of Aspect", ylab="Correlation Coefficient")

###Calculate best cosinus for LST (176°)
correlations_lst <- cor(df_cos$lst, df_cos[, !names(df_cos) %in% c("variable", "lst", "snow", "date", "degrees")] , use="pairwise", method="pearson")
cormelt_lst <- melt(correlations_lst)
cormelt_lst <- cormelt_lst[, -1]
asp <- c(0:359)
cormelt_lst$asp <- asp

lstaspcos176_pearson <- cor.test(df_cos$lst, df_cos$aspect_plus_176, method="pearson")
lstaspcos176_pearson
plot(cormelt_lst$asp, cormelt_lst$value, xlab="Starting Cosinus Angle of Aspect", ylab="Correlation Coefficient")

####merge results
df_cos <- df_cos[c("variable","date", "aspect_plus_176", "aspect_plus_338")]

df_merged <- merge(x=df_orig, y=df_cos, by=c("variable", "date"), all.x=TRUE)

write.csv(df_merged,"dataframes/df_with_doycos_aspcos.csv", row.names = FALSE)
