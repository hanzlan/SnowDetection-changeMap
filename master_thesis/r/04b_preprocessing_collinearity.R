library(corrplot)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")

df <- read.csv("df_with_doycos_aspcos.csv")

df$lon <- round(df$lon, digits=5)
df$lat <- round(df$lat, digits=5)

df$variable <- as.character(df$variable)
df$days <- as.Date(df$days)
df$date <- as.POSIXct(df$date, format= "%Y-%m-%d %H:%M:%S")
df$date_num <- as.numeric(df$date)
cols.num <- c("snow", "lag.snow", "lst", "lag.lst", "rad", "lag.rad", "wr", "lag.wr", 
              "prec", "lag.prec", "lon", "lat", "dem", "eastness", "northness",
              "slope", "doy_cos_plus_42", "doy_cos_plus_209", 
              "aspect_plus_176", "aspect_plus_338", "date_num")
cols.int <- c("doy", "month", "year", "minute")
cols.fac <- c("variable", "sat")
df[cols.num] <- sapply(df[cols.num],as.numeric)
df[cols.int] <- sapply(df[cols.int],as.integer)
df[cols.fac] <- sapply(df[cols.fac],factor)

df <- df[which(df$days > '2002-12-31' & df$days<'2021-01-01'), ]
df <- df[complete.cases(df), ]

###Full Corrplot
correlations <- cor(df[,c(3, 6:29)], use="pairwise", method="pearson")
colnames(correlations) <- c("Day of the Year", "Snow", "Snow (day before)",
                            "Land Surface Temperature", "Land Surface Temperature (day before)",
                            "Incoming shortwave radiation", "Incoming shortwave radiation (day before)",
                            "Warming rate", "Warming rate (day before)", "Precipitation", "Precipitation (day before)",
                            "Longitude", "Latitude", "Altitude", "Eastness", "Northness", "Slope", "Month", "Year",
                            "Minute", "Cosinus of day of the year (starting from Feb 12)", "Cosinus of day of the year (starting from Jul 29)",
                            "Cosinus of aspect (starting from 176°)", "Cosinus of aspect (starting from 338°)","Numerical date")
rownames(correlations) <- c("Day of the Year", "Snow", "Snow (day before)",
                            "Land Surface Temperature", "Land Surface Temperature (day before)",
                            "Incoming shortwave radiation", "Incoming shortwave radiation (day before)",
                            "Warming rate", "Warming rate (day before)", "Precipitation", "Precipitation (day before)",
                            "Longitude", "Latitude", "Altitude", "Eastness", "Northness", "Slope", "Month", "Year",
                            "Minute", "Cosinus of day of the year (starting from Feb 12)", "Cosinus of day of the year (starting from Jul 29)",
                            "Cosinus of aspect (starting from 176°)", "Cosinus of aspect (starting from 338°)","Numerical date")
corrplot_full <- corrplot.mixed(correlations, tl.pos = "lt", order = "hclust")
res1 <- cor.mtest(df[,c(3, 6:29)], use="pairwise", method="pearson", conf.level = .95)

## specialized the insignificant value according to the significant level
corrplot(correlations, p.mat = res1$p, insig = "p-value", sig.level = -1)

###Selected Corrplot
correlations_selected <- cor(df[,c("snow","lst", "rad", "wr", "prec", "lag.prec", "lon",
                                   "lat", "dem", "slope", "year", "minute", "doy_cos_plus_42",
                                   "doy_cos_plus_209", "aspect_plus_176", "aspect_plus_338",
                                   "date_num")], use="pairwise", method="pearson")
colnames(correlations_selected) <- c("Snow", "Land Surface Temperature",
                            "Incoming shortwave radiation",
                            "Warming rate", "Precipitation", "Precipitation (day before)",
                            "Longitude", "Latitude", "Altitude", "Slope", "Year",
                            "Minute", "Cosinus of day of the year (starting from Feb 12)", "Cosinus of day of the year (starting from Jul 29)",
                            "Cosinus of aspect (starting from 176°)", "Cosinus of aspect (starting from 338°)","Numerical date")
rownames(correlations_selected) <- c("Snow", "Land Surface Temperature",
                                     "Incoming shortwave radiation",
                                     "Warming rate", "Precipitation", "Precipitation (day before)",
                                     "Longitude", "Latitude", "Altitude", "Slope", "Year",
                                     "Minute", "Cosinus of day of the year (starting from Feb 12)", "Cosinus of day of the year (starting from Jul 29)",
                                     "Cosinus of aspect (starting from 176°)", "Cosinus of aspect (starting from 338°)","Numerical date")

corrplot_selected <- corrplot.mixed(correlations_selected, tl.pos = "lt", order = "hclust", tl.cex= 7/17)

covar <- cor(df, method = "spearman", use = "complete.obs")

sapply(df, class)
data_sp <- unique(df[,c("variable","lat","lon")])

df <- df[which(df$days > '2002-12-31' & df$days<'2021-01-01'), ]
df <- df[complete.cases(df), ]


df_train <- df[which(df$year %in% c("2003", "2004", "2006", "2007", "2009", "2010", "2012",
                                    "2013", "2015", "2016", "2018", "2019")), ]
df_test <- df[which(df$year %in% c("2005", "2008", "2011", "2014", "2017", "2020")), ]

set.seed(123)
df_train_small <- df_train[sample(nrow(df_train), nrow(df_train)/20), ]
df_test_small <- df_test[sample(nrow(df_test), nrow(df_test)/20), ]

write.csv(df_train_small,"df_train_small30.csv", row.names = FALSE)
write.csv(df_test_small,"df_test_small30.csv", row.names = FALSE)
