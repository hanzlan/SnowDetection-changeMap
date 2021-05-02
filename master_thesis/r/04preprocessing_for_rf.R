setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")

df <- read.csv("df_nooutliers_nosnowna_doycos.csv")

df$lon <- round(df$lon, digits=5)
df$lat <- round(df$lat, digits=5)

df$variable <- as.character(df$variable)
df$days <- as.Date(df$days)
df$date <- as.POSIXct(df$date, format= "%Y-%m-%d %H:%M:%S")
cols.num <- c("doy","snow", "lag.snow", "month", "year", "minute")
cols.fac <- c("variable", "sat")
df[cols.num] <- sapply(df[cols.num],as.numeric)
df[cols.fac] <- sapply(df[cols.fac],factor)

sapply(df, class)
data_sp <- unique(df[,c("variable","lat","lon")])

df <- df[complete.cases(df), ]
df <- df[which(df$days > '2002-12-31' & df$days<'2021-01-01'), ]
df_train <- df[which(df$year %in% c("2003", "2004", "2006", "2007", "2009", "2010", "2012",
                                    "2013", "2015", "2016", "2018", "2019")), ]
df_test <- df[which(df$year %in% c("2005", "2008", "2011", "2014", "2017", "2020")), ]

set.seed(123)
df_train_small <- df_train[sample(nrow(df_train), nrow(df_train)/10), ]
df_test_small <- df_test[sample(nrow(df_test), nrow(df_test)/10), ]

write.csv(df_train_small,"df_train_small.csv", row.names = FALSE)
write.csv(df_test_small,"df_test_small.csv", row.names = FALSE)
