library(ggplot2)

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

gp <- ggplot(aes(x = snow, y = lst), 
             data = df)
gp + geom_point(alpha = 0.7) + geom_smooth(method = "lm", se = FALSE)
