library(raster)
library(sp) # used to create a SpatialPoint object
library(randomForest)
library(dismo)
library(rgdal)
library(rts)
library(readr)
library(dplyr)
library(reshape2)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")
df_orig <- read.csv("dataframes/df_with_doycos_aspcos.csv")

###for doy cos + 209 (LST prediction)
dem <-  raster("dem/dem_kazbegi_clip.tif")
days <- as.character(seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"))
days <- paste0(substr(days, 6, 7), substr(days, 9, 10))
new_doy <- c(158:366, 1:157)

#calculate circular radians to account for circular behaviour of day of the year
degrees <- (new_doy/366)*360
radians <- degrees*pi/180
doy_cos <- cos(radians)
doy_cos <- as.data.frame(cbind(days, doy_cos))
doy_cos$doy_cos <- as.numeric(doy_cos$doy_cos)

dem[dem > 0] <- doy_cos$doy_cos[1]
dem 

for( i in 1:nrow(doy_cos)) {
  rf <- dem
  rf[rf > 0] <- doy_cos$doy_cos[i]
  writeRaster(rf, filename=file.path("doy_cos/leap", paste0(doy_cos$days[i], "_doycos_plus_209.tif")), format="GTiff", overwrite=TRUE)
}

####for normal years
doy_cos$normdays <- c(as.character(seq(as.Date("2019/1/1"), as.Date("2019/12/31"), "days")),"xxx")
doy_cos <- doy_cos[-nrow(doy_cos),]
doy_cos$normdays <- paste0(substr(doy_cos$normdays, 6, 7), substr(doy_cos$normdays, 9, 10))

dem[dem > 0] <- doy_cos$doy_cos[1]

for( i in 1:nrow(doy_cos)) {
  rf <- dem
  rf[rf > 0] <- doy_cos$doy_cos[i]
  writeRaster(rf, filename=file.path("doy_cos/norm", paste0(doy_cos$normdays[i], "_doycos_plus_209.tif")), format="GTiff", overwrite=TRUE)
}

###for doy cos +42 (Snow prediction)
dem <-  raster("dem/dem_kazbegi_clip.tif")
days <- as.character(seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"))
days <- paste0(substr(days, 6, 7), substr(days, 9, 10))
new_doy <- c(325:366, 1:324)

#calculate circular radians to account for circular behaviour of day of the year
degrees <- (new_doy/366)*360
radians <- degrees*pi/180
doy_cos <- cos(radians)
doy_cos <- as.data.frame(cbind(days, doy_cos))
doy_cos$doy_cos <- as.numeric(doy_cos$doy_cos)

dem[dem > 0] <- doy_cos$doy_cos[1]
dem 

for( i in 1:nrow(doy_cos)) {
  rf <- dem
  rf[rf > 0] <- doy_cos$doy_cos[i]
  writeRaster(rf, filename=file.path("doy_cos/leap", paste0(doy_cos$days[i], "_doycos_plus_42.tif")), format="GTiff", overwrite=TRUE)
}

####for normal years
doy_cos$normdays <- c(as.character(seq(as.Date("2019/1/1"), as.Date("2019/12/31"), "days")),"xxx")
doy_cos <- doy_cos[-nrow(doy_cos),]
doy_cos$normdays <- paste0(substr(doy_cos$normdays, 6, 7), substr(doy_cos$normdays, 9, 10))

dem[dem > 0] <- doy_cos$doy_cos[1]

for( i in 1:nrow(doy_cos)) {
  rf <- dem
  rf[rf > 0] <- doy_cos$doy_cos[i]
  writeRaster(rf, filename=file.path("doy_cos/norm", paste0(doy_cos$normdays[i], "_doycos_plus_42.tif")), format="GTiff", overwrite=TRUE)
}
