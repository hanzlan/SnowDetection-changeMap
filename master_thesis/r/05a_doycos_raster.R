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
dem <-  raster("dem/dem_kazbegi_clip.tif")
days <- as.character(seq(as.Date("2020/1/1"), as.Date("2020/12/31"), "days"))
days <- paste0(substr(days, 1, 4), substr(days, 6, 7), substr(days, 9, 10))
new_doy <- c(138:366, 1:137)

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
  writeRaster(rf, filename=file.path("doy_cos/2020", paste0(doy_cos$days[i], "_doycos.tif")), format="GTiff", overwrite=TRUE)
}
