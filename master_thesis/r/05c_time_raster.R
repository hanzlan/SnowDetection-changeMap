library(raster)
library(sp) # used to create a SpatialPoint object
library(randomForest)
library(dismo)
library(rgdal)
library(rts)
library(readr)
library(dplyr)
library(reshape2)

####create year 2020 raster
setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")
dem <-  raster("dem/dem_kazbegi_clip.tif")
dem[dem > 0] <- 2020
writeRaster(dem, filename=file.path("time", "year2020.tif"), format="GTiff", overwrite=TRUE)

####create minute raster files
hours_terra = read.csv("time/hours_lst_terra_withoutgeo.csv")
hours_aqua = read.csv("time/hours_lst_aqua_withoutgeo.csv")

hours_terra$date <- substr(hours_terra$system.index, 1, 10)
hours_terra$system.index <- NULL
hours_terra$cat <- NULL
hours_terra$label <- NULL
hours_terra$value <- NULL

hours_terra <- hours_terra[which(substr(hours_terra$date, 1, 4)==2020), ]
hours_terra$date <- paste0(substr(hours_terra$date, 1, 4), substr(hours_terra$date, 6, 7), substr(hours_terra$date, 9, 10))
hours_terra$mean <- na.approx(hours_terra$mean)
hours_terra$mean <- (hours_terra$mean-44.54475/15)*60

for( i in 1:nrow(hours_terra)) {
  rf <- dem
  rf[rf > 0] <- hours_terra$mean[i]
  writeRaster(rf, filename=file.path("time/terra/2020", paste0(hours_terra$date[i], "_min_terra.tif")), format="GTiff", overwrite=TRUE)
}

hours_aqua$date <- substr(hours_aqua$system.index, 1, 10)
hours_aqua$system.index <- NULL
hours_aqua$cat <- NULL
hours_aqua$label <- NULL
hours_aqua$value <- NULL

hours_aqua <- hours_aqua[which(substr(hours_aqua$date, 1, 4)==2020), ]
hours_aqua$date <- paste0(substr(hours_aqua$date, 1, 4), substr(hours_aqua$date, 6, 7), substr(hours_aqua$date, 9, 10))
hours_aqua$mean <- na.approx(hours_aqua$mean)
hours_aqua$mean <- (hours_aqua$mean-44.54475/15)*60

for( i in 1:nrow(hours_aqua)) {
  rf <- dem
  rf[rf > 0] <- hours_aqua$mean[i]
  writeRaster(rf, filename=file.path("time/aqua/2020", paste0(hours_aqua$date[i], "_min_aqua.tif")), format="GTiff", overwrite=TRUE)
}
