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

####create minute raster files
hours_terra = read.csv("time/hours_lst_terra_withoutgeo.csv")
hours_aqua = read.csv("time/hours_lst_aqua_withoutgeo.csv")

hours_terra$date <- substr(hours_terra$system.index, 1, 10)
hours_terra$date <- gsub("_", "-", hours_terra$date)
hours_terra$system.index <- NULL
hours_terra$cat <- NULL
hours_terra$label <- NULL
hours_terra$value <- NULL

hours_terra <- hours_terra %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))

hours_terra$mean <- na.approx(hours_terra$mean)

hours_aqua$date <- substr(hours_aqua$system.index, 1, 10)
hours_aqua$date <- gsub("_", "-", hours_aqua$date)
hours_aqua$system.index <- NULL
hours_aqua$cat <- NULL
hours_aqua$label <- NULL
hours_aqua$value <- NULL

hours_aqua <- hours_aqua %>%
  mutate(date = as.Date(date)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"))

hours_aqua$mean <- na.approx(hours_aqua$mean)

val <- c(2005, 2008, 2011, 2014, 2017, 2020)

for (x in val) {
  hours_terr <- hours_terra[which(substr(hours_terra$date, 1, 4)==x), ]
  hours_terr$date <- paste0(substr(hours_terr$date, 1, 4), substr(hours_terr$date, 6, 7), substr(hours_terr$date, 9, 10))
  hours_terr$mean <- (hours_terr$mean-44.54475/15)*60
  
  for( i in 1:nrow(hours_terr)) {
    rf <- dem
    rf[rf > 0] <- hours_terr$mean[i]
    writeRaster(rf, filename=file.path(paste0("time/terra/", x), paste0(hours_terr$date[i], "_min_terra.tif")), format="GTiff", overwrite=TRUE)
  }


  hours_aqu <- hours_aqua[which(substr(hours_aqua$date, 1, 4)==x), ]
  hours_aqu$date <- paste0(substr(hours_aqu$date, 1, 4), substr(hours_aqu$date, 6, 7), substr(hours_aqu$date, 9, 10))
  hours_aqu$mean <- (hours_aqu$mean-44.54475/15)*60

  for( i in 1:nrow(hours_aqu)) {
    rf <- dem
    rf[rf > 0] <- hours_aqu$mean[i]
    writeRaster(rf, filename=file.path(paste0("time/aqua/", x), paste0(hours_aqu$date[i], "_min_aqua.tif")), format="GTiff", overwrite=TRUE)
  }
}
