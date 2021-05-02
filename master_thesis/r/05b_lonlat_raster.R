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
lat <-  raster("dem/dem_kazbegi_clip.tif")
lon <-  raster("dem/dem_kazbegi_clip.tif")

dem_points <- as.data.frame(rasterToPoints(lat))
lat[lat > 0] <- dem_points$y
lon[lon > 0] <- dem_points$x

writeRaster(lat, filename=file.path("dem", "lat.tif"), format="GTiff", overwrite=TRUE)
writeRaster(lon, filename=file.path("dem", "lon.tif"), format="GTiff", overwrite=TRUE)
