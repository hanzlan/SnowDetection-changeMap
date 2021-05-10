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
sppts <- readOGR(dsn = "kazbegi", layer = "randompoints500")

# LOAD RASTERS INTO A LIST OBJECT. 
terra_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/rad", pattern = "terr.tif$", full.names = TRUE)
aqua_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/rad", pattern = "aqua.tif$", full.names = TRUE)

# CREATE RASTER TIME SERIES OBJECT (RTS)
d_terra <- parse_number(terra_list)
d_aqua <- parse_number(aqua_list)

d_aqua <- as.character(d_aqua)
d_terra <- as.character(d_terra)



d_aqua <- as.POSIXct(d_aqua, tz = 'UTC', format = "%Y%m%d")
d_terra <- as.POSIXct(d_terra, tz = 'UTC', format = "%Y%m%d")
rt_aqua <- rts(aqua_list,d_aqua) # creating a RasterStackTS object
rt_terra <- rts(terra_list,d_terra) # creating a RasterStackTS object

# THE NEXT LINE PROVIDES THE SAME OUTPUT AS THE FOLLOWING LONG CODE
aqua_train <- as.data.frame(extract(rt_aqua, sppts))
terra_train <- as.data.frame(extract(rt_terra, sppts))

aqua_train <- cbind(date = rownames(aqua_train), aqua_train)
terra_train <- cbind(date = rownames(terra_train),terra_train)

library(reshape2)
aqua <- melt(aqua_train)
names(aqua)[names(aqua) == "value"] <- "rad"
aqua$sat <- rep('aqua',nrow(aqua))

terra <- melt(terra_train)
names(terra)[names(terra) == "value"] <- "rad"
terra$sat <- rep('terra',nrow(terra))

write.csv(terra,"dataframes/rad_terra_500.csv", row.names = FALSE)
write.csv(aqua,"dataframes/rad_aqua_500.csv", row.names = FALSE)

