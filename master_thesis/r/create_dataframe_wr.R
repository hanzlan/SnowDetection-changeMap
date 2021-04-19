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
terra_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/wr", pattern = "terra.tif$", full.names = TRUE)
aqua_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/wr", pattern = "aqua.tif$", full.names = TRUE)

# CREATE RASTER TIME SERIES OBJECT (RTS)
d_terra <- parse_number(terra_list)
d_aqua <- parse_number(aqua_list)

time_aqua <- read.csv("time/hours_lst_aqua.csv")
time_terra <- read.csv("time/hours_lst_terra.csv")
time_aqua$date <- paste0(substr(time_aqua$date, 1, 4), substr(time_aqua$date, 6, 7), substr(time_aqua$date, 9, 10))
time_terra$date <- paste0(substr(time_terra$date, 1, 4), substr(time_terra$date, 6, 7), substr(time_terra$date, 9, 10))

getdates <- function(filedays, datelist){
  d <- data.frame()
  for(x in filedays){
    rownum <- match(x, datelist$date)
    d <- rbind(d, datelist$posix[[rownum]])
  }
  return(d)
}

d_aqua <- getdates(d_aqua, time_aqua)
d_terra <- getdates(d_terra, time_terra)
colnames(d_aqua) <- "date"
colnames(d_terra) <- "date"
d_aqua <- unlist(d_aqua)
d_terra <- unlist(d_terra)

d_aqua <- as.POSIXct(d_aqua, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S")
d_terra <- as.POSIXct(d_terra, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S")
rt_aqua <- rts(aqua_list,d_aqua) # creating a RasterStackTS object
rt_terra <- rts(terra_list,d_terra) # creating a RasterStackTS object

# THE NEXT LINE PROVIDES THE SAME OUTPUT AS THE FOLLOWING LONG CODE
aqua_train <- as.data.frame(extract(rt_aqua, sppts))
terra_train <- as.data.frame(extract(rt_terra, sppts))

aqua_train <- cbind(date = rownames(aqua_train), aqua_train)
terra_train <- cbind(date = rownames(terra_train),terra_train)

library(reshape2)
aqua <- melt(aqua_train)
names(aqua)[names(aqua) == "value"] <- "wr"
aqua$sat <- rep('aqua',nrow(aqua))

terra <- melt(terra_train)
names(terra)[names(terra) == "value"] <- "wr"
terra$sat <- rep('terra',nrow(terra))

write.csv(terra,"dataframes/wr_terra_500.csv", row.names = FALSE)
write.csv(aqua,"dataframes/wr_aqua_500.csv", row.names = FALSE)

