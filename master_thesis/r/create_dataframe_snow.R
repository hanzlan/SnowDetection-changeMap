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
snow_terra_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/snow_new", pattern = "terr.tif$", full.names = TRUE)
snow_aqua_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/snow_new", pattern = "aqua.tif$", full.names = TRUE)

# CREATE RASTER TIME SERIES OBJECT (RTS)
d_terra <- parse_number(snow_terra_list)
d_aqua <- parse_number(snow_aqua_list)

d_aqua <- as.character(d_aqua)
d_terra <- as.character(d_terra)

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
snow_rt_aqua <- rts(snow_aqua_list,d_aqua) # creating a RasterStackTS object
snow_rt_terra <- rts(snow_terra_list,d_terra) # creating a RasterStackTS object

plot(snow_rt_aqua)
plot(snow_rt_terra)

# THE NEXT LINE PROVIDES THE SAME OUTPUT AS THE FOLLOWING LONG CODE
snow_aqua_train <- as.data.frame(extract(snow_rt_aqua, sppts))
snow_terra_train <- as.data.frame(extract(snow_rt_terra, sppts))

snow_aqua_train <- cbind(date = rownames(snow_aqua_train), snow_aqua_train)
snow_terra_train <- cbind(date = rownames(snow_terra_train), snow_terra_train)

library(reshape2)
snow_aqua <- melt(snow_aqua_train)
names(snow_aqua)[names(snow_aqua) == "value"] <- "snow"
snow_aqua$sat <- rep('aqua',nrow(snow_aqua))

snow_terra <- melt(snow_terra_train)
names(snow_terra)[names(snow_terra) == "value"] <- "snow"
snow_terra$sat <- rep('terra',nrow(snow_terra))

write.csv(snow_terra,"dataframes/snow_terra_500.csv", row.names = FALSE)
write.csv(snow_aqua,"dataframes/snow_aqua_500.csv", row.names = FALSE)
