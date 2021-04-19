library(raster)
library(sp) # used to create a SpatialPoint object
library(randomForest)
library(dismo)
library(rgdal)
library(rts)
library(readr)
library(dplyr)
library(reshape2)
library(stringr)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")
sppts <- readOGR(dsn = "kazbegi", layer = "randompoints500")

# LOAD RASTERS INTO A LIST OBJECT. 
terra_list <- list.files("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/prec", pattern = "_prec.tif$", full.names = TRUE)

# CREATE RASTER TIME SERIES OBJECT (RTS)
d_terra <- parse_number(terra_list)

time_terra <- read.csv("time/hours_lst_terra.csv")
time_terra$date <- paste0(substr(time_terra$date, 1, 4), substr(time_terra$date, 6, 7), substr(time_terra$date, 9, 10))

getdates <- function(filedays, datelist){
  d <- data.frame()
  for(x in filedays){
    rownum <- match(x, datelist$date)
    d <- rbind(d, datelist$posix[[rownum]])
  }
  return(d)
}

d_terra <- getdates(d_terra, time_terra)
colnames(d_terra) <- "date"
d_terra <- unlist(d_terra)

d_terra <- as.POSIXct(d_terra, tz = 'UTC', format = "%Y-%m-%d %H:%M:%S")
rt_terra <- rts(terra_list,d_terra) # creating a RasterStackTS object

# THE NEXT LINE PROVIDES THE SAME OUTPUT AS THE FOLLOWING LONG CODE
terra_train <- as.data.frame(extract(rt_terra, sppts))

terra_train <- cbind(date = rownames(terra_train),terra_train)

library(reshape2)
terra <- melt(terra_train)
names(terra)[names(terra) == "value"] <- "prec"
terra$sat <- rep('terra',nrow(terra))

write.csv(terra,"dataframes/prec_500.csv", row.names = FALSE)
