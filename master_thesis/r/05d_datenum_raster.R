library(raster)
library(sp) # used to create a SpatialPoint object
library(randomForest)
library(dismo)
library(rgdal)
library(rts)
library(readr)
library(dplyr)
library(reshape2)
library(tidyr)

####create year 2020 raster
setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

df <- read.csv("dataframes/df_with_topo.csv")
dem <-  raster("dem/dem_kazbegi_clip.tif")
df <- df[ , names(df) %in% c("date","days", "sat")]

df_uniq <- as.data.frame(unique(df$date))
colnames(df_uniq) <- "date"

df_aqua <- filter(df,sat == "aqua")
df_terra <- filter(df,sat == "terra")

df_aqua_uniq <- unique( df_aqua[ , 1:3 ] )
df_aqua_uniq$date <- as.POSIXct(df_aqua_uniq$date)
df_num_aqua <- df_aqua_uniq %>%
  mutate(date1 = as.Date(date)) %>%
  complete(date1 = seq.Date(min(date1), max(date1), by="day"))
df_num_aqua$datenum <- as.numeric(df_num_aqua$date)
df_num_aqua$datenum <- na.approx(df_num_aqua$datenum)
df_num_aqua <- df_num_aqua[ , names(df_num_aqua) %in% c("date1","datenum")]
colnames(df_num_aqua) <- c("date", "datenum")

df_terra_uniq <- unique( df_terra[ , 1:3 ] )
df_terra_uniq$date <- as.POSIXct(df_terra_uniq$date)
df_num_terra <- df_terra_uniq %>%
  mutate(date1 = as.Date(date)) %>%
  complete(date1 = seq.Date(min(date1), max(date1), by="day"))
df_num_terra$datenum <- as.numeric(df_num_terra$date)
df_num_terra$datenum <- na.approx(df_num_terra$datenum)
df_num_terra <- df_num_terra[ , names(df_num_terra) %in% c("date1","datenum")]
colnames(df_num_terra) <- c("date", "datenum")

val <- c(2005, 2008, 2011, 2014, 2017, 2020)

for (x in val) {
  df_num_aqu <- df_num_aqua[which(substr(df_num_aqua$date, 1, 4)==x), ]

  for( i in 1:nrow(df_num_aqu)) {
    rf <- dem
    rf[rf > 0] <- df_num_aqu$datenum[i]
    writeRaster(rf, filename=file.path(paste0("time/datenum_aqua/", x), 
                                       paste0(df_num_aqu$date[i], "_datenum_aqua.tif")),
                format="GTiff", overwrite=TRUE)
  }
  
  df_num_terr <- df_num_terra[which(substr(df_num_terra$date, 1, 4)==x), ]
  
  for( i in 1:nrow(df_num_terr)) {
    rf <- dem
    rf[rf > 0] <- df_num_terr$datenum[i]
    writeRaster(rf, filename=file.path(paste0("time/datenum_terra/", x), 
                                       paste0(df_num_terr$date[i], "_datenum_terra.tif")), 
                format="GTiff", overwrite=TRUE)
  }
}
