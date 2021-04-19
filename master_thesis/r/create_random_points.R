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
kaz_wo_glac <- readOGR(dsn = "kazbegi/kazbegi_wo_glaciers", layer = "kazbegi_wo_glaciers")
dem <-  raster("dem/dem_kazbegi_clip.tif")
dem <- crop(dem, extent(kaz_wo_glac))
dem <- mask(dem, kaz_wo_glac)
dem_df <- as.data.frame(dem)
dem_df <- as.data.frame(dem_df[!is.na(dem_df), ])

set.seed(123)
pts <- randomPoints(dem, 500, ext=extent(kaz_wo_glac))
pts_df <- as.data.frame(pts)

#rename columns
pts_df <- pts_df %>% 
  rename(
    lat = y,
    lon = x
  )

# NEW CODE STARTS HERE
sppts <- SpatialPoints(coords = pts_df, 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

pts_df$dem <- extract(dem, sppts)

spptsdf <- SpatialPointsDataFrame(coords = pts_df[,c("lon", "lat")], data=pts_df,
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))

writeOGR(obj=spptsdf, dsn="kazbegi", layer="randompoints500", driver="ESRI Shapefile") # this is in equal area projection
