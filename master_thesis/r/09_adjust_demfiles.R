library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/")

dem <-  raster("Data/dem/dem_kazbegi_clip.tif")

eastness <- raster("Data/dem/eastness_kazbegi_clip.tif")
northness <- raster("Data/dem/northness_kazbegi_clip.tif")
slope <- raster("Data/dem/slope_kazbegi_clip.tif")
lat <- raster("Data/dem/lat.tif")
lon <- raster("Data/dem/lon.tif")

eastness <- crop(eastness, extent(dem))
eastness <- mask(eastness, dem)

northness <- crop(northness, extent(dem))
northness <- mask(northness, dem)

slope <- crop(slope, extent(dem))
slope <- mask(slope, dem)

lat <- crop(lat, extent(dem))
lat <- mask(lat, dem)

lon <- crop(lon, extent(dem))
lon <- mask(lon, dem)

writeRaster(eastness, 
            filename=file.path("Data/dem", "eastness_kazbegi_clip.tif"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(northness, 
            filename=file.path("Data/dem", "northness_kazbegi_clip.tif"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(slope, 
            filename=file.path("Data/dem", "slope_kazbegi_clip.tif"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(lat, 
            filename=file.path("Data/dem", "lat.tif"),
            format="GTiff",
            overwrite=TRUE)

writeRaster(lon, 
            filename=file.path("Data/dem", "lon.tif"),
            format="GTiff",
            overwrite=TRUE)
