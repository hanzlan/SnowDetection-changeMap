library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/")

snow_new <- list.files("GEE/Download_Leonie/newfiles_SNOW/", pattern = "^.*\\.tif$", full.names = TRUE, ignore.case = TRUE)
snow_df <- data.frame(snow_new)
snow_df$name <- as.character(substr(snow_df$snow_new, 35, 46))

dem <-  raster("Data/dem/dem_kazbegi_clip.tif")

for( i in 1:nrow(snow_df)) {
  snow <- raster(snow_df$snow_new[i])
  snow <- crop(snow, extent(dem))
  snow <- mask(snow, dem)
  writeRaster(snow, 
              filename=file.path("Data/snow_new", paste0("snow_", snow_df$name[i], ".tif")),
              format="GTiff",
              overwrite=TRUE)
}


