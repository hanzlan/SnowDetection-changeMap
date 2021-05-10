library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/")

rad_new <- list.files("GEE/Download_Leonie/newfiles_RAD/", pattern = "^.*\\.tif$", full.names = TRUE, ignore.case = TRUE)

rad_df <- data.frame(rad_new)
rad_df$name <- as.character(substr(rad_df$rad_new, 34, 45))

kaz<- readOGR(dsn = "Data/kazbegi/kazbegi", layer = "kazbegi")

for( i in 1:nrow(rad_df)) {
  rad <- raster(rad_df$rad_new[i])
  rad <- crop(rad, extent(kaz))
  rad <- mask(rad, kaz)
  writeRaster(rad, 
                filename=file.path("Data/rad_new", paste0("rad_", rad_df$name[i], ".tif")),
                format="GTiff",
                overwrite=TRUE)
}

rad_new <- list.files("Data/rad_new/", pattern = "^.*\\.tif$", full.names = TRUE, ignore.case = TRUE)
rad_df <- data.frame(rad_new)
rad_df$name <- as.character(substr(rad_df$rad_new, 18, 29))

dem <-  raster("Data/dem/dem_kazbegi_clip.tif")

for( i in 1:nrow(rad_df)) {
  rad <- raster(rad_df$rad_new[i])
  rad <- crop(rad, extent(dem))
  rad <- mask(rad, dem)
  writeRaster(rad, 
              filename=file.path("Data/rad_new", paste0("rad_", rad_df$name[i], ".tif")),
              format="GTiff",
              overwrite=TRUE)
}


