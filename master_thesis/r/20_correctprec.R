library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/")

prec_new <- list.files("GEE/Download_Leonie/newfiles_Prec/", pattern = "^.*\\.tif$", full.names = TRUE, ignore.case = TRUE)

prec_df <- data.frame(prec_new)
prec_df$name <- as.character(substr(prec_df$prec_new, 35, 47))

dem <-  raster("Data/dem/dem_kazbegi_clip.tif")

for( i in 1:nrow(prec_df)) {
  prec <- raster(prec_df$prec_new[i])
  prec <- crop(prec, extent(dem))
  prec <- mask(prec, dem)
  writeRaster(prec, 
              filename=file.path("Data/prec_new", paste0(prec_df$name[i], ".tif")),
              format="GTiff",
              overwrite=TRUE)
}


