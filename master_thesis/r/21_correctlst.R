library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/")

lst_new <- list.files("GEE/Download_Leonie/newfiles_LST/", pattern = "^.*\\.tif$", full.names = TRUE, ignore.case = TRUE)

lst_df <- data.frame(lst_new)
lst_df$name <- as.character(substr(lst_df$lst_new, 34, 45))

dem <-  raster("Data/dem/dem_kazbegi_clip.tif")

for( i in 1:nrow(lst_df)) {
  lst <- raster(lst_df$lst_new[i])
  lst <- crop(lst, extent(dem))
  lst <- mask(lst, dem)
  writeRaster(lst, 
              filename=file.path("Data/lst_new", paste0(lst_df$name[i], ".tif")),
              format="GTiff",
              overwrite=TRUE)
}


