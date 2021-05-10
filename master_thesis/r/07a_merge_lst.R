library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

lst_aqua_modelled <- list.files("validation_stacks/pred_lst_aqua/", pattern = "^lst_20191231.*\\.tif$", full.names = TRUE, ignore.case = TRUE)
lst_terra_modelled <- list.files("validation_stacks/pred_lst_terra/", pattern = "^lst_20191231.*\\.tif$", full.names = TRUE, ignore.case = TRUE)
lst_aqua <- list.files("lst/", pattern = "^20191231.*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
lst_terra <- list.files("lst/", pattern = "^20191231.*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)

lst_df1 <- data.frame(lst_aqua_modelled)
lst_df1$date <- as.character(substr(lst_df1$lst_aqua_modelled, 37, 44))

lst_df2 <- data.frame(lst_aqua)
lst_df2$date <- as.character(substr(lst_df2$lst_aqua, 5, 12))

lst_df <- merge(lst_df1, lst_df2, by="date", all.x= TRUE)
kaz<- readOGR(dsn = "kazbegi/kazbegi", layer = "kazbegi")

for( i in 1:nrow(lst_df)) {
  lst1 <- raster(lst_df$lst_aqua_modelled[i])
  if (!is.na(lst_df$lst_aqua[i])){
    lst2 <- raster(lst_df$lst_aqua[i])
    lst1 <- crop(lst1, extent(kaz))
    lst1 <- mask(lst1, kaz)
    lst2 <- crop(lst2, extent(lst1))
    lst2 <- crop(lst2, extent(kaz))
    lst2 <- mask(lst2, kaz)
    lst_clean <- cover(lst2, lst1)
    writeRaster(lst_clean, 
                filename=file.path("lst_modelled", paste0("lst_modelled_", lst_df$date[i], "_aqua.tif")),
                format="GTiff",
                overwrite=TRUE)
  } else {
    writeRaster(lst1, 
                filename=file.path("lst_modelled", paste0("lst_modelled_", lst_df$date[i], "_aqua.tif")),
                format="GTiff",
                overwrite=TRUE)
  }
}
