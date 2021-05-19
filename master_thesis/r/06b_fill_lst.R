library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")


years <- c(2005, 2008, 2011, 2014, 2017, 2020)

for(year in years){
  lst_pred_aqua <- list.files("validation_stacks/pred_lst_aqua/", pattern = paste0("lst_", year, ".*\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  lst_orig_aqua <- list.files("lst_new/", pattern = paste0("^",year, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  
  x = substr(lst_pred_aqua, 37, 44)
  y = substr(lst_orig_aqua, 9, 16)

  df_pred <- as.data.frame(cbind(lst_pred_aqua, x))
  df_orig <- as.data.frame(cbind(lst_orig_aqua, y))
  df <- merge(df_pred, df_orig, by.x = "x", 
                     by.y = "y", all.x = TRUE, all.y = FALSE)
  for (row in 1:nrow(df)) {
    if (!is.na(df[row, "lst_orig_aqua"])){
      ras_pred <- raster(df[row, "lst_pred_aqua"])
      ras_orig <- raster(df[row, "lst_orig_aqua"])
      newras <- cover(ras_orig, ras_pred)
      writeRaster(newras, filename=file.path(paste0("lst_merged/lst_", df[row, "x"], "aqua.tif")),
                  format="GTiff", overwrite=TRUE)
    } else {
      file.copy(df[row, "lst_pred_aqua"], file.path(paste0("lst_merged/lst_", df[row, "x"], "aqua.tif")), overwrite = TRUE)
    }
  }
}
  
for(year in years){
  lst_pred_terra <- list.files("validation_stacks/pred_lst_terra/", pattern = paste0("lst_", year, ".*\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  lst_orig_terra <- list.files("lst_new/", pattern = paste0("^",year, ".*terr\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  
  x = substr(lst_pred_terra, 38, 45)
  y = substr(lst_orig_terra, 9, 16)
  
  df_pred <- as.data.frame(cbind(lst_pred_terra, x))
  df_orig <- as.data.frame(cbind(lst_orig_terra, y))
  df <- merge(df_pred, df_orig, by.x = "x", 
              by.y = "y", all.x = TRUE, all.y = FALSE)
  for (row in 1:nrow(df)) {
    if (!is.na(df[row, "lst_orig_terra"])){
      ras_pred <- raster(df[row, "lst_pred_terra"])
      ras_orig <- raster(df[row, "lst_orig_terra"])
      newras <- cover(ras_orig, ras_pred)
      writeRaster(newras, filename=file.path(paste0("lst_merged/lst_", df[row, "x"], "terra.tif")),
                  format="GTiff", overwrite=TRUE)
    } else {
      file.copy(df[row, "lst_pred_terra"], file.path(paste0("lst_merged/lst_", df[row, "x"], "terra.tif")), overwrite = TRUE)
    }
  }
}