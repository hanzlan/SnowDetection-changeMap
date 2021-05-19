library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

dem <-  raster("dem/dem_kazbegi_clip.tif")
lat <-  raster("dem/lat.tif")

doy_cos_norm <- list.files("doy_cos/norm", pattern = "42\\.tif$", full.names = TRUE, ignore.case = TRUE)
doy_cos_leap <- list.files("doy_cos/leap", pattern = "42\\.tif$", full.names = TRUE, ignore.case = TRUE)

years <- c(2005, 2011, 2014, 2017)

for( x in years){
  lst_aqua <- list.files("lst_merged/", pattern = paste0("lst_", x, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  lst_terra <- list.files("lst_merged/", pattern = paste0("lst_", x, ".*terra\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  rad_aqua <- list.files("rad/", pattern = paste0("rad_", x, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  rad_terra <- list.files("rad/", pattern = paste0("rad_", x, ".*terr\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  minute_aqua <- list.files(paste0("time/aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  minute_terra <- list.files(paste0("time/terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  datenum_aqua <- list.files(paste0("time/datenum_aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  datenum_terra <- list.files(paste0("time/datenum_terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  
  for( i in 1:length(doy_cos_norm)) {
    lstaqua_rf <- raster(lst_aqua[i])
    doycos_rf <- raster(doy_cos_norm[i])
    radaqua_rf <- raster(rad_aqua[i])
    dem_rf <- dem
    datenumaqua_rf <- raster(datenum_aqua[i])
    lat_rf <- lat
    minuteaqua_rf <- raster(minute_aqua[i])
    stk <- stack(lstaqua_rf, doycos_rf, radaqua_rf, 
                 dem_rf, datenumaqua_rf, lat_rf, 
                 minuteaqua_rf)
    writeRaster(stk, filename=file.path(paste0("validation_stacks/snow_aqua/predstack_", substr(rad_aqua[i], 9, 20), ".tif")),
                format="GTiff", overwrite=TRUE)
  }
  
  for( i in 1:length(doy_cos_norm)) {
    lstterra_rf <- raster(lst_terra[i])
    doycos_rf <- raster(doy_cos_norm[i])
    radterra_rf <- raster(rad_terra[i])
    dem_rf <- dem
    datenumterra_rf <- raster(datenum_terra[i])
    lat_rf <- lat
    minuteterra_rf <- raster(minute_terra[i])
    stk <- stack(lstterra_rf, doycos_rf, radterra_rf,
                 dem_rf, datenumterra_rf,
                 lat_rf, minuteterra_rf)
    writeRaster(stk, filename=file.path(paste0("validation_stacks/snow_terra/predstack_", substr(rad_terra[i], 9, 20), ".tif")),
                format="GTiff", overwrite=TRUE)
  }
}

leapyears <- c(2008, 2020)

for( x in leapyears){
  lst_aqua <- list.files("lst_merged/", pattern = paste0("lst_", x, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  lst_terra <- list.files("lst_merged/", pattern = paste0("lst_", x, ".*terra\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  rad_aqua <- list.files("rad/", pattern = paste0("rad_", x, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  rad_terra <- list.files("rad/", pattern = paste0("rad_", x, ".*terr\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  minute_aqua <- list.files(paste0("time/aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  minute_terra <- list.files(paste0("time/terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  datenum_aqua <- list.files(paste0("time/datenum_aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  datenum_terra <- list.files(paste0("time/datenum_terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

  for( i in 1:length(doy_cos_leap)) {
    lstaqua_rf <- raster(lst_aqua[i])
    doycos_rf <- raster(doy_cos_leap[i])
    radaqua_rf <- raster(rad_aqua[i])
    dem_rf <- dem
    datenumaqua_rf <- raster(datenum_aqua[i])
    lat_rf <- lat
    minuteaqua_rf <- raster(minute_aqua[i])
    stk <- stack(lstaqua_rf, doycos_rf, radaqua_rf, 
                 dem_rf, datenumaqua_rf, lat_rf, 
                 minuteaqua_rf)
    writeRaster(stk, filename=file.path(paste0("validation_stacks/snow_aqua/predstack_", substr(rad_aqua[i], 9, 20), ".tif")),
                format="GTiff", overwrite=TRUE)
  }
  
  for( i in 1:length(doy_cos_leap)) {
    lstterra_rf <- raster(lst_terra[i])
    doycos_rf <- raster(doy_cos_leap[i])
    radterra_rf <- raster(rad_terra[i])
    dem_rf <- dem
    datenumterra_rf <- raster(datenum_terra[i])
    lat_rf <- lat
    minuteterra_rf <- raster(minute_terra[i])
    stk <- stack(lstterra_rf, doycos_rf, radterra_rf,
                 dem_rf, datenumterra_rf,
                 lat_rf, minuteterra_rf)
    writeRaster(stk, filename=file.path(paste0("validation_stacks/snow_terra/predstack_", substr(rad_terra[i], 9, 20), ".tif")),
                format="GTiff", overwrite=TRUE)
  }
}