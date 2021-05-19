library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

dem <-  raster("dem/dem_kazbegi_clip.tif")
lat <-  raster("dem/lat.tif")
lon <-  raster("dem/lon.tif")

doy_cos_norm <- list.files("doy_cos/norm", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
doy_cos_leap <- list.files("doy_cos/leap", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

years <- c(2005, 2011, 2014, 2017)

for( x in years){
  
rad_aqua <- list.files("rad/", pattern = paste0("rad_", x, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
rad_terra <- list.files("rad/", pattern = paste0("rad_", x, ".*terr\\.tif$"), full.names = TRUE, ignore.case = TRUE)
prec <- list.files("prec/", pattern = paste0("^", x, ".*\\.tif$"), full.names = TRUE, ignore.case = TRUE)
minute_aqua <- list.files(paste0("time/aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
minute_terra <- list.files(paste0("time/terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
datenum_aqua <- list.files(paste0("time/datenum_aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
datenum_terra <- list.files(paste0("time/datenum_terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

for( i in 1:length(doy_cos_norm)) {
  doycos_rf <- raster(doy_cos_norm[i])
  radaqua_rf <- raster(rad_aqua[i])
  dem_rf <- dem
  datenumaqua_rf <- raster(datenum_aqua[i])
  minuteaqua_rf <- raster(minute_aqua[i])
  lat_rf <- lat
  lon_rf <- lon
  prec_rf <- raster(prec[i])
  stk <- stack(doycos_rf, radaqua_rf, dem_rf, 
               datenumaqua_rf, minuteaqua_rf,
               lat_rf, lon_rf, prec_rf)
  writeRaster(stk, filename=file.path(paste0("validation_stacks/lst_aqua/predstack_", substr(rad_aqua[i], 9, 20), ".tif")),
              format="GTiff", overwrite=TRUE)
}

for( i in 1:length(doy_cos_norm)) {
  doycos_rf <- raster(doy_cos_norm[i])
  radterra_rf <- raster(rad_terra[i])
  dem_rf <- dem
  datenumterra_rf <- raster(datenum_terra[i])
  minuteterra_rf <- raster(minute_terra[i])
  lat_rf <- lat
  lon_rf <- lon
  prec_rf <- raster(prec[i])
  stk <- stack(doycos_rf, radterra_rf, dem_rf, 
               datenumterra_rf, minuteterra_rf,
               lat_rf, lon_rf, prec_rf)
  writeRaster(stk, filename=file.path(paste0("validation_stacks/lst_terra/predstack_", substr(rad_terra[i], 9, 20), ".tif")),
              format="GTiff", overwrite=TRUE)
}
}

leapyears <- c(2008, 2020)

for( x in leapyears){
  
  rad_aqua <- list.files("rad/", pattern = paste0("rad_", x, ".*aqua\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  rad_terra <- list.files("rad/", pattern = paste0("rad_", x, ".*terr\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  prec <- list.files("prec/", pattern = paste0("^", x, ".*\\.tif$"), full.names = TRUE, ignore.case = TRUE)
  minute_aqua <- list.files(paste0("time/aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  minute_terra <- list.files(paste0("time/terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  datenum_aqua <- list.files(paste0("time/datenum_aqua/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  datenum_terra <- list.files(paste0("time/datenum_terra/", x, "/"), pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
  
  for( i in 1:length(doy_cos_leap)) {
    doycos_rf <- raster(doy_cos_leap[i])
    radaqua_rf <- raster(rad_aqua[i])
    dem_rf <- dem
    datenumaqua_rf <- raster(datenum_aqua[i])
    minuteaqua_rf <- raster(minute_aqua[i])
    lat_rf <- lat
    lon_rf <- lon
    prec_rf <- raster(prec[i])
    stk <- stack(doycos_rf, radaqua_rf, dem_rf, 
                 datenumaqua_rf, minuteaqua_rf,
                 lat_rf, lon_rf, prec_rf)
    writeRaster(stk, filename=file.path(paste0("validation_stacks/lst_aqua/predstack_", substr(rad_aqua[i], 9, 20), ".tif")),
                format="GTiff", overwrite=TRUE)
  }
  
  for( i in 1:length(doy_cos_leap)) {
    doycos_rf <- raster(doy_cos_leap[i])
    radterra_rf <- raster(rad_terra[i])
    dem_rf <- dem
    datenumterra_rf <- raster(datenum_terra[i])
    minuteterra_rf <- raster(minute_terra[i])
    lat_rf <- lat
    lon_rf <- lon
    prec_rf <- raster(prec[i])
    stk <- stack(doycos_rf, radterra_rf, dem_rf, 
                 datenumterra_rf, minuteterra_rf,
                 lat_rf, lon_rf, prec_rf)
    writeRaster(stk, filename=file.path(paste0("validation_stacks/lst_terra/predstack_", substr(rad_terra[i], 9, 20), ".tif")),
                format="GTiff", overwrite=TRUE)
  }
}
