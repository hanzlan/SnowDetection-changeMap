library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

dem <-  raster("dem/dem_kazbegi_clip.tif")
lat <-  raster("dem/lat.tif")
lon <-  raster("dem/lon.tif")
year <- raster("time/year2019.tif")

doy_cos <- list.files("doy_cos/2019", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
rad_aqua <- list.files("rad/", pattern = "^2019.*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
rad_terra <- list.files("rad/", pattern = "^2019.*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)
lagrad_aqua <- list.files("rad/", pattern = "^(20181231|2019).*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
lagrad_aqua <- head(lagrad_aqua,-1)
lagrad_terra <- list.files("rad/", pattern = "^(20181231|2019).*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)
lagrad_terra <- head(lagrad_terra,-1)
minute_aqua <- list.files("time/aqua/2019/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
minute_terra <- list.files("time/terra/2019/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

for( i in 1:length(doy_cos)) {
  doycos_rf <- raster(doy_cos[i])
  radaqua_rf <- raster(rad_aqua[i])
  lagradaqua_rf <- raster(lagrad_aqua[i])
  dem_rf <- dem
  minuteaqua_rf <- raster(minute_aqua[i])
  year_rf <- year
  lat_rf <- lat
  lon_rf <- lon
  stk <- stack(doycos_rf, radaqua_rf, lagradaqua_rf, dem_rf, minuteaqua_rf,
       year_rf, lat_rf, lon_rf)
  writeRaster(stk, filename=file.path(paste0("validation_stacks/aqua/predstack_", substr(doy_cos[i], 14, 21), ".tif")),
              format="GTiff", overwrite=TRUE)
}

for( i in 1:length(doy_cos)) {
  doycos_rf <- raster(doy_cos[i])
  radterra_rf <- raster(rad_terra[i])
  lagradterra_rf <- raster(lagrad_terra[i])
  dem_rf <- dem
  minuteterra_rf <- raster(minute_terra[i])
  year_rf <- year
  lat_rf <- lat
  lon_rf <- lon
  stk <- stack(doycos_rf, radterra_rf, lagradterra_rf, dem_rf, minuteterra_rf,
               year_rf, lat_rf, lon_rf)
  writeRaster(stk, filename=file.path(paste0("validation_stacks/terra/predstack_", substr(doy_cos[i], 14, 21), ".tif")),
              format="GTiff", overwrite=TRUE)
}
