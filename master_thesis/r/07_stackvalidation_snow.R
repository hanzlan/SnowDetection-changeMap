library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

dem <-  raster("dem/dem_kazbegi_clip.tif")
lat <-  raster("dem/lat.tif")
year <- raster("time/year2020.tif")

doy_cos <- list.files("doy_cos", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
rad_aqua <- list.files("rad/", pattern = "^2020.*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
rad_terra <- list.files("rad/", pattern = "^2020.*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)
lagrad_aqua <- list.files("rad/", pattern = "^(20191231|2020).*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
lagrad_aqua <- head(lagrad_aqua,-1)
lagrad_terra <- list.files("rad/", pattern = "^(20191231|2020).*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)
lagrad_terra <- head(lagrad_terra,-1)
minute_aqua <- list.files("time/aqua/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
minute_terra <- list.files("time/terra/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)
lst_aqua <- list.files("lst_modelled/", pattern = "^lst_modelled_2020.*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
lst_terra <- list.files("lst_modelled/", pattern = "^lst_modelled_2020.*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)
laglst_aqua <- list.files("lst_modelled/", pattern = "^lst_modelled_(20191231|2020).*aqua\\.tif$", full.names = TRUE, ignore.case = TRUE)
laglst_aqua <- head(laglst_aqua,-1)
laglst_terra <- list.files("lst_modelled/", pattern = "^lst_modelled_(20191231|2020).*terra\\.tif$", full.names = TRUE, ignore.case = TRUE)
laglst_terra <- head(laglst_terra,-1)

for( i in 1:length(doy_cos)) {
  laglstaqua_rf <- raster(laglst_aqua[i])
  lstaqua_rf <- raster(lst_aqua[i])
  doycos_rf <- raster(doy_cos[i])
  laglstaqua_rf <- crop(extend(laglstaqua_rf, doycos_rf), doycos_rf)
  lstaqua_rf <- crop(extend(lstaqua_rf, doycos_rf), doycos_rf)
  radaqua_rf <- raster(rad_aqua[i])
  lagradaqua_rf <- raster(lagrad_aqua[i])
  dem_rf <- dem
  lat_rf <- lat
  minuteaqua_rf <- raster(minute_aqua[i])
  year_rf <- year
  stk <- stack(laglstaqua_rf, lstaqua_rf, doycos_rf, radaqua_rf, lagradaqua_rf, dem_rf, 
               lat_rf, minuteaqua_rf, year_rf)
  writeRaster(stk, filename=file.path(paste0("validation_stacks/snow_aqua/predstack_snow_aqua_", substr(doy_cos[i], 9, 16), ".tif")),
              format="GTiff", overwrite=TRUE)
}

for( i in 1:length(doy_cos)) {
  laglstterra_rf <- raster(laglst_terra[i])
  lstterra_rf <- raster(lst_terra[i])
  doycos_rf <- raster(doy_cos[i])
  laglstterra_rf <- crop(extend(laglstterra_rf, doycos_rf), doycos_rf)
  lstterra_rf <- crop(extend(lstterra_rf, doycos_rf), doycos_rf)
  radterra_rf <- raster(rad_terra[i])
  lagradterra_rf <- raster(lagrad_terra[i])
  dem_rf <- dem
  lat_rf <- lat
  minuteterra_rf <- raster(minute_terra[i])
  year_rf <- year
  stk <- stack(laglstterra_rf, lstterra_rf, doycos_rf, radterra_rf, lagradterra_rf, dem_rf, 
               lat_rf, minuteterra_rf, year_rf)
  writeRaster(stk, filename=file.path(paste0("validation_stacks/snow_terra/predstack_snow_terra_", substr(doy_cos[i], 9, 16), ".tif")),
              format="GTiff", overwrite=TRUE)
}
