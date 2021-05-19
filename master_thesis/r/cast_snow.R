library(CAST)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)
library(caret)
library(raster)
library(latticeExtra)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

df <- read.csv("dataframes/df_train_small.csv")
head(df)
df$lon <- round(df$lon, digits=5)
df$lat <- round(df$lat, digits=5)
df$variable <- as.character(df$variable)
data_sp <- unique(df[,c("variable","lat","lon")])
rownames(data_sp) <- NULL
data_sp <- st_as_sf(data_sp,coords=c("lon","lat"),crs=4326)
plot(data_sp,axes=T,col="black")
mapviewOptions(basemaps = c("Esri.WorldImagery"))
mapview(data_sp)

df$days <- as.Date(df$days)
df$date <- as.POSIXct(df$date, format= "%Y-%m-%d %H:%M:%S")
cols.num <- c("doy","snow", "lag.snow", "month", "year", "minute")
cols.fac <- c("variable", "sat")
df[cols.num] <- sapply(df[cols.num],as.numeric)
df[cols.fac] <- sapply(df[cols.fac],factor)

predictors_snow <- c("sat","lst","lag.lst","rad",
                "lag.rad","wr","lag.wr",
                "prec", "lag.prec", "lon", "lat",
                "dem", "eastness", "northness", "slope",
                "year", "minute", "doy_cos")

set.seed(10)
indices <- CreateSpacetimeFolds(df,spacevar = "variable",
                                timevar = "doy",
                                k=3)

set.seed(10)
ffsmodel_LLTO <- ffs(df[,predictors_snow],df$snow,metric="Rsquared",
                    method="rf", tuneGrid=data.frame("mtry"=2),
                    verbose=FALSE,ntree=50,
                    trControl=trainControl(method="cv",
                                           index = indices$index))
ffsmodel_LLTO

ffsmodel_LLTO$selectedvars

saveRDS(ffsmodel_LLTO, "./snow_models/ffsmodel_llto.rds")

ffsmodel_LLTO <- readRDS("snow_models/ffsmodel_llto.rds")

plot_ffs(ffsmodel_LLTO)
plot(varImp(ffsmodel_LLTO))

stacklist <- list.files("validation_stacks/snow_terra/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

###run over stacklist and predict snow terra
for( i in 1:length(stacklist)) {
  date <- substr(stacklist[i], 51, 58)
  predictors_sp <- stack(stacklist[i])
  names(predictors_sp) <- c('lag.lst', 'lst', 'doy_cos', 'rad', 'lag.rad', 'dem',
                            'lat', 'minute', 'year')
  prediction_ffs <- predict(predictors_sp,ffsmodel_LLTO)
  writeRaster(prediction_ffs, 
              filename=file.path(paste0("validation_stacks/pred_snow_terra/snow_terra_", date, ".tif")),
              format="GTiff", overwrite=TRUE)
}

stacklist_aqua <- list.files("validation_stacks/snow_aqua/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

###run over stacklist and predict snow aqua
for( i in 1:length(stacklist_aqua)) {
  date <- substr(stacklist_aqua[i], 49, 56)
  predictors_sp <- stack(stacklist_aqua[i])
  names(predictors_sp) <- c('lag.lst', 'lst', 'doy_cos', 'rad', 'lag.rad', 'dem',
                            'lat', 'minute', 'year')
  prediction_ffs <- predict(predictors_sp,ffsmodel_LLTO)
  writeRaster(prediction_ffs, 
              filename=file.path(paste0("validation_stacks/pred_snow_aqua/snow_aqua_", date, ".tif")),
              format="GTiff", overwrite=TRUE)
}

prediction_ffs <- predict(predictors_sp,ffsmodel_LLTO)
spplot(prediction_ffs)

### AOA for which the spatial CV error applies:
AOA <- aoa(predictors_sp,ffsmodel_LLTO)

spplot(prediction_ffs,main="prediction for the AOA \n(spatial CV error applied)")+
  spplot(AOA$AOA,col.regions=c("grey","transparent"))

### AOA for which the random CV error applies:
AOA_random <- aoa(predictors_sp,model)
spplot(prediction,main="prediction for the AOA \n(random CV error applied)")+
  spplot(AOA_random$AOA,col.regions=c("grey","transparent"))

