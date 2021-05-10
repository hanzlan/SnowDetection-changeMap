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

predictors <- c("rad",
                "lag.rad","prec", "lag.prec", "lon", "lat",
                "dem", "eastness", "northness", "slope",
                "year", "minute", "doy_cos")

set.seed(10)
indices <- CreateSpacetimeFolds(df,spacevar = "variable",
                                timevar = "doy",
                                k=3)

ffsmodel_LLTO <- ffs(df[,predictors],df$lst,metric="Rsquared",
                    method="rf", tuneGrid=data.frame("mtry"=2),
                    verbose=FALSE,ntree=50,
                    trControl=trainControl(method="cv",
                                           index = indices$index))
ffsmodel_LLTO

ffsmodel_LLTO$selectedvars

saveRDS(ffsmodel_LLTO, "./lst_models/ffsmodel_llto.rds")

plot_ffs(ffsmodel_LLTO)
plot(varImp(ffsmodel_LLTO))

stacklist <- list.files("validation_stacks/terra/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

###run over stacklist and predict lst
for( i in 1:length(stacklist)) {
  date <- substr(stacklist[i], 35, 42)
  predictors_sp <- stack(stacklist[i])
  names(predictors_sp) <- c('doy_cos', 'rad', 'lag.rad', 'dem',
                            'minute', 'year', 'lat', 'lon')
  prediction_ffs <- predict(predictors_sp,ffsmodel_LLTO)
  writeRaster(prediction_ffs, 
              filename=file.path(paste0("validation_stacks/pred_lst_terra/lst_", date, ".tif")),
              format="GTiff", overwrite=TRUE)
  }

stacklist_aqua <- list.files("validation_stacks/aqua/", pattern = ".tif$", full.names = TRUE, ignore.case = TRUE)

###run over stacklist and predict lst
for( i in 1:length(stacklist_aqua)) {
  date <- substr(stacklist_aqua[i], 34, 41)
  predictors_sp <- stack(stacklist_aqua[i])
  names(predictors_sp) <- c('doy_cos', 'rad', 'lag.rad', 'dem',
                            'minute', 'year', 'lat', 'lon')
  prediction_ffs <- predict(predictors_sp,ffsmodel_LLTO)
  writeRaster(prediction_ffs, 
              filename=file.path(paste0("validation_stacks/pred_lst_aqua/lst_", date, ".tif")),
              format="GTiff", overwrite=TRUE)
}

spplot(prediction_ffs)

### AOA for which the spatial CV error applies:
AOA <- aoa(predictors_sp,ffsmodel_LLTO)

spplot(prediction_ffs,main="prediction for the AOA \n(spatial CV error applied)")+
  spplot(AOA$AOA,col.regions=c("grey","transparent"))

### AOA for which the random CV error applies:
AOA_random <- aoa(predictors_sp,model)
spplot(prediction,main="prediction for the AOA \n(random CV error applied)")+
  spplot(AOA_random$AOA,col.regions=c("grey","transparent"))

