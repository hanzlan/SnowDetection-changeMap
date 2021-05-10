library(CAST)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)
library(caret)
library(raster)
library(latticeExtra)
library(doParallel)

## Parallel Computing
# cores <- 3
# cl <- makeCluster(cores)
# registerDoParallel(cores)
# getDoParWorkers()

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
# mapviewOptions(basemaps = c("Esri.WorldImagery"))
# mapview(data_sp)

df$days <- as.Date(df$days)
df$date <- as.POSIXct(df$date, format= "%Y-%m-%d %H:%M:%S")
df <- df[, !names(df) %in% c("lag.snow", "lag.lst", "lag.rad", "lag.wr", "prec", "eastness", "northness",
                             "month", "year", "doy_cos_plus_209", "aspect_plus_176")]
cols.num <- c("doy","snow", "lst", "rad", "wr", "lag.prec", "lon", "lat", "dem",
              "slope", "minute", "doy_cos_plus_42", "aspect_plus_338",
              "date_num")
cols.fac <- c("variable", "sat")
df[cols.num] <- sapply(df[cols.num],as.numeric)
df[cols.fac] <- sapply(df[cols.fac],factor)

predictors <- c("lst", "rad", "wr", "lag.prec", "lon", "lat", "dem",
                "slope", "minute", "doy_cos_plus_42", "aspect_plus_338",
                "date_num")

# #visualize dataset:
# ggplot(data = df, aes(x=doy_cos_plus_42, y=snow)) + geom_point(alpha=0.1)

set.seed(10)

###Normal model
# model <- train(df[,predictors],df$lst,
#                method="rf",
#                tuneGrid=data.frame("mtry"=c(2,3,4)),
#                importance=TRUE,ntree=250,
#                trControl=trainControl(method="cv",number=3),
#                do.trace=100)

indices <- CreateSpacetimeFolds(df,spacevar = "variable",
                                timevar = "doy")

ffsmodel_LLTO <- ffs(df[,predictors],df$snow,
                     metric="Rsquared",
                     method="rf", 
                     minVar=3,
                     tuneGrid=data.frame("mtry"=c(2,3,4)),
                     verbose=FALSE, ntree=50,
                     trControl=trainControl(method="cv",
                                            index = indices$index),
                     do.trace=50)
# stopCluster(cl)
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

