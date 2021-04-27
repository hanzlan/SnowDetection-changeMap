library(CAST)
library(sf)
library(mapview)
library(lubridate)
library(ggplot2)
library(caret)
library(raster)
library(latticeExtra)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

data <- read.csv("dataframes/df_no_outliers.csv")
head(data)
data_sp <- unique(data[,c("variable","lon","lat")])
data_sp <- st_as_sf(data_sp,coords=c("lon","lat"),crs=4326)
plot(data_sp,axes=T,col="black")
mapviewOptions(basemaps = c("Esri.WorldImagery"))
mapview(data_sp)

trainDat <- data[data$altitude==-0.3&
                   year(data$Date)==2012&
                   week(data$Date)%in%c(10:12),]
ggplot(data = trainDat, aes(x=Date, y=VW)) +
  geom_line(aes(colour=SOURCEID))

predictors <- c("DEM","TWI","Precip_cum","cday",
                "MaxT_wrcc","Precip_wrcc","BLD",
                "Northing","Easting","NDRE.M")
set.seed(10)
model <- train(trainDat[,predictors],trainDat$VW,
               method="rf",tuneGrid=data.frame("mtry"=2),
               importance=TRUE,ntree=50,
               trControl=trainControl(method="cv",number=3))

predictors_sp <- stack(system.file("extdata","predictors_2012-03-25.grd",package="CAST"))
prediction <- predict(predictors_sp,model)
spplot(prediction)

model

set.seed(10)
indices <- CreateSpacetimeFolds(trainDat,spacevar = "SOURCEID",
                                k=3)
set.seed(10)
model_LLO <- train(trainDat[,predictors],trainDat$VW,
                   method="rf",tuneGrid=data.frame("mtry"=2), importance=TRUE,
                   trControl=trainControl(method="cv",
                                          index = indices$index))
model_LLO

plot(varImp(model_LLO))

set.seed(10)
ffsmodel_LLO <- ffs(trainDat[,predictors],trainDat$VW,metric="Rsquared",
                    method="rf", tuneGrid=data.frame("mtry"=2),
                    verbose=FALSE,ntree=50,
                    trControl=trainControl(method="cv",
                                           index = indices$index))
ffsmodel_LLO

ffsmodel_LLO$selectedvars

plot_ffs(ffsmodel_LLO)

prediction_ffs <- predict(predictors_sp,ffsmodel_LLO)
spplot(prediction_ffs)

### AOA for which the spatial CV error applies:
AOA <- aoa(predictors_sp,ffsmodel_LLO)

spplot(prediction_ffs,main="prediction for the AOA \n(spatial CV error applied)")+
  spplot(AOA$AOA,col.regions=c("grey","transparent"))

### AOA for which the random CV error applies:
AOA_random <- aoa(predictors_sp,model)
spplot(prediction,main="prediction for the AOA \n(random CV error applied)")+
  spplot(AOA_random$AOA,col.regions=c("grey","transparent"))

