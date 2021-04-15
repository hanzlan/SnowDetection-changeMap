library(raster)
library(rgdal)
library(rgrass7)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Region_of_Interest/")
kazbegi = readOGR(dsn = "kazbegi", layer = "kazbegi")
glaciers = readOGR(dsn = "glaciers", layer = "glaciers")

kazbegi_wo_glaciers = kazbegi - glaciers
plot(kazbegi_wo_glaciers)

writeOGR(kazbegi_wo_glaciers, ".", "kazbegi_wo_glaciers", 
         driver = "ESRI Shapefile")

#get center coordinates of shapefile
print(coordinates(kazbegi))
