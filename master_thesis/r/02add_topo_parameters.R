library(rgdal)
library(raster)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/")

###load points
sppts <- readOGR(dsn = "kazbegi", layer = "randompoints500")

###load tifs
eastness <-  raster("dem/eastness_kazbegi_clip.tif")
northness <-  raster("dem/northness_kazbegi_clip.tif")
slope <-  raster("dem/slope_kazbegi_clip.tif")

pts_df <- as.data.frame(sppts)

pts_df$eastness <- extract(eastness, sppts)
pts_df$northness <- extract(northness, sppts)
pts_df$slope <- extract(slope, sppts)

##assign variable "Vxxx"
variable <- 1:500
variable <- paste0("V", as.character(variable))

pts_df[4:5] <- list(NULL)
pts_df <- cbind(pts_df, variable)

df <- read.csv("dataframes/df_complete.csv")
df_new <- merge(df,pts_df,by="variable")

write.csv(df_new,"dataframes/df_with_topo.csv", row.names = FALSE)
