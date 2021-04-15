library(dplyr)
library(lubridate)

setwd('C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/')
hours = read.csv("time_lst_aqua.csv")
colnames(hours)
mean <- as.data.frame(hours$mean)
time <- as.data.frame(hours$system.index)
time <- substring(time$`hours$system.index`, 0, 10)
time <- as.data.frame(time)
hours <- cbind(time, mean)

#rename columns
hours <- hours %>% 
  rename(
    date = time,
    solar = `hours$mean`
  )

#calculate utc decimal hours (44,54574E = mean longitude of kazbegi)
hours$utc <- hours$solar-44.54475/15
hours$posix <- as.POSIXct(hours$date, format="%Y_%m_%d", tz="UTC")
hours$posix <- hours$posix+(3600*hours$utc)
hours$numposix <- as.numeric(hours$posix)

write.csv(hours,"hours_lst_aqua.csv", row.names = FALSE)
