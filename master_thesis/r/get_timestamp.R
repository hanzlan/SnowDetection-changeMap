library(dplyr)
library(lubridate)
library(zoo)

setwd('C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/time/')
hours = read.csv("time_lst_aqua_total.csv")
hours$.geo <- NULL
colnames(hours)
hours <- hours %>% select(1, 4)
hours$date <- substring(hours$system.index, 0, 10)
hours$system.index <- NULL

#rename columns
hours <- hours %>% 
  rename(
    date = date,
    solar = mean
  )

#interpolate missing solar times
hours$solar <- na.approx(hours$solar)

#calculate utc decimal hours (44,54574E = mean longitude of kazbegi)
hours$utc <- hours$solar-44.54475/15
hours$posix <- as.POSIXct(hours$date, format="%Y_%m_%d", tz="UTC")
hours$posix <- hours$posix+(3600*hours$utc)
hours$numposix <- as.numeric(hours$posix)

write.csv(hours,"hours_lst_aqua.csv", row.names = FALSE)
