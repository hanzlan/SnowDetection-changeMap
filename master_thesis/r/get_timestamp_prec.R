library(dplyr)
library(lubridate)
library(zoo)

# ### load terra hours
# setwd('C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/time/')
# hours_terra = read.csv("time_lst_terra_total.csv")
# hours_terra$.geo <- NULL
# write.csv(hours_terra,"hours_lst_terra_withoutgeo.csv", row.names = FALSE)
hours_terra = read.csv("hours_lst_terra_withoutgeo.csv")
colnames(hours_terra)
hours_terra <- hours_terra %>% select(1, 4)
hours_terra$date <- substring(hours_terra$system.index, 0, 10)
hours_terra$system.index <- NULL

#rename columns
colnames(hours_terra) <- c("solar", "date")

# ### load aqua hours
# setwd('C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/time/')
# hours_aqua = read.csv("time_lst_aqua_total.csv")
# hours_aqua$.geo <- NULL
# write.csv(hours_aqua,"hours_lst_aqua_withoutgeo.csv", row.names = FALSE)
hours_aqua = read.csv("hours_lst_aqua_withoutgeo.csv")
colnames(hours_aqua)
hours_aqua <- hours_aqua %>% select(1, 4)
hours_aqua$date <- substring(hours_aqua$system.index, 0, 10)
hours_aqua$system.index <- NULL

#rename columns
colnames(hours_aqua) <- c("solar", "date")

#merge dfs
hours <- merge(x = hours_terra, y = hours_aqua, by = "date", all = TRUE)
hours$solar <- coalesce(hours$solar.x, hours$solar.y)
hours[2:3] <- NULL 

#interpolate missing solar times
hours$solar <- na.approx(hours$solar)
hours$date <- paste0(substr(hours$date, 1, 4), substr(hours$date, 6, 7), substr(hours$date, 9, 10))

#calculate missing dates
date_sequence <- as.character(seq(as.Date("2000/02/26"), as.Date("2021/04/15"), "days"))
date_sequence <- as.data.frame(paste0(substr(date_sequence, 1, 4), substr(date_sequence, 6, 7), substr(date_sequence, 9, 10)))
colnames(date_sequence) <- "date"
date_sequence[,'solar'] <- NA #add empty column

#add missing dates
date_missing <- dplyr::anti_join(date_sequence, hours, by = "date")
hours <- rbind(hours, date_missing)
hours <- hours[with(hours, order(hours$date)), ]
hours$solar <- na.approx(hours$solar)

#calculate utc decimal hours (44,54574E = mean longitude of kazbegi)
hours$utc <- hours$solar-44.54475/15
hours$posix <- as.POSIXct(hours$date, format="%Y%m%d", tz="UTC")
hours$posix <- hours$posix+(3600*hours$utc)
hours$numposix <- as.numeric(hours$posix)

write.csv(hours,"hours_lst_prec.csv", row.names = FALSE)
