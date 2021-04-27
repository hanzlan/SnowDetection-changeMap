library(data.table)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")

snow_aqua <- read.csv("snow_aqua_500.csv")
snow_terra <- read.csv("snow_terra_500.csv")

lst_aqua <- read.csv("lst_aqua_500.csv")
lst_terra <- read.csv("lst_terra_500.csv")

wr_aqua <- read.csv("wr_aqua_500.csv")
wr_terra <- read.csv("wr_terra_500.csv")

rad_aqua <- read.csv("rad_aqua_500.csv")
rad_terra <- read.csv("rad_terra_500.csv")

prec <- read.csv("prec_500.csv")


lagging <- function(data) {
  # return a new vector containing the value of the previous day
  # Example: center(c(1, 2, 3), 0) => c(-1, 0, 1)
  days <- as.data.frame(seq(as.Date("2000-02-26"), as.Date("2021-05-01"), "days"))
  names(days)[1] <- "days"
  
  data$days <- as.Date(paste0(substr(data$date, 1, 4), substr(data$date, 6, 7), substr(data$date, 9, 10)), format='%Y%m%d')
  
  df <- merge(x=days, y=data, by=c("days"), all=TRUE)
  valuename <- names(df)[names(df) %in% c('snow','lst','wr','prec', 'rad')]
  names(df)[names(df) %in% c('snow','lst','wr','prec', 'rad')] <- 'value'
  df <- df[order(df$variable,df$date),]
  df <- data.table(df)
  
  df_lagged <- df[, lag.value:=c(NA, value[-.N]), by=variable]
  names(df_lagged)[names(df_lagged) == 'value'] <- valuename
  names(df_lagged)[names(df_lagged) == 'lag.value'] <- paste0('lag.',valuename)
  return(df_lagged)
}

df_snow_aqua <- lagging(snow_aqua)
df_snow_terra <- lagging(snow_terra)
df_lst_aqua <- lagging(lst_aqua)
df_lst_terra <- lagging(lst_terra)
df_rad_aqua <- lagging(rad_aqua)
df_rad_terra <- lagging(rad_terra)
df_wr_aqua <- lagging(wr_aqua)
df_wr_terra <- lagging(wr_terra)
df_prec <- lagging(prec)

df_prec_terra <- df_prec
df_prec_terra$sat <- 'terra'

df_prec_aqua <- df_prec
df_prec_aqua$sat <- 'aqua'

snow <- rbind(df_snow_terra, df_snow_aqua)
lst <- rbind(df_lst_terra, df_lst_aqua)
rad <- rbind(df_rad_terra, df_rad_aqua)
wr <- rbind(df_wr_terra, df_wr_aqua)
prec_complete <- rbind(df_prec_terra, df_prec_aqua)
prec_complete <- prec_complete[!is.na(prec_complete$variable),]

df <- NULL
df <- merge(x=snow, y=lst[ , c("days", "variable", "sat", "lst", "lag.lst")], by=c("days","variable","sat"), all.x =TRUE)
df <- df[!is.na(df$variable),]
df <- merge(x=df, y=rad[ , c("days", "variable", "sat", "rad", "lag.rad")], by=c("days","variable","sat"), all.x =TRUE)
df <- merge(x=df, y=wr[ , c("days", "variable", "sat", "wr", "lag.wr")], by=c("days","variable","sat"), all.x=TRUE)
df <- merge(x=df, y=prec_complete[ , c("days", "variable", "sat", "prec", "lag.prec")], by=c("days","variable","sat"), all.x=TRUE)
df <- df[!is.na(df$variable),]

write.csv(df,"df_complete.csv", row.names = FALSE)
