setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")

snow_aqua <- read.csv("snow_aqua_500.csv")
snow_terra <- read.csv("snow_terra_500.csv")

lst_aqua <- read.csv("lst_aqua_500.csv")
lst_terra <- read.csv("lst_terra_500.csv")

wr_aqua <- read.csv("wr_aqua_500.csv")
wr_terra <- read.csv("wr_terra_500.csv")

prec <- read.csv("prec_500.csv")

snow <- rbind(snow_terra, snow_aqua)
lst <- rbind(lst_terra, lst_aqua)
wr <- rbind(wr_terra, wr_aqua)

df <- merge(x=snow, y=lst, by=c("date","variable","sat"), all=TRUE)
df <- merge(x=df, y=wr, by=c("date","variable","sat"), all=TRUE)
df <- merge(x=df, y=prec, by=c("date","variable"), all=TRUE)

write.csv(df,"df_complete.csv", row.names = FALSE)
