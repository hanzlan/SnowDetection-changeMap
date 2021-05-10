library(ggplot2)

setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")
df <- read.csv("df_with_topo.csv")
boxplot(df$lst)
boxplot(df$wr)
summary(df$wr)


#get outlier values
outliers_lst <- boxplot(df$lst, plot=FALSE)$out
max(outliers_lst[outliers_lst<0]) #-42.07
min(outliers_lst[outliers_lst>0]) #54.43

#replace negative outliers in lst and the according values in wr by NA
df$lst[df$lst>=min(outliers_lst[outliers_lst>0])] <- NA
df$lst[df$lst<=max(outliers_lst[outliers_lst<0])] <- NA

outliers_wr <- boxplot(df$wr, plot=FALSE)$out
max(outliers_wr[outliers_wr<0]) #-7.48
min(outliers_wr[outliers_wr>0]) #32.2

#replace positive outliers in lst and the according values in wr by NA
df$wr[df$wr<=max(outliers_wr[outliers_wr<0])] <- NA
df$wr[df$wr>=min(outliers_wr[outliers_wr>0])] <- NA

#get outlier values
outliers_laglst <- boxplot(df$lag.lst, plot=FALSE)$out
max(outliers_laglst[outliers_laglst<0]) #-42.05
min(outliers_laglst[outliers_laglst>0]) #54.55

#replace negative outliers in lst and the according values in wr by NA
df$lag.lst[df$lag.lst>=min(outliers_laglst[outliers_laglst>0])] <- NA
df$lag.lst[df$lag.lst<=max(outliers_laglst[outliers_laglst<0])] <- NA

#get outlier values
outliers_lagwr <- boxplot(df$lag.wr, plot=FALSE)$out
max(outliers_lagwr[outliers_lagwr<0]) #-7.48
min(outliers_lagwr[outliers_lagwr>0]) #32.22

#replace negative outliers in lst and the according values in wr by NA
df$lag.wr[df$lag.wr>=min(outliers_lagwr[outliers_lagwr>0])] <- NA
df$lag.wr[df$lag.wr<=max(outliers_lagwr[outliers_lagwr<0])] <- NA


boxplot(df$lst)
boxplot(df$wr)

###remove rows where snow = NA
df_no_na <- df[!is.na(df$snow),]
df_no_na$snow <- as.numeric(df_no_na$snow)
df_no_na$lag.snow <- as.numeric(df_no_na$lag.snow)
df_no_na$days <- as.Date(df_no_na$days, format='%Y-%m-%d')
df_no_na$doy <- as.numeric(strftime(df_no_na$date, format = "%j"))
df_no_na$month <- as.numeric(strftime(df_no_na$date, format = "%m"))
df_no_na$year <- as.numeric(strftime(df_no_na$date, format = "%Y"))
df_no_na$minute <- as.numeric(substr(df_no_na$date, 12, 13))*60+as.numeric(substr(df_no_na$date, 15, 16))

####calculate the day of year with average least snow
mean_snow <- aggregate(df_no_na$snow, list(df_no_na$doy), mean)
which.min(mean_snow$x) #### 229 = 16th August in leap years (366 days)
names(mean_snow)[names(mean_snow) == 'Group.1'] <- 'doy'
mean_snow$new_doy <- mean_snow$doy

#set day of the year with least snow as first day
new_doy <- c(138:366, 1:137)
mean_snow$new_doy <- new_doy

#calculate circular radians to account for circular behaviour of day of the year
mean_snow$degrees <- (mean_snow$new_doy/366)*360
mean_snow$radians <- mean_snow$degrees*pi/180
mean_snow$doy_cos <- cos(mean_snow$radians)


df_with_doy <- merge(df_no_na, mean_snow[, c("doy", "doy_cos")], by="doy")

write.csv(df_with_doy,"df_nooutliers_nosnowna_doycos.csv", row.names = FALSE)


ggplot(data=df_no_na,
       aes(x=doy, y=snow)) +
  geom_line()
