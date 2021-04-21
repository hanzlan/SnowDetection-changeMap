setwd("C:/Users/andre/OneDrive/Dokumente/Masterarbeit/Data/dataframes/")
df <- read.csv("df_with_topo.csv")
boxplot(df$lst)
boxplot(df$wr)
summary(df$wr)

#get outlier values
outliers_lst <- boxplot(df$lst, plot=FALSE)$out
outliers_wr <- boxplot(df$wr, plot=FALSE)$out

max(outliers_lst[outliers_lst<0]) #-42.01
min(outliers_lst[outliers_lst>0]) #54.35

#replace negative outliers in lst and the according values in wr by NA
df$lst[df$lst>=min(outliers_lst[outliers_lst>0])] <- NA
df$lst[df$lst<=max(outliers_lst[outliers_lst<0])] <- NA

max(outliers_wr[outliers_wr<0]) #-7.48
min(outliers_wr[outliers_wr>0]) #32.22

#replace positive outliers in lst and the according values in wr by NA
df$wr[df$wr<=max(outliers_wr[outliers_wr<0])] <- NA
df$wr[df$wr>=min(outliers_wr[outliers_wr>0])] <- NA

boxplot(df$lst)
boxplot(df$wr)

write.csv(df,"df_no_outliers.csv", row.names = FALSE)
