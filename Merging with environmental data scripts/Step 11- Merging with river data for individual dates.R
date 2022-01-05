df.menhaden<-read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")

# Add monthly index of MS discharge for each observation
load("C:/Users/w966213/Documents/NGOM Oceanographic Data/MISS.ATCH.River.Flow.RData")
colnames(MISS.ATCH.river.data)[2:6]=c("day","month","year","daily.atch.discharge","daily.miss.discharge")
MISS.ATCH.river.data$date <- as.Date(paste(MISS.ATCH.river.data$day, MISS.ATCH.river.data$month, MISS.ATCH.river.data$year, sep = "/"), format = "%d/%m/%Y" )

df.menhaden$date <- as.Date(df.menhaden$date)

library(lubridate)


for(i in 1:nrow(df.menhaden)){
  df.menhaden$individual.monthly.miss[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= df.menhaden$date[i] &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-30))], na.rm = T)
  df.menhaden$individual.monthly.miss.lag1[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (df.menhaden$date[i]-30) &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-60))], na.rm = T)
  df.menhaden$individual.monthly.miss.lag2[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (df.menhaden$date[i]-60) &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-90))], na.rm = T)
  df.menhaden$individual.monthly.miss.lag3[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (df.menhaden$date[i]-90) &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-120))], na.rm = T)
  df.menhaden$individual.monthly.miss.lag4[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (df.menhaden$date[i]-120) &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-150))], na.rm = T)
  df.menhaden$individual.monthly.miss.lag5[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (df.menhaden$date[i]-150) &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-180))], na.rm = T)
  df.menhaden$individual.monthly.miss.lag6[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (df.menhaden$date[i]-180) &   MISS.ATCH.river.data$date >= (df.menhaden$date[i]-210))], na.rm = T)
  print(i)
}

# Scale monthly.individual
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("individual.monthly.miss")) == "individual.monthly.miss")
for(i in ind.miss.cols){
  df.menhaden$new.column <- NA
  df.menhaden$new.column <- scale(df.menhaden[,i])
  colnames(df.menhaden)[which(colnames(df.menhaden)=="new.column")] <- paste(colnames(df.menhaden)[i],".scaled",sep="")
}

write.csv(df.menhaden, file = "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")