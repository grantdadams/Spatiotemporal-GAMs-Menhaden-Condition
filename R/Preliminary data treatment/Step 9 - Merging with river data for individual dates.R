# data.frame.menhaden<-read.csv("Data\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")

# Add monthly index of MS discharge for each observation
load("Data/Mississippi_Atchafalaya_river_flow.RData")
colnames(MISS.ATCH.river.data)[2:6]=c("day","month","year","daily.atch.discharge","daily.miss.discharge")
MISS.ATCH.river.data$date <- as.Date(paste(MISS.ATCH.river.data$day, MISS.ATCH.river.data$month, MISS.ATCH.river.data$year, sep = "/"), format = "%d/%m/%Y" )

data.frame.menhaden$date <- as.Date(data.frame.menhaden$date)

library(lubridate)


for(i in 1:nrow(data.frame.menhaden)){
  data.frame.menhaden$individual.monthly.miss[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= data.frame.menhaden$date[i] &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-30))], na.rm = T)
  data.frame.menhaden$individual.monthly.miss.lag1[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (data.frame.menhaden$date[i]-30) &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-60))], na.rm = T)
  data.frame.menhaden$individual.monthly.miss.lag2[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (data.frame.menhaden$date[i]-60) &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-90))], na.rm = T)
  data.frame.menhaden$individual.monthly.miss.lag3[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (data.frame.menhaden$date[i]-90) &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-120))], na.rm = T)
  data.frame.menhaden$individual.monthly.miss.lag4[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (data.frame.menhaden$date[i]-120) &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-150))], na.rm = T)
  data.frame.menhaden$individual.monthly.miss.lag5[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (data.frame.menhaden$date[i]-150) &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-180))], na.rm = T)
  data.frame.menhaden$individual.monthly.miss.lag6[i] <-  sum(MISS.ATCH.river.data$daily.miss.discharge[which(MISS.ATCH.river.data$date <= (data.frame.menhaden$date[i]-180) &   MISS.ATCH.river.data$date >= (data.frame.menhaden$date[i]-210))], na.rm = T)
  print(i)
}

# Scale monthly.individual
ind.miss.cols <- which(substr(colnames(data.frame.menhaden), start = 0, stop = nchar("individual.monthly.miss")) == "individual.monthly.miss")
for(i in ind.miss.cols){
  data.frame.menhaden$new.column <- NA
  data.frame.menhaden$new.column <- scale(data.frame.menhaden[,i])
  colnames(data.frame.menhaden)[which(colnames(data.frame.menhaden)=="new.column")] <- paste(colnames(data.frame.menhaden)[i],".scaled",sep="")
}

# write.csv(data.frame.menhaden, file = "Data\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")