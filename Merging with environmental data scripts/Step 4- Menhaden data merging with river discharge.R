# This function creates an index of monthly river discharge from summed daily river discharge values
# It also creates an index of average monthly spring river discharge from March to June


data.frame.men <- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 3.csv")
data.frame.men$date<- as.Date(data.frame.men$date, origin="1970-01-01")
head(data.frame.men[order(data.frame.men$date),])


# # 
# # Open the river data
load("C:/Users/w966213/Documents/NGOM Oceanographic Data/MISS.ATCH.River.Flow.RData")
colnames(MISS.ATCH.river.data)[2:6]=c("day","month","year","daily.atch.discharge","daily.miss.discharge")

# 
# Summed daily river discharge to create a monthly index for each month and year
attach(MISS.ATCH.river.data)
monthly.discharge.atch=aggregate(daily.atch.discharge, by=list(year,month), FUN= function(atch){ sum(atch, na.rm=TRUE)})
monthly.discharge.miss= aggregate(daily.miss.discharge, by=list(year,month), FUN= function(miss){ sum(miss, na.rm=TRUE)})
monthly.discharge.miss[which(monthly.discharge.miss$x ==0),]$x <- NA
colnames(monthly.discharge.atch)[3]="monthly.atch"
colnames(monthly.discharge.miss)[3]="monthly.miss"
monthly.discharge.miss$monthly.miss.scaled <- scale(monthly.discharge.miss[,3])[,1]
monthly.discharge=merge(monthly.discharge.miss,monthly.discharge.atch,by=c("Group.1","Group.2"))
colnames(monthly.discharge)[1:2]=c("year","month")
monthly.discharge$month=as.numeric(monthly.discharge$month)
monthly.discharge=monthly.discharge[order(monthly.discharge$year,monthly.discharge$month),]

#### Create a lagged index of monthly discharge

library(dplyr)
monthly.discharge= 
  monthly.discharge %>%
#   mutate(monthly.atch.lag1=lag(monthly.atch,1)) %>%
#   mutate(monthly.atch.lag2=lag(monthly.atch,2)) %>%
#   mutate(monthly.atch.lag3=lag(monthly.atch,3)) %>%
#   mutate(monthly.atch.lag4=lag(monthly.atch,4)) %>%
#   mutate(monthly.atch.lag5=lag(monthly.atch,5)) %>%
#   mutate(monthly.atch.lag6=lag(monthly.atch,6)) %>%
#   
   mutate(monthly.miss.lag1.scaled=lag(monthly.miss.scaled,1)) %>%
   mutate(monthly.miss.lag2.scaled=lag(monthly.miss.scaled,2)) %>%
   mutate(monthly.miss.lag3.scaled=lag(monthly.miss.scaled,3)) %>%
   mutate(monthly.miss.lag4.scaled=lag(monthly.miss.scaled,4)) %>%
   mutate(monthly.miss.lag5.scaled=lag(monthly.miss.scaled,5)) %>%
   mutate(monthly.miss.lag6.scaled=lag(monthly.miss.scaled,6))
# 
# 
# 
monthly.discharge[monthly.discharge==0]=NA



##### Create an Index for River Discharge from Nov to March
Nov.to.March.discharge=data.frame(year=NA, miss.discharge.Nov.to.March=NA, atch.discharge.Nov.to.March=NA)
for ( i in min(monthly.discharge$year) : max(monthly.discharge$year)){
  winter.discharge.sub.1<-subset(monthly.discharge, monthly.discharge$year==i & monthly.discharge$month %in% c(1:3))
  winter.discharge.sub.2<-subset(monthly.discharge, monthly.discharge$year==i-1 & monthly.discharge$month %in% c(11:12))
  winter.dischage<- rbind(winter.discharge.sub.1, winter.discharge.sub.2)
  Nov.to.March.discharge[i-min(monthly.discharge$year) ,"year"]=i
  Nov.to.March.discharge[i-min(monthly.discharge$year), "miss.discharge.Nov.to.March"]<- (sum(winter.dischage[,"monthly.miss"])/5)
  Nov.to.March.discharge[i-min(monthly.discharge$year), "atch.discharge.Nov.to.March"]<- (sum(winter.dischage[,"monthly.atch"])/5)
}

Nov.to.March.discharge$miss.discharge.Nov.to.March.scaled <- scale(Nov.to.March.discharge$miss.discharge.Nov.to.March)
Nov.to.March.discharge$atch.discharge.Nov.to.March.scaled <- scale(Nov.to.March.discharge$atch.discharge.Nov.to.March)
Nov.to.March.discharge=Nov.to.March.discharge[,c(1,4,5)]




#add river data from just March to May
spring.vect <- data.frame(mean.monthly.spring.discharge.of.year=c(rep(NA,times=3),rep("spring",times=3),rep(NA,times=3),rep(NA,times=3)),month=c(12,1:11))
monthly.discharge=merge(monthly.discharge,spring.vect,by="month")
monthly.discharge=monthly.discharge[order(monthly.discharge$year,monthly.discharge$month),]


attach(monthly.discharge)
spring.discharge.atch=aggregate(monthly.atch, by=list(year,mean.monthly.spring.discharge.of.year), FUN= function(monthly.atch){ sum(monthly.atch, na.rm=TRUE)/length(!is.na(monthly.atch))})
spring.discharge.miss=aggregate(monthly.miss, by=list(year,mean.monthly.spring.discharge.of.year), FUN= function(monthly.miss){ sum(monthly.miss, na.rm=TRUE)/length(!is.na(monthly.miss))})
colnames(spring.discharge.atch)[3]="mean.monthly.March.to.May.atch"
colnames(spring.discharge.miss)[3]="mean.monthly.March.to.May.miss"
spring.discharge=merge(spring.discharge.miss,spring.discharge.atch,by=c("Group.1","Group.2"))
colnames(spring.discharge)[1:2]=c("year","spring.discharge")
March.to.June.discharge=subset(spring.discharge, select= -spring.discharge)

March.to.June.discharge$mean.monthly.March.to.May.miss.scaled <- scale(March.to.June.discharge$mean.monthly.March.to.May.miss)
March.to.June.discharge$mean.monthly.March.to.May.atch.scaled <- scale(March.to.June.discharge$mean.monthly.March.to.May.atch)
March.to.May.discharge=March.to.June.discharge[,c(1,4,5)]

river.discharge<- merge(Nov.to.March.discharge, March.to.May.discharge,by=c("year"))
save( river.discharge, file = "winter_and_spring_discharge.RData")


##### Merge all the data sets
data.frame.men=merge(data.frame.men,MISS.ATCH.river.data, by=c("day","month","year"), all.x=T) #merging the daily data
data.frame.men=merge(data.frame.men,monthly.discharge,by=c("year","month"))
data.frame.men=merge(data.frame.men,March.to.May.discharge,by=c("year"))
data.frame.men<- merge(data.frame.men, Nov.to.March.discharge, by = c("year"))

write.csv(data.frame.men, file = "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 4.csv")
