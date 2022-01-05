data.frame.menhaden<- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 4.csv")
data.frame.menhaden$date=as.Date(data.frame.menhaden$date)

####
#### Add Multivariate ENSO Index data
####

library(dplyr)
mei_data=read.csv("C:/Users/w966213/Documents/NGOM Oceanographic Data/Monthly Mei.csv")
mei_data$month_date=format(as.Date(mei_data$month_date,"%m/%d/%Y"))
mei_data<- subset(mei_data, select= - c(3:159))
mei_data= 
  mei_data %>%
  mutate(mei.lag.1=lag(mei,1)) %>%
  mutate(mei.lag.2=lag(mei,2)) %>%
  mutate(mei.lag.3=lag(mei,3)) %>%
  mutate(mei.lag.4=lag(mei,4)) %>%
  mutate(mei.lag.5=lag(mei,5)) %>%
  mutate(mei.lag.6=lag(mei,6)) %>%
  mutate(mei.lag.7=lag(mei,7)) %>%
  mutate(mei.lag.8=lag(mei,8)) %>%
  mutate(mei.lag.9=lag(mei,9)) %>%
  mutate(mei.lag.10=lag(mei,10)) %>%
  mutate(mei.lag.11=lag(mei,11)) %>%
  mutate(mei.lag.12=lag(mei,12))

mei_data$month_date<-as.Date(mei_data$month_date)
mei_data$month <- as.numeric(format(mei_data$month_date, format = "%m"))
mei_data$year <- as.numeric(format(mei_data$month_date, format = "%Y"))

data.frame.menhaden= merge(data.frame.menhaden, mei_data, by=c("month","year"))

write.csv(data.frame.menhaden, file = "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 5.csv")




