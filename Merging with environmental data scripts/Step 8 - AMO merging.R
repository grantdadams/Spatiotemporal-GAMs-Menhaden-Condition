

amo_data <- read.csv("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\Atlantic multi-decadal oscillation.csv")
library(reshape)
amo_data <- melt(amo_data, id = "Year")
amo_data[,2] <- as.numeric(gsub( "X","" , amo_data[,2]))
colnames(amo_data)[which(colnames(amo_data)=="variable")] <- "month"
colnames(amo_data)[which(colnames(amo_data)=="value")] <- "amo"
colnames(amo_data)[which(colnames(amo_data)=="Year")] <- "year"




####
#### Add Multivariate ENSO Index data
####

amo_data <- amo_data[with(amo_data, order(amo_data$month, amo_data$year)), ]

library(dplyr)

amo_data= 
  amo_data %>%
  mutate(amo.lag.1=lag(amo,1)) %>%
  mutate(amo.lag.2=lag(amo,2)) %>%
  mutate(amo.lag.3=lag(amo,3)) %>%
  mutate(amo.lag.4=lag(amo,4)) %>%
  mutate(amo.lag.5=lag(amo,5)) %>%
  mutate(amo.lag.6=lag(amo,6)) %>%
  mutate(amo.lag.7=lag(amo,7)) %>%
  mutate(amo.lag.8=lag(amo,8)) %>%
  mutate(amo.lag.9=lag(amo,9)) %>%
  mutate(amo.lag.10=lag(amo,10)) %>%
  mutate(amo.lag.11=lag(amo,11)) %>%
  mutate(amo.lag.12=lag(amo,12))


######
# nao
######

nao_data <- read.csv("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\Monthly_mean_north_atlantic_oscillation_index.csv")
library(reshape)
nao_data <- melt(nao_data, id = "Year")
nao_data[,2] <- as.numeric(gsub( "X","" , nao_data[,2]))
colnames(nao_data)[which(colnames(nao_data)=="variable")] <- "month"
colnames(nao_data)[which(colnames(nao_data)=="value")] <- "nao"
colnames(nao_data)[which(colnames(nao_data)=="Year")] <- "year"

nao_data <- nao_data[with(nao_data, order(nao_data$month, nao_data$year)), ]

library(dplyr)

nao_data= 
  nao_data %>%
  mutate(nao.lag.1=lag(nao,1)) %>%
  mutate(nao.lag.2=lag(nao,2)) %>%
  mutate(nao.lag.3=lag(nao,3)) %>%
  mutate(nao.lag.4=lag(nao,4)) %>%
  mutate(nao.lag.5=lag(nao,5)) %>%
  mutate(nao.lag.6=lag(nao,6)) %>%
  mutate(nao.lag.7=lag(nao,7)) %>%
  mutate(nao.lag.8=lag(nao,8)) %>%
  mutate(nao.lag.9=lag(nao,9)) %>%
  mutate(nao.lag.10=lag(nao,10)) %>%
  mutate(nao.lag.11=lag(nao,11)) %>%
  mutate(nao.lag.12=lag(nao,12))




df.menhaden <- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 7")

df.menhaden= merge(df.menhaden, amo_data, by=c("month","year"))
df.menhaden= merge(df.menhaden, nao_data, by=c("month","year"))

write.csv(df.menhaden, file = "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")



















mei_data=read.csv("C:/Users/w966213/Documents/NGOM Oceanographic Data/Monthly Mei.csv")
mei_data$month_date=as.Date(mei_data$month_date,"%m/%d/%Y")
mei_data<- subset(mei_data, select= - c(3:159))

climate.dat <- merge(mei_data, amo_data , by = "month_date")

cor(climate.dat[,2], climate.dat[,3])
