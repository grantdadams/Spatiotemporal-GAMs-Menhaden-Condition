
#####
##### Combine Data Sheets into one Data Frame
#####


setwd("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\Raw NMFS Menhaden Surveys 1964-2011\\")
getwd()


length.col.names <- c()
names.mat <- matrix(NA,length(dir()),26)
for (j in 1:length(dir())) {
  
  length.col.names[j] <- length(read.csv(dir()[j]))
  names.mat[j,1:26] <- names(read.csv(dir()[j]))
}

data.frame.men.01 <- c()
for (j in 1:25) {
  data.frame.men.01 <- rbind(data.frame.men.01, read.csv(dir()[j]))
}

data.frame.men.02 <- c()
for (j in 26:length(dir())) {
  data.frame.men.02 <- rbind(data.frame.men.02, read.csv(dir()[j]))
}


names(data.frame.men.02)=tolower(names(data.frame.men.02))


colnames(data.frame.men.02)[14]="collection"
colnames(data.frame.men.02)[16:25]=colnames(data.frame.men.01)[16:25]
colnames(data.frame.men.02)[3]="scale_no"
colnames(data.frame.men.02)[5]="fork_len"


#have to change the location of data.frame.men.01 to be an integer
data.frame.men.01$location=format(as.character(data.frame.men.01$location))

data.frame.men.01<- data.frame.men.01[,-11]
data.frame.men.02<- data.frame.men.02[,c(8,1,9,10,11,12,13,2,14,7,15,3,5,6,16:26)]
data.frame.men=rbind(data.frame.men.01,data.frame.men.02)


data.frame.men$year= ifelse(data.frame.men$year<20,data.frame.men$year+2000,data.frame.men$year+1900)

data.frame.men$date=as.Date(paste(data.frame.men$day, data.frame.men$month,data.frame.men$year,sep="."),format="%d.%m.%Y")


write.csv(data.frame.men,file="C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011.csv")











