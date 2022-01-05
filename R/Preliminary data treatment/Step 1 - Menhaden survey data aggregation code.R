
#####
##### Combine Data Sheets into one Data Frame
#####


data_dir <- "Data\\Raw NMFS Menhaden Surveys 1964-2011\\"


# Older and newer data frames have different column names
data.frame.menhaden.01 <- c()
for (j in 1:25) {
  data.frame.menhaden.01 <- rbind(data.frame.menhaden.01, read.csv(paste0(data_dir, dir(data_dir)[j])))
}

data.frame.menhaden.02 <- c()
for (j in 26:length(dir(data_dir))) {
  data.frame.menhaden.02 <- rbind(data.frame.menhaden.02, read.csv(paste0(data_dir, dir(data_dir)[j])))
}


# Adjust column names
names(data.frame.menhaden.02)=tolower(names(data.frame.menhaden.02))
colnames(data.frame.menhaden.02)[14]="collection"
colnames(data.frame.menhaden.02)[16:25]=colnames(data.frame.menhaden.01)[16:25]
colnames(data.frame.menhaden.02)[3]="scale_no"
colnames(data.frame.menhaden.02)[5]="fork_len"


# Have to change the location of data.frame.menhaden.01 to be an integer
data.frame.menhaden.01$location=format(as.character(data.frame.menhaden.01$location))
data.frame.menhaden.01<- data.frame.menhaden.01[,-11]
data.frame.menhaden.02<- data.frame.menhaden.02[,c(8,1,9,10,11,12,13,2,14,7,15,3,5,6,16:26)]

# Merge
data.frame.menhaden=rbind(data.frame.menhaden.01,data.frame.menhaden.02)

# Adjust years
data.frame.menhaden$year= ifelse(data.frame.menhaden$year<20,data.frame.menhaden$year+2000,data.frame.menhaden$year+1900)

# Format dates
data.frame.menhaden$date=as.Date(paste(data.frame.menhaden$day, data.frame.menhaden$month,data.frame.menhaden$year,sep="."),format="%d.%m.%Y")

# Write
# write.csv(data.frame.menhaden,file="Data\\NMFS Menhaden survey data set 1964-2011.csv")











