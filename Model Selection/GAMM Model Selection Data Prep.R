df.menhaden<-read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")

rm.vec <- c()

# Remove below length at maturity
percent.mature <- .99
Length.at.maturity <- (log((1/percent.mature) - 1)/-0.1300797)+126.6695276
Length.at.maturity
1/(1+exp(-0.1300797*(Length.at.maturity-126.6695276)))

df.menhaden$week <- as.numeric(format(as.POSIXct(df.menhaden$date), "%U"))
rm.vec <- c(rm.vec, length(which(is.na(df.menhaden$month))))
df.menhaden <- df.menhaden[which(!is.na(df.menhaden$month)),]


rm.vec <- c(rm.vec, length(which(df.menhaden$fork_len < Length.at.maturity)))
df.menhaden <- df.menhaden[which(df.menhaden$fork_len > Length.at.maturity),]

rm.vec <- c(rm.vec, length(which(df.menhaden$fork_len==0)))
df.menhaden <- df.menhaden[which(df.menhaden$fork_len!=0 ),]

rm.vec <- c(rm.vec, length(which(is.na(df.menhaden$fork_len))))
df.menhaden <- df.menhaden[which(!is.na(df.menhaden$fork_len)),]

rm.vec <- c(rm.vec, length(which(df.menhaden$longitude > -88)))



# Remove Mississippi Sound
df.menhaden <- df.menhaden[which(df.menhaden$longitude < -88),]
df.menhaden <- df.menhaden[which(df.menhaden$latitude < 30),]


# Get rid of lake ponchairtrain
rm.vec <- c(rm.vec, length(which(df.menhaden$longitude < -89.67 & df.menhaden$latitude >  29.666)))
df.menhaden <- df.menhaden[-which(df.menhaden$longitude < -89.67 & df.menhaden$latitude >  29.666),]


#####
####Remove data outside the 99.99% Prediction Interval of the log length at weight relationship for each year
#####
data.frame.men.pi=data.frame(matrix(ncol=c(ncol(df.menhaden)+3)))
par(mfrow=c(2,2))
for (j in c(1964:2011)) {
  x=subset(df.menhaden, df.menhaden$year==j)
  if(nrow(x)>0){
    LW.dat <- cbind(log(x$fork_len),log(x$weight))
    weight.length.fit <- lm(LW.dat[,2] ~ LW.dat[,1])
    #plot(log(x$fork_len), log(x$weight), main=j )
    #abline(weight.length.fit, col=2)
    weight.length.fit.sum <- summary(weight.length.fit)
    weight.length.fit.sum
#
    new<-c(length.data=log(x$fork_len))
    weight.length.fit.predict<-predict(weight.length.fit, data.frame(new), interval = "prediction", level=.99999)
    #lines(new, weight.length.fit.predict[,2], col=2 )
    #lines(new, weight.length.fit.predict[,3], col=2 )
#
    data.frame.men.pi.added<-cbind(weight.length.fit.predict,x)
    colnames(data.frame.men.pi)<- colnames(data.frame.men.pi.added)
    data.frame.men.pi<- rbind( data.frame.men.pi, data.frame.men.pi.added)
#
  }
}
#
data.frame.men.pi<-data.frame.men.pi[-1,]
head(data.frame.men.pi)
data.frame.men.pi$remove<- ifelse( log(data.frame.men.pi$weight)>data.frame.men.pi$upr , "spurious", ifelse(log(data.frame.men.pi$weight)<data.frame.men.pi$lwr, "spurious", "not_spurious") )

df.menhaden<- subset(data.frame.men.pi, data.frame.men.pi$remove=="not_spurious")
spurious<- subset(subset(data.frame.men.pi, data.frame.men.pi$remove=="spurious"))
rm.vec <- c(rm.vec, nrow(spurious))


# Get unique trip
library(dplyr)
check <- df.menhaden %>% group_by(month,vessel,year) %>% count()
check <- as.data.frame(check)
check <- check[,-6]
check$collection <- paste("trip",1:nrow(check),sep=".")
df.menhaden <- df.menhaden[,-which(colnames(df.menhaden)=="collection")]
df.menhaden <- merge(df.menhaden, check, by = c("month","vessel","year"))

df.menhaden$collection <- as.factor(df.menhaden$collection)



##### Calculate condition

LW.dat <- cbind(df.menhaden$fork_len,df.menhaden$weight)
weight.length.fit <- nls(LW.dat[,2] ~ a* LW.dat[,1] ^b, 
                         start = list(a = 0.00003, b = 3))
weight.length.fit.sum <- summary(weight.length.fit)
weight.length.fit.sum

a.est <- weight.length.fit.sum$parameters[1,1] 
b.est <- weight.length.fit.sum$parameters[2,1]

#Calculate relative condition

df.menhaden$relative.condition=(df.menhaden$weight/(a.est*df.menhaden$fork_len^b.est))*100


rm(j,new,percent.mature,weight.length.fit.sum,weight.length.fit,weight.length.fit.predict,LW.dat,data.frame.men.pi.added,spurious,x, data.frame.men.pi)

save(df.menhaden, file = "menhaden_data_post_processing_6_20.RData")
