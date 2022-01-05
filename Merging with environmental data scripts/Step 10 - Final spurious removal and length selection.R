df.menhaden <- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 8.csv")

# 



# 
# 
# #####
# ##### Remove data outside the 99.99% Prediction Interval of the log length at weight relationship for each year
# #####
# 
# data.frame.men.pi=data.frame(matrix(ncol=c(ncol(data.frame.menhaden)+3)))
# par(mfrow=c(2,2))
# for (j in c(1964:2011)) {
#   x=subset(data.frame.menhaden, data.frame.menhaden$year==j)
#   if(nrow(x)>0){
#     LW.dat <- cbind(log(x$fork_len),log(x$weight))
#     weight.length.fit <- lm(LW.dat[,2] ~ LW.dat[,1]) 
#     #plot(log(x$fork_len), log(x$weight), main=j )
#     #abline(weight.length.fit, col=2)                  
#     weight.length.fit.sum <- summary(weight.length.fit)
#     weight.length.fit.sum
#     
#     new<-c(length.data=log(x$fork_len))
#     weight.length.fit.predict<-predict(weight.length.fit, data.frame(new), interval = "prediction", level=.9999)
#     #lines(new, weight.length.fit.predict[,2], col=2 )
#     #lines(new, weight.length.fit.predict[,3], col=2 )
#     
#     data.frame.men.pi.added<-cbind(weight.length.fit.predict,x)
#     colnames(data.frame.men.pi)<- colnames(data.frame.men.pi.added)
#     data.frame.men.pi<- rbind( data.frame.men.pi, data.frame.men.pi.added)
#     
#   }
# }
# 
# data.frame.men.pi<-data.frame.men.pi[-1,]
# head(data.frame.men.pi)
# data.frame.men.pi$remove<- ifelse( log(data.frame.men.pi$weight)>data.frame.men.pi$upr , "spurious", ifelse(log(data.frame.men.pi$weight)<data.frame.men.pi$lwr, "spurious", "not_spurious") )
# 
# data.frame.men.spurious.removed<- subset(data.frame.men.pi, data.frame.men.pi$remove=="not_spurious")
# spurious<- subset(subset(data.frame.men.pi, data.frame.men.pi$remove=="spurious"))

#write.csv(spurious, file= "C:/Users/w966213/Documents/Mehaden Data/NMFS Menhaden Surveys 1964-2011 mid-treatment/menhaden_spurious_data_by_9999%PI.csv" )



####
#### RESULTS PLOTS
####


data.frame.menhaden<- read.csv("C:\\Users\\w966213\\Documents\\Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location, spurious data removed by 99-99%PI.csv")

spurious<- read.csv("C:/Users/w966213/Documents/Mehaden Data/NMFS Menhaden Surveys 1964-2011 mid-treatment/menhaden_spurious_data_by_9999%PI.csv")

data.frame.menhaden.no.pda=read.csv("C:\\Users\\w966213\\Documents\\Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location.csv")
data.frame.menhaden.no.pda=subset(data.frame.menhaden.no.pda,data.frame.menhaden.no.pda$fork_len>137.2)

#####Create relative.condition

LW.dat <- cbind(data.frame.menhaden$fork_len,data.frame.menhaden$weight)
weight.length.fit <- nls(LW.dat[,2] ~ a* LW.dat[,1] ^b, 
                         start = list(a = 0.00003, b = 3))
weight.length.fit.sum <- summary(weight.length.fit)
weight.length.fit.sum

a.est <- weight.length.fit.sum$parameters[1,1] 
b.est <- weight.length.fit.sum$parameters[2,1]

#Calculate relative condition

data.frame.menhaden$relative.condition=(data.frame.menhaden$weight/(a.est*data.frame.menhaden$fork_len^b.est))*100



##### calculate relative condition for data with no outliers removed
LW.dat <- cbind(data.frame.menhaden.no.pda$fork_len, data.frame.menhaden.no.pda$weight)
weight.length.fit <- nls(LW.dat[,2] ~ a* LW.dat[,1] ^b, 
                         start = list(a = 0.00003, b = 3))
weight.length.fit.sum <- summary(weight.length.fit)
weight.length.fit.sum

a.est <- weight.length.fit.sum$parameters[1,1] 
b.est <- weight.length.fit.sum$parameters[2,1]

#Calculate relative condition

data.frame.menhaden.no.pda$relative.condition=(data.frame.menhaden.no.pda$weight/(a.est*data.frame.menhaden.no.pda$fork_len^b.est))*100

#see which data are not removed



#Relative condition at month by year of data with and without spurious data points removed
pdf("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1 Data and Map Work\\Adams Grant - Preliminary Data Analysis Plots with spurious data removed by 99_99 PI.pdf")
for (i in 1950:2012){
  x=subset(data.frame.menhaden, data.frame.menhaden$year==i)
  z=subset(data.frame.menhaden.no.pda, data.frame.menhaden.no.pda$year==i)
  s<-subset(spurious, spurious$year==i)
  if(nrow(x)>0){
    par(mfrow=c(4,2), mar=c(3,3,2,2)+.1, mgp=c(2,.5,0))
    plot(z$fork_len, z$weight, main = i, ylab="weight (g)", xlab="fork length (mm)" )
    points(s$fork_len, s$weight, col= 2)
    plot(x$fork_len, x$weight, main = paste( i,"Spurious Data Removed", sep = " "), ylab="weight (g)", xlab="fork length (mm)")
    boxplot(z$relative.condition~z$month, xlab= "month" , ylab = "Relative Condtion")
    boxplot(x$relative.condition~x$month, xlab= "month" , ylab = "Relative Condtion")
    hist(z$relative.condition, main =NA , xlab="Relative Condition")
    hist(x$relative.condition, main =NA , xlab="Relative Condition")
    plot(z$fork_len, z$relative.condition, main= NA, xlab= "Fork Length (mm)", ylab="Relative Condition")
    plot(x$fork_len, x$relative.condition,  main= NA, xlab= "Fork Length (mm)", ylab="Relative Condition")
  }
}
dev.off()




data.frame.menhaden=subset(data.frame.menhaden,data.frame.menhaden3$relative.condition<175)





####
#### VISUAL OUTLIER REMOVAL
####

# data.frame.menhaden=read.csv("C:/Users/w966213/Documents/Mehaden Data/menhaden_data_with_atch_and_location.csv")
# data.frame.menhaden$date=format(as.Date(data.frame.menhaden$date,"%m/%d/%Y"))
# 
# data.frame.menhaden=subset(data.frame.menhaden,data.frame.menhaden$fork_len>137.2) #
# 


# #Weight at length per year
# par(mfrow=c(2,2))
# for (i in 1950:2012){
#   x=subset(data.frame.menhaden, data.frame.menhaden$year==i)
#   if(nrow(x)>0){
#     plot(x$fork_len,x$weight, main = i)
#   }
# }
# 
# 
# 
# #there is an issue with some years
# 2000 and 2001 has the bifurcation
#so I created a loop to create a vector of row names for manually identified outliers
#then the last script removes them from the data.frame
# outliers=c()
# par(mfrow=c(2,2))
# for (j in c(1964:1996, 1998:2011)) {
#   plot(weight~fork_len, data= data.frame.menhaden[which(data.frame.menhaden$year==j),], main = j)
#   outliers= c(outliers,identify(data.frame.menhaden$fork_len,data.frame.menhaden$weight, labels=row.names(data.frame.menhaden)))
#   
# }

# data.frame.menhaden2<- data.frame.menhaden[-c(outliers),]
# spurious<- data.frame.menhaden[outliers,]
# 
# 
# #compare the removal
# outliers2<-c()
# par(mfrow=c(2,2))
# for (k in c(1964:1996, 1998:2011)){
#   plot(weight~fork_len, data= data.frame.menhaden[which(data.frame.menhaden$year==k),], main = k)
#   plot(weight~fork_len, data= data.frame.menhaden2[which(data.frame.menhaden2$year==k),], main = expression(paste(k & "outliers removed")))
#   outliers2= c(outliers2,identify(data.frame.menhaden2$fork_len,data.frame.menhaden2$weight, labels=row.names(data.frame.menhaden2)))
# }
# 
# data.frame.menhaden3<- data.frame.menhaden2[-c(outliers2),]
# spurious<- rbind(spurious, data.frame.menhaden2[outliers2,])
# 
# #one more try
# outliers3<-c()
# par(mfrow=c(2,2))
# for (k in  c(1964:1996, 1998:2011)){
#   plot(weight~fork_len, data= data.frame.menhaden[which(data.frame.menhaden$year==k),], main = k)
#   plot(weight~fork_len, data= data.frame.menhaden3[which(data.frame.menhaden3$year==k),], main = expression(paste(k & "outliers removed")))
#   outliers3= c(outliers3,identify(data.frame.menhaden3$fork_len,data.frame.menhaden3$weight, labels=row.names(data.frame.menhaden3)))
# }
# 
# data.frame.menhaden4<- data.frame.menhaden3[-c(outliers3),]
# spurious<- rbind(spurious, data.frame.menhaden3[outliers3,])

##compare to double check removal and replace those that accidently got removed
# par(mfrow=c(2,2))
# for (k in c(1964:1996, 1998:2011)){
#   plot(weight~fork_len, data= data.frame.menhaden[which(data.frame.menhaden$year==k),], main = k)
#   plot(weight~fork_len, data= data.frame.menhaden4[which(data.frame.menhaden4$year==k),], main = expression(paste(k & "outliers removed")))
# }

# # 
# spurious2<-spurious[-c(),]
# # 
# # 
# not.so.spurious<-spurious[c(),-c()]
# # 
# # 
# data.frame.menhaden4<- rbind(data.frame.menhaden4, not.so.spurious)
# # 
# write.csv(spurious2, file= "C:/Users/w966213/Documents/Mehaden Data/menhaden_spurious_data.csv" )
# write.csv(data.frame.menhaden4, file= "C:/Users/w966213/Documents/Mehaden Data/menhaden_data_with_atch_and_location_outliers_removed.csv" )

