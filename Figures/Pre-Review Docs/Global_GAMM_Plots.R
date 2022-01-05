model <- global.bam
summary(model)

###################
# River
###################

# Load state lines
state.lines <- readShapeLines("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Maps\\Internal State Boundaries\\state_bounds.shp")

####################
# Coefficients
####################

#Predict Values
pred=predict(model,type="terms",se.fit=T) # Predicts the response given the original data
pred.vec=pred[[1]][,3]
sigCI <- ifelse(pred.vec+1.96*pred$se.fit[,3]>=0 & pred.vec-1.96*pred$se.fit[,3]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden.short<- cbind(df.menhaden.short, pred.vec, sigCI)


# Create data frame of predictor coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
coef.df <- data.frame( location = NA, predictor.coef = NA)
for (i in 1:length(unique(df.menhaden.short$location))) {
  location.vec <- unique(df.menhaden.short$location)
  loc.subset.df <- subset(df.menhaden.short, df.menhaden.short$location == location.vec[i])
  subset.lm <- lm(pred.vec ~ monthly.miss.scaled, data = loc.subset.df ) ############## CHANGE Predictor HERE
  coef.df[i,2] <- subset.lm$coefficients[2]
  coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI) ~ location, data = df.menhaden.short, mean ,na.rm=T)
df.location.u <- merge(df.loc, coef.df , by = "location")

y.extent <- range(df.menhaden.short$latitude)[2]-range(df.menhaden.short$latitude)[1]
x.extent <- range(df.menhaden.short$longitude)[2]-range(df.menhaden.short$longitude)[1]
max.t=max(abs(df.location.u$predictor.coef), na.rm=T)




###################
######## Plot it
######################
tiff(file="Global_GAMM_River_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
nf <- layout(matrix(c(1,1,2,3,4,4,5,5) , 4 , 2 , byrow=TRUE) ,  heights=c(1 , 1 , 1 , .4) , widths = c(2.77,1),  TRUE)
par( mar=c(0, 3 , .5 , 4) , oma=c(0 , 1 , 0 , 0))


#Predictor
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
max.t=max(abs(df.location.u$predictor.coef), na.rm=T)
map("usa",fill=T,col="grey",add=T)

#positive slopes
symbols( x = df.location.u$longitude[df.location.u$predictor.coef>0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$predictor.coef>0 & df.location.u$sigCI==1] , circle= abs((df.location.u$predictor.coef)[df.location.u$predictor.coef>0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$predictor.coef)[df.location.u$predictor.coef>0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="white")  

#negative slopes
symbols( x = df.location.u$longitude[df.location.u$predictor.coef<0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$predictor.coef<0 & df.location.u$sigCI==1] , circle= abs((df.location.u$predictor.coef)[df.location.u$predictor.coef<0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$predictor.coef)[df.location.u$predictor.coef<0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="#555555")


symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12)
text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),2)),"+ effect"))

symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12,bg="#555555")
text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),2)*-1),"- effect"))

#State lines
plot(state.lines, add=T)

#axis
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
#points(y = df.menhaden$latitude, x=df.menhaden$longitude)
#legend("topleft", "Mississippi River discharge", bg = "grey90", cex=1.5)
box(which = "plot", lty = "solid")



dev.off()





################
# MEI
################

tiff(file="Global_GAMM_ENSO_Plot.tiff" , height=2.6 ,  width=3 , pointsize=9 , family = "serif" , units = "in", res =600)
par(xpd=F)
nf <- layout(matrix(c(1:2) , 2 , 1 , byrow=TRUE) ,  heights=c(1 , .6 ) ,  TRUE)
par( mar=c(0 , 2 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))
plot(model , scale=0 , pch=19 , scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=3 , xlim=c(min(df.menhaden.short$mei.lag.10, na.rm=T) , max(df.menhaden.short$mei.lag.10, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1)
legend("topleft", "a.", bty="n", inset = -0.05)
title(ylab=substitute(paste(italic("Kn" ))), line=.8 , outer=T , adj = .7)

par( mar=c(3 , 2 , .5 , 1))

hist(~(df.menhaden.short$mei.lag.10) , xlab=NA , ylab=NA ,  main=NULL ,   xlim=c(min(df.menhaden.short$mei.lag.10, na.rm=T) , max(df.menhaden.short$mei.lag.10, na.rm=T)), las =1, yaxt = "n" )
legend("topleft", "b.", bty="n", inset = c(-0.05,-.15))
axis(side = 2, at = c(seq(from = 0 , to = 25000, by = 5000)), labels = c("0","","1","","2",""), las = 1)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 400, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 400, x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
mtext( "Multivariate ENSO Index" , side = 1 , line =2)
title(ylab="Frequency", line=.8 , outer=T , adj = .21)
title(ylab="(x 10,000)", line=-.10 , outer=T , adj = .21)

dev.off()

#############################
# Wind
#############################

# Load state lines
state.lines <- readShapeLines("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Maps\\Internal State Boundaries\\state_bounds.shp")

####################
#u-wind Coefficients
####################

#Predict Values
pred=predict(model,type="terms",se.fit=T) # Predicts the response given the original data
pred.UWind.vec=pred[[1]][,5]
sigCI <- ifelse(pred.UWind.vec+1.96*pred$se.fit[,5]>=0 & pred.UWind.vec-1.96*pred$se.fit[,5]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden.short<- cbind(df.menhaden.short, pred.UWind.vec, sigCI)


# Create data frame of linear wind coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
u.wind.coef.df <- data.frame( location = NA, variable.uwind.coef = NA)
for (i in 1:length(unique(df.menhaden.short$location))) {
  location.vec <- unique(df.menhaden.short$location)
  loc.subset.df <- subset(df.menhaden.short, df.menhaden.short$location == location.vec[i])
  subset.lm <- lm(pred.UWind.vec ~ monthly.u.wind.lag1, data = loc.subset.df ) ##############
  u.wind.coef.df[i,2] <- subset.lm$coefficients[2]
  u.wind.coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI) ~ location, data = df.menhaden.short, mean ,na.rm=T)
df.location.u <- merge(df.loc, u.wind.coef.df , by = "location")

y.extent <- range(df.menhaden.short$latitude)[2]-range(df.menhaden.short$latitude)[1]
x.extent <- range(df.menhaden.short$longitude)[2]-range(df.menhaden.short$longitude)[1]
max.t=max(abs(df.location.u$variable.uwind.coef), na.rm=T)





######################
# V- Wind Coefficients
######################
pred.VWind.vec=pred[[1]][,6]
sigCI.v <- ifelse(pred.VWind.vec+1.96*pred$se.fit[,6]>=0 & pred.VWind.vec-1.96*pred$se.fit[,6]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden.short<- cbind(df.menhaden.short, pred.VWind.vec, sigCI.v)

# Create data frame of linear wind coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
coef.df <- data.frame( location = NA, variable.vwind.coef = NA)
for (i in 1:length(unique(df.menhaden.short$location))) {
  location.vec <- unique(df.menhaden.short$location)
  loc.subset.df <- subset(df.menhaden.short, df.menhaden.short$location == location.vec[i])
  subset.lm <- lm(pred.VWind.vec ~ monthly.v.wind.lag1, data = loc.subset.df )
  coef.df[i,2] <- subset.lm$coefficients[2]
  coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI.v) ~ location, data = df.menhaden.short, mean ,na.rm=T)
df.location <- merge(df.loc, coef.df , by = "location")
max.t.vwnd=max(abs(df.location$variable.vwind.coef), na.rm=T)


####################
# cluster analysis
####################
df.coefficients <- merge(df.location.u, df.location, by = c("longitude", "latitude", "location"))
df.coefficients2 <- df.coefficients[complete.cases(df.coefficients),]
df.coefficients2 <- subset(df.coefficients2, df.coefficients2$sigCI ==1 & df.coefficients2$sigCI.v ==1)

df <- df.coefficients2[,c(5,7)]

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} # http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

#wssplot(df)
#library(NbClust)
#set.seed(1234)
#nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
#table(nc$Best.n[1,])

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)                           #3
fit.km$size
fit.km$centers
fit.km$cluster
df.coefficients2 <- data.frame(df.coefficients2, fit.km$cluster)
head(df.coefficients2)

# Regular Polygons
library(cluster)
library(rgeos)

df.coefficients23= df.coefficients2


###################
######## Plot it
######################
tiff(file="Global_GAMM_Wind_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
nf <- layout(matrix(c(1,1,2,2,3,4,5,5) , 4 , 2 , byrow=TRUE) ,  heights=c(1 , 1 , 1 , .4) , widths = c(2.77,1),  TRUE)
par( mar=c(0, 3 , .5 , 4) , oma=c(0 , 1 , 0 , 0))


#U-Wind
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
max.t=max(abs(df.location.u$variable.uwind.coef), na.rm=T)
map("usa",fill=T,col="grey",add=T)

#positive slopes
symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="white")  

#negative slopes
symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="#555555")


symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12)
text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)),"West wind"))

symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12,bg="#555555")
text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)*-1),"East wind"))

#State lines
plot(state.lines, add=T)

#axis
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
#points(y = df.menhaden.short$latitude, x=df.menhaden.short$longitude)
box(which = "plot", lty = "solid")
legend("topleft", "a.", bty ="n", cex=1.5) 






##########
### v-Wind
##########
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )

map("usa",fill=T,col="grey",add=T)

#positive slopes
symbols( x = df.location$longitude[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1] , y = df.location$latitude[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1] , circle= abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1]) , inches=0.12*max(abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1]), na.rm=T)/max.t.vwnd,add=T,fg=1,bg="white")  

#negative slopes
symbols( x = df.location$longitude[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1] , y = df.location$latitude[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1] , circle= abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1]) , inches=0.12*max(abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1]), na.rm=T)/max.t.vwnd,add=T,fg=1,bg="#555555")

#legend
symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12)
text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t.vwnd,max.t.vwnd/5,length=4),3)),"South wind"))

symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t.vwnd,max.t.vwnd/5,length=4),add=T,inches=0.12,bg="#555555")
text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t.vwnd,max.t.vwnd/5,length=4),3)*-1),"North wind"))

plot(state.lines, add=T)

#axis
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
#axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
#points(y = df.menhaden.short$latitude, x=df.menhaden.short$longitude)
box(which = "plot", lty = "solid")
legend("topleft", "b.", bty ="n", cex=1.5) 



######################
##### Cluster analysis
######################

par( mar=c(0, 3 , .5 , .5))


wind.color.ramp <- colorRampPalette(c("white","black"))
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -88) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
map("usa",fill=T,col="grey",add=T)
plot(state.lines, add=T)
df.coefficients23<- data.frame(df.coefficients23)
points(x=df.coefficients23$longitude, y = df.coefficients23$latitude, pch =22 , col=1, bg = wind.color.ramp(length(unique(df.coefficients23$fit.km.cluster)))[df.coefficients23$fit.km.cluster], cex = 2.1)
#axis

axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
#points(y = df.menhaden.short$latitude, x=df.menhaden.short$longitude)
box(which = "plot", lty = "solid")
legend("topleft", "c.", bty ="n", cex=1.5) 


######################
# Clusters
######################
par( mar=c(0, .5 , .5 , 4))


plot(NA, xlim= c(min(df.coefficients23$variable.uwind.coef, na.rm =T),max(df.coefficients23$variable.uwind.coef, na.rm =T)), ylim = c(min(df.coefficients23$variable.vwind.coef, na.rm =T),max(df.coefficients23$variable.vwind.coef, na.rm =T)) , xlab = NA, ylab=NA, las =1, yaxt = "n", xaxt="n")
for( i in 1: length(unique(df.coefficients23$fit.km.cluster))){
  dat <- df.coefficients23[which(df.coefficients23$fit.km.cluster==i),c("variable.uwind.coef","variable.vwind.coef")]
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ] 
  polygon(coords, col = wind.color.ramp(length(unique(df.coefficients23$fit.km.cluster)))[i])
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  center = gCentroid(sp_poly,byid=TRUE)
  #text(center, labels = i)
}

axis(side=1, at = seq(from=-0.02,to=0.02,by = 0.01),labels = c(NA,"-0.01",NA,"0.01",NA))
axis(side = 4)
mtext( "Zonal wind (E-W)" , side = 1 , line =2.4, cex = .8)
mtext("Meridional wind (N-S)", side= 4, line=2.4, cex = .8)
legend("topright", "d.", bty ="n", cex=1.5) 

abline(h=0,v=0, lty =5)
#mtext("W",side=1, line=2.4, adj=1.1)
#mtext("E",side=1, line=2.4, adj=-.05)
#mtext("S",side=4, line=2.4, padj=-6,las=1)
#mtext("N",side=4, line=2.4, padj=6.5,las=1)

par( mar=c(5, 3 , .5 , 4))

dev.off()




##################
## SST
##################
tiff(file="Global_GAMM_SST_Plot.tiff" , height=2.6 ,  width=3 , pointsize=9 , family = "serif" , units = "in", res =600)
par(xpd=F)
nf <- layout(matrix(c(1:2) , 2 , 1 , byrow=TRUE) ,  heights=c(1 , .6 ) ,  TRUE)
par( mar=c(0 , 2 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))
plot(model , scale=0 , pch=19 ,  scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=6 , xlim=c(min(df.menhaden.short$SST_9km_lag_0, na.rm=T) , max(df.menhaden.short$SST_9km_lag_0, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1)
legend("topleft", "a.", bty="n", inset = -0.05)
title(ylab=substitute(paste(italic("Kn" ))), line=.8 , outer=T , adj = .7)

par( mar=c(3 , 2 , .5 , 1))

hist(~(df.menhaden.short$SST_9km_lag_0) , xlab=NA , ylab=NA ,  main=NULL ,   xlim=c(min(df.menhaden.short$SST_9km_lag_0, na.rm=T) , max(df.menhaden.short$SST_9km_lag_0, na.rm=T)), las =1, yaxt = "n" )
legend("topleft", "b.", bty="n", inset = c(-0.05,-.15))
axis(side = 2, at = c(seq(from = 0 , to = 6000, by = 2000)), labels = c("","2,000","","4,000"), las = 1)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 400, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 400, x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
mtext( expression(paste("SST (",degree,"C)")) , side = 1 , line =2)
title(ylab="Frequency", line=.8 , outer=T , adj = .21)


dev.off()




#######################
# Chl
#######################

####################
# Coefficients
####################

#Predict Values
pred=predict(model,type="terms",se.fit=T) # Predicts the response given the original data
pred.vec=pred[[1]][,8]
sigCI <- ifelse(pred.vec+1.96*pred$se.fit[,8]>=0 & pred.vec-1.96*pred$se.fit[,8]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden.short<- cbind(df.menhaden.short, pred.vec, sigCI)


# Create data frame of predictor coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
coef.df <- data.frame( location = NA, predictor.coef = NA)
for (i in 1:length(unique(df.menhaden.short$location))) {
  location.vec <- unique(df.menhaden.short$location)
  loc.subset.df <- subset(df.menhaden.short, df.menhaden.short$location == location.vec[i])
  subset.lm <- lm(pred.vec ~ chl_9km_lag_5, data = loc.subset.df ) ############## CHANGE Predictor HERE
  coef.df[i,2] <- subset.lm$coefficients[2]
  coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI) ~ location, data = df.menhaden.short, mean ,na.rm=T)
df.location.u <- merge(df.loc, coef.df , by = "location")

y.extent <- range(df.menhaden.short$latitude)[2]-range(df.menhaden.short$latitude)[1]
x.extent <- range(df.menhaden.short$longitude)[2]-range(df.menhaden.short$longitude)[1]
max.t=max(abs(df.location.u$predictor.coef), na.rm=T)



###################
######## Plot it
######################
tiff(file="Global_GAMM_CHL_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
nf <- layout(matrix(c(1,1,2,3,4,4,5,5) , 4 , 2 , byrow=TRUE) ,  heights=c(1 , 1 , 1 , .4) , widths = c(2.77,1),  TRUE)
par( mar=c(0, 3 , .5 , 4) , oma=c(0 , 1 , 0 , 0))


#Predictor
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
max.t=max(abs(df.location.u$predictor.coef), na.rm=T)
map("usa",fill=T,col="grey",add=T)

plot(state.lines, add=T)


#positive slopes
symbols( x = df.location.u$longitude[df.location.u$predictor.coef>0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$predictor.coef>0 & df.location.u$sigCI==1] , circle= abs((df.location.u$predictor.coef)[df.location.u$predictor.coef>0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$predictor.coef)[df.location.u$predictor.coef>0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="white")  

#negative slopes
symbols( x = df.location.u$longitude[df.location.u$predictor.coef<0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$predictor.coef<0 & df.location.u$sigCI==1] , circle= abs((df.location.u$predictor.coef)[df.location.u$predictor.coef<0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$predictor.coef)[df.location.u$predictor.coef<0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="#555555")


symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12)
text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),4)),"+ effect"))

symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12,bg="#555555")
text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),4)*-1),"- effect"))

#State lines


#axis
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
#points(y = df.menhaden.short$latitude, x=df.menhaden.short$longitude)
box(which = "plot", lty = "solid")
#legend("topleft", "a.", bty="n", cex=1.5) 



dev.off()



#####################
# Dist
#####################


###########
# Plot it


###########

model.sum<-summary(model)
coef <- c(model.sum$p.coeff)
coef[2:length(coef)] <- coef[2:length(coef)] + coef[1]
sd <- c(model.sum$p.table[,2])
year <- c(2003:2011)
d <- data.frame(std.dev = sd, coefficients = coef, year = year)


############
# Plot
############

tiff(file="Global_GAMM_SHORT_Distribution_Seasonal_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
nf <- layout(matrix(c(1,2,3,4,5,5,6,6,7,7) , 5 , 2 , byrow=TRUE) ,  heights=c(1 , .4 , .2 , 1.5,.2) , widths = c(1.885,1.885),  TRUE)
par(  oma=c(0 , 1 , 0 , 0))
par(xpd=F)

##################
# Figures
#################

par( mar=c(0 , 4 , .5 , 1))

# Year
plot(x = year, y = coef, las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5)
#arrows(year,  coef - sd, year , coef + sd, code = 3, length = .001, col = 1)
mtext(substitute(paste(italic("Kn" ))),side=2, adj = .5, line= 3.5)
legend("topleft", "a.", bty="n", cex = 1.5, inset = -0.05)

par( mar=c(0 , 3.5 , .5 , 1))


# Week
plot(model , scale=0 , pch=19 , scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=8 , xlim=c(min(df.menhaden.short$week, na.rm=T) , max(df.menhaden.short$week, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1, cex.axis = 1.5)
legend("topleft", "b.", bty="n", cex = 1.5, inset = -0.05)



############
#Histogram
############
# Year

par( mar=c(0 , 4 , .5, 1))

hist(~(df.menhaden.short$year ) , xlab=NA , ylab=NA ,  main=NULL ,  xlim=c(2002.5 ,2011.5), las =1, yaxt = "n" , cex.axis = 1.5,breaks = c(seq(from = 2002.5,to=2011.5,by=1)), ylim = c(0,6000))
axis(side =1, at =c(seq(from=2003,to=2011,by=2)), labels = NA)


axis(side = 2, at = c(seq(from = 0 , to = 6000, by = 1000)), labels = c("0","","2","","4","","6"), las = 1,  cex.axis = 1.5)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 400, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 400, x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line

legend("topleft", "c.", bty="n", cex = 1.5, inset = c(-0.05,-.15))
mtext("Year" , side = 1 , line =2.5)
mtext("Frequency", side = 2, line=3.5 , adj = .21)
mtext("(x 1,000)", side = 2, line=2.2 , adj = .21)

# Month
par( mar=c(0 , 3.5 , .5 , 1))
par(xpd=F)

hist(~(df.menhaden.short$week ) , xlab=NA , ylab=NA ,  main=NULL ,  xlim=c(min(df.menhaden.short$week, na.rm=T) , max(df.menhaden.short$week, na.rm=T)), las =1, yaxt = "n" , cex.axis = 1.5,ylim=c(0,3000))

legend("topleft", "d.", bty="n", cex = 1.5, inset = c(-0.05,-.15))
axis(side = 2, at = c(seq(from = 0 , to = 3000, by = 500)), labels = c("0","","1","","2","","3"), las = 1,  cex.axis = 1.5)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 200, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 200, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 200, x1 = par("usr")[2], y1 = par("usr")[4] + 200, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
mtext( "Week"  , side = 1 , line =2.5)
#title(ylab="Frequency", line=2 , adj = .21)
par(xpd=F)


plot.new()
par(mar=c(2, 4 , 2 , 1))

# Distribution
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -88) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )

vis.gam(model , view = c("longitude", "latitude") ,color = "gray", plot.type = "contour", type = "link", labcex = .7, xlab="",ylab="", too.far = .1, main=NA, nCol = 100 , contour.col = 1, n.grid = 100 , nlevels = 6, axes=F, add=T)

map("usa",fill=T,col="grey",add=T)
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
plot(state.lines ,add= T)
legend("topleft", "e.", bty = "n", cex=1.5) 
box(which = "plot", lty = "solid")
dev.off()

