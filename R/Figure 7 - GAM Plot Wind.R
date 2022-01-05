library(mgcv)
library(fossil)
library(sp)
library(maptools)



load("Data/Gulf_Menhaden_Data.RData")
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag2) + s(latitude, longitude, by = monthly.v.wind.lag2), data = df.menhaden, method = "fREML")
model <- final_model

# Load state lines
state.lines <- readOGR(".", "state_lines")

####################
#u-wind Coefficients
####################

#Predict Values
pred = predict(model,type="terms",se.fit=T) # Predicts the response given the original data
pred.UWind.vec=pred[[1]][,6]
sigCI <- ifelse(pred.UWind.vec+1.96*pred$se.fit[,6]>=0 & pred.UWind.vec-1.96*pred$se.fit[,6]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden <- cbind(df.menhaden, pred.UWind.vec, sigCI)


# Create data frame of linear wind coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
u.wind.coef.df <- data.frame( location = NA, variable.uwind.coef = NA)
for (i in 1:length(unique(df.menhaden$location))) {
  location.vec <- unique(df.menhaden$location)
  loc.subset.df <- subset(df.menhaden, df.menhaden$location == location.vec[i])
  subset.lm <- lm(pred.UWind.vec ~ monthly.u.wind.lag2, data = loc.subset.df ) ##############
  u.wind.coef.df[i,2] <- subset.lm$coefficients[2]
  u.wind.coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI) ~ location, data = df.menhaden, mean ,na.rm=T)
df.location.u <- merge(df.loc, u.wind.coef.df , by = "location")

y.extent <- range(df.menhaden$latitude)[2]-range(df.menhaden$latitude)[1]
x.extent <- range(df.menhaden$longitude)[2]-range(df.menhaden$longitude)[1]
max.t=max(abs(df.location.u$variable.uwind.coef), na.rm=T)





######################
# V- Wind Coefficients
######################
pred.VWind.vec=pred[[1]][,7]
sigCI.v <- ifelse(pred.VWind.vec+1.96*pred$se.fit[,7]>=0 & pred.VWind.vec-1.96*pred$se.fit[,7]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden<- cbind(df.menhaden, pred.VWind.vec, sigCI.v)

# Create data frame of linear wind coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
coef.df <- data.frame( location = NA, variable.vwind.coef = NA)
for (i in 1:length(unique(df.menhaden$location))) {
  location.vec <- unique(df.menhaden$location)
  loc.subset.df <- subset(df.menhaden, df.menhaden$location == location.vec[i])
  subset.lm <- lm(pred.VWind.vec ~ monthly.v.wind.lag2, data = loc.subset.df )
  coef.df[i,2] <- subset.lm$coefficients[2]
  coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI.v) ~ location, data = df.menhaden, mean ,na.rm=T)
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














###################
# No Cluster
###################
#tiff(file="Figures/Fig_7_GAM_Wind_Plot_No_Cluster.tiff" , height= 3.65,  width=3.34646*2 , pointsize=10 , family = "serif" , units = "in", res =300)
postscript("Figures/Fig_7_GAM_Wind_Plot_No_Cluster.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height= 3.65,  width=3.34646*2, pointsize = 10, family = "serif" )
nf <- layout(matrix(c(1,1,2,2,3,4) , 3 , 2 , byrow=TRUE) ,  heights=c(1 , 1 , .18) , widths = c(2.77,1),  TRUE)
par( mar=c(0, 1.5 , .5 , .1) , oma=c(0 , 1 , 0 , 0))
par(xpd = F)


#U-Wind
plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
max.t=max(abs(df.location.u$variable.uwind.coef), na.rm=T)
map("usa",fill=T,col="grey",add=T)

#positive slopes
symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="white")  

#negative slopes
symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="#555555")


symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12, bg = "white")
text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)),"West wind"))

symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12,bg="#555555")
text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)*-1),"East wind"))

#State lines
plot(state.lines, add=T)

#axis
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
#points(y = df.menhaden$latitude, x=df.menhaden$longitude)
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
symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12, bg = "white")
text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t.vwnd,max.t.vwnd/5,length=4),3)),"South wind"))

symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t.vwnd,max.t.vwnd/5,length=4),add=T,inches=0.12,bg="#555555")
text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t.vwnd,max.t.vwnd/5,length=4),3)*-1),"North wind"))

plot(state.lines, add=T)

#axis
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
#points(y = df.menhaden$latitude, x=df.menhaden$longitude)
box(which = "plot", lty = "solid")
legend("topleft", "b.", bty ="n", cex=1.5)

dev.off()



# ##################################################
# # Plot EW Wind
# ##################################################
# tiff(file="GAMM_Wind_Plot_No_Cluster_EW.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
# nf <- layout(matrix(c(1,1,2,2,3,4,5,5) , 4 , 2 , byrow=TRUE) ,  heights=c(1 , 1 , 1 , .4) , widths = c(2.77,1),  TRUE)
# par( mar=c(0, 3 , .5 , 4) , oma=c(0 , 1 , 0 , 0))
# 
# 
# #U-Wind
# plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
# max.t=max(abs(df.location.u$variable.uwind.coef), na.rm=T)
# map("usa",fill=T,col="grey",add=T)
# 
# #positive slopes
# symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="white")  
# 
# #negative slopes
# symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="#555555")
# 
# 
# symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12, bg = "white")
# text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)),"West wind"))
# 
# symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12,bg="#555555")
# text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)*-1),"East wind"))
# 
# #State lines
# plot(state.lines, add=T)
# 
# #axis
# axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
# axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
# #points(y = df.menhaden$latitude, x=df.menhaden$longitude)
# box(which = "plot", lty = "solid")
# legend("topleft", "Zonal wind vector (E-W)", bg = "grey90", cex=1.5) 
# dev.off()

# 
# tiff(file="Figures/Fig_7_GAM_Wind_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
# nf <- layout(matrix(c(1,1,2,2,3,4,5,5) , 4 , 2 , byrow=TRUE) ,  heights=c(1 , 1 , 1 , .4) , widths = c(2.77,1),  TRUE)
# par( mar=c(0, 3 , .5 , 4) , oma=c(0 , 1 , 0 , 0))
# 
# 
# #U-Wind
# plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
# max.t=max(abs(df.location.u$variable.uwind.coef), na.rm=T)
# map("usa",fill=T,col="grey",add=T)
# 
# #positive slopes
# symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef>0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="white")  
# 
# #negative slopes
# symbols( x = df.location.u$longitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , y = df.location.u$latitude[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1] , circle= abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]) , inches=0.12*max(abs((df.location.u$variable.uwind.coef)[df.location.u$variable.uwind.coef<0 & df.location.u$sigCI==1]), na.rm=T)/max.t,add=T,fg=1,bg="#555555")
# 
# 
# symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12, bg = "white")
# text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)),"West wind"))
# 
# symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12,bg="#555555")
# text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t,max.t/5,length=4),3)*-1),"East wind"))
# 
# #State lines
# plot(state.lines, add=T)
# 
# #axis
# axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
# #points(y = df.menhaden$latitude, x=df.menhaden$longitude)
# box(which = "plot", lty = "solid")
# legend("topleft", "a.", bty ="n", cex=1.5) 
# 
# 
# 
# 
# 
# 
# ##########
# ### v-Wind
# ##########
# plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -86) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
# 
# map("usa",fill=T,col="grey",add=T)
# 
# #positive slopes
# symbols( x = df.location$longitude[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1] , y = df.location$latitude[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1] , circle= abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1]) , inches=0.12*max(abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef>0 & df.location$sigCI.v==1]), na.rm=T)/max.t.vwnd,add=T,fg=1,bg="white")  
# 
# #negative slopes
# symbols( x = df.location$longitude[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1] , y = df.location$latitude[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1] , circle= abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1]) , inches=0.12*max(abs((df.location$variable.vwind.coef)[df.location$variable.vwind.coef<0 & df.location$sigCI.v==1]), na.rm=T)/max.t.vwnd,add=T,fg=1,bg="#555555")
# 
# #legend
# symbols(rep(-88.3,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t,max.t/5,length=4),add=T,inches=0.12, bg = "white")
# text(-87.75,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t.vwnd,max.t.vwnd/5,length=4),3)),"South wind"))
# 
# symbols(rep(-87,4),c(28.8,29.1,29.4,29.7),circles=seq(max.t.vwnd,max.t.vwnd/5,length=4),add=T,inches=0.12,bg="#555555")
# text(-86.45,c(28.8,29.1,29.4,29.7,29.9),c(format(round(seq(max.t.vwnd,max.t.vwnd/5,length=4),3)*-1),"North wind"))
# 
# plot(state.lines, add=T)
# 
# #axis
# axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
# #axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
# #points(y = df.menhaden$latitude, x=df.menhaden$longitude)
# box(which = "plot", lty = "solid")
# legend("topleft", "b.", bty ="n", cex=1.5) 
# 
# 
# 
# ######################
# ##### Cluster analysis
# ######################
# 
# par( mar=c(0, 3 , .5 , .5))
# 
# 
# wind.color.ramp <- colorRampPalette(c("white","black"))
# plot(NA, ylim = c(28.5 , 31) , xlim = c( -95 , -88) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )
# map("usa",fill=T,col="grey",add=T)
# plot(state.lines, add=T)
# df.coefficients23<- data.frame(df.coefficients23)
# points(x=df.coefficients23$longitude, y = df.coefficients23$latitude, pch =22 , col=1, bg = wind.color.ramp(length(unique(df.coefficients23$fit.km.cluster)))[df.coefficients23$fit.km.cluster], cex = 2.1)
# #axis
# 
# axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
# axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
# #points(y = df.menhaden$latitude, x=df.menhaden$longitude)
# box(which = "plot", lty = "solid")
# legend("topleft", "c.", bty ="n", cex=1.5) 
# 
# 
# ######################
# # Clusters
# ######################
# par( mar=c(0, .5 , .5 , 4))
# 
# 
# plot(NA, xlim= c(min(df.coefficients23$variable.uwind.coef, na.rm =T),max(df.coefficients23$variable.uwind.coef, na.rm =T)), ylim = c(min(df.coefficients23$variable.vwind.coef, na.rm =T),max(df.coefficients23$variable.vwind.coef, na.rm =T)) , xlab = NA, ylab=NA, las =1, yaxt = "n", xaxt="n")
# for( i in 1: length(unique(df.coefficients23$fit.km.cluster))){
#   dat <- df.coefficients23[which(df.coefficients23$fit.km.cluster==i),c("variable.uwind.coef","variable.vwind.coef")]
#   ch <- chull(dat)
#   coords <- dat[c(ch, ch[1]), ] 
#   polygon(coords, col = wind.color.ramp(length(unique(df.coefficients23$fit.km.cluster)))[i])
#   sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
#   center = gCentroid(sp_poly,byid=TRUE)
#   #text(center, labels = i)
# }
# 
# axis(side=1, at = seq(from=-0.02,to=0.02,by = 0.01),labels = c(NA,"-0.01",NA,"0.01",NA))
# axis(side = 4)
# mtext( "Zonal wind (E-W)" , side = 1 , line =2.4, cex = .8)
# mtext("Meridional wind (N-S)", side= 4, line=2.4, cex = .8)
# legend("topright", "d.", bty ="n", cex=1.5) 
# 
# abline(h=0,v=0, lty =5)
# #mtext("W",side=1, line=2.4, adj=1.1)
# #mtext("E",side=1, line=2.4, adj=-.05)
# #mtext("S",side=4, line=2.4, padj=-6,las=1)
# #mtext("N",side=4, line=2.4, padj=6.5,las=1)
# 
# par( mar=c(5, 3 , .5 , 4))
# 
# dev.off()
# 
# 
# 
