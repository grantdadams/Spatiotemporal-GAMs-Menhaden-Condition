
library(mgcv)
library(fossil)
library(sp)
library(maptools)


load("menhaden_data_post_processing_6_20.RData")
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

chl.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_2 ) + s(month, k = 6) + factor(year) + s(vessel, bs = "re"), data= df.menhaden.short, method = "ML", family = Gamma(link = "log"))

chl.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s(chl_9km_lag_2 ) + s(month, k = 6) + factor(year) + s(vessel, bs = "re"), data= df.menhaden.short, method = "ML", family = Gamma(link = "log"))

model <- chl.bam.vc.lag.2

# Load state lines
state.lines <- readShapeLines("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Shapefiles\\Internal State Boundaries\\state_bounds.shp")

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
  subset.lm <- lm(pred.vec ~ chl_9km_lag_2, data = loc.subset.df ) ############## CHANGE Predictor HERE
  coef.df[i,2] <- subset.lm$coefficients[2]
  coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI) ~ location, data = df.menhaden.short, mean ,na.rm=T)
df.location.u <- merge(df.loc, coef.df , by = "location")

y.extent <- range(df.menhaden.short$latitude)[2]-range(df.menhaden.short$latitude)[1]
x.extent <- range(df.menhaden.short$longitude)[2]-range(df.menhaden.short$longitude)[1]
max.t=max(abs(df.location.u$predictor.coef), na.rm=T)





#######################
## cluster analysis
#######################
df.coefficients <- df.location.u
df.coefficients2 <- df.coefficients[complete.cases(df.coefficients),]
df.coefficients2 <- subset(df.coefficients2, df.coefficients2$sigCI ==1)

df <- df.coefficients2[,c(5)]

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")} # http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

wssplot(df)
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

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
tiff(file="GAMM_CHL_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
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
