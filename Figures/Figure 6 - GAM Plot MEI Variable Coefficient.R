library(mgcv)
library(fossil)
library(sp)
library(maptools)



load("menhaden_data_post_processing_6_20.RData")
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(latitude, longitude, by = mei.lag.7) + s(individual.monthly.miss.scaled, k = 6) + s(latitude, longitude, by = monthly.u.wind.lag2) + s(latitude, longitude, by = monthly.v.wind.lag2), data = df.menhaden, method = "fREML")
model <- final_model

# Load state lines
state.lines <- readOGR(".", "state_lines")

####################
#u-wind Coefficients
####################

####################
# Coefficients
####################

#Predict Values
pred=predict(model,type="terms",se.fit=T) # Predicts the response given the original data
pred.vec=pred[[1]][,4]
sigCI <- ifelse(pred.vec+1.96*pred$se.fit[,4]>=0 & pred.vec-1.96*pred$se.fit[,4]<0,0,1) #Is the CI significantly different from zero? 0 if no, 1 if yes
df.menhaden<- cbind(df.menhaden, pred.vec, sigCI)


# Create data frame of predictor coefficients #NEED TO CHECK TO MAKE SURE CI DOES NOT CROSS 0!!!!
coef.df <- data.frame( location = NA, predictor.coef = NA)
for (i in 1:length(unique(df.menhaden$location))) {
  location.vec <- unique(df.menhaden$location)
  loc.subset.df <- subset(df.menhaden, df.menhaden$location == location.vec[i])
  subset.lm <- lm(pred.vec ~ mei.lag.7, data = loc.subset.df ) ############## CHANGE Predictor HERE
  coef.df[i,2] <- subset.lm$coefficients[2]
  coef.df[i,1] <- location.vec[i]
}


df.loc <- aggregate(cbind(latitude, longitude, sigCI) ~ location, data = df.menhaden, mean ,na.rm=T)
df.location.u <- merge(df.loc, coef.df , by = "location")

y.extent <- range(df.menhaden$latitude)[2]-range(df.menhaden$latitude)[1]
x.extent <- range(df.menhaden$longitude)[2]-range(df.menhaden$longitude)[1]
max.t=max(abs(df.location.u$predictor.coef), na.rm=T)



###################
######## Plot it
######################
tiff(file="GAMM_MEI_VC_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
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
#points(y = df.menhaden$latitude, x=df.menhaden$longitude)
box(which = "plot", lty = "solid")
#legend("topleft", "a.", bty="n", cex=1.5) 



dev.off()
