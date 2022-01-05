library(mgcv)
library(fossil)
library(sp)
library(maptools)
library(Hmisc)
library(FSA)


load("menhaden_data_post_processing_6_20.RData")
spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k =6) + factor(year), data= df.menhaden, method = "fREML", family = Gamma(link = "log"))
model <- spatially.explicit.bam

# Load state lines
state.lines <- maptools::readShapeLines("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Shapefiles\\Internal State Boundaries\\state_bounds.shp")

###########
# Plot it


###########

model.sum<-summary(model)
coef <- c(model.sum$p.coeff)
coef[2:length(coef)] <- coef[2:length(coef)] + coef[1]
sd <- c(model.sum$p.table[,2])
year <- c(1964:1996, 1998:2011)
year <- c(year, 1997)
coef <- c(coef, NA)
coef <- coef[order(year)]
sd <- c(sd, NA)
sd <- sd[order(year)]


year <- sort(year)



d <- data.frame(std.dev = sd, coefficients = coef, year = year)




############
# Plot
############

tiff(file="GAMM_Distribution_Seasonal_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
nf <- layout(matrix(c(1,2,3,4,5,5,6,6,7,7) , 5 , 2 , byrow=TRUE) ,  heights=c(1 , .4 , .2 , 1.5,.2) , widths = c(1.885,1.885),  TRUE)
par(  oma=c(0 , 1 , 0 , 0))
par(xpd=F)

##################
# Figures
#################

par( mar=c(0 , 4 , .5 , 1))

# Year
plot(x = year, y = exp(coef), las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5)
#arrows(year,  coef - sd, year , coef + sd, code = 3, length = .001, col = 1)
mtext(substitute(paste(italic("Kn" ))),side=2, adj = .5, line= 3.5)
legend("topleft", "a.", bty="n", cex = 1.5, inset = -0.05)

par( mar=c(0 , 3.5 , .5 , 1))


# Week
plot(model , scale=0 , pch=19 , scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=2 , xlim=c(min(df.menhaden$month, na.rm=T) , max(df.menhaden$month, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1, cex.axis = 1.5)
legend("topleft", "b.", bty="n", cex = 1.5, inset = -0.05)



############
#Histogram
############
# Year

par( mar=c(0 , 4 , .5 , 1))

hist(~(df.menhaden$year ) , xlab=NA , ylab=NA ,  main=NULL ,  xlim=c(min(df.menhaden$year, na.rm=T) , max(df.menhaden$year, na.rm=T)), las =1, yaxt = "n" , cex.axis = 1.5)
axis(side =1, at =c(seq(from=1964,to=2011,by=2)), labels = NA)


axis(side = 2, at = c(seq(from = 0 , to = 25000, by = 5000)), labels = c("0","","1","","2",""), las = 1,  cex.axis = 1.5)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 400, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 400, x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line

legend("topleft", "c.", bty="n", cex = 1.5, inset = c(-0.05,-.15))
mtext("Year" , side = 1 , line =2.5)
mtext("Frequency", side = 2, line=3.5 , adj = .21)
mtext("(x 10,000)", side = 2, line=2.2 , adj = .21)

# Month
par( mar=c(0 , 3.5 , .5 , 1))
par(xpd=F)

hist(~(df.menhaden$month ) , xlab=NA , ylab=NA ,  main=NULL ,  xlim=c(min(df.menhaden$month, na.rm=T) , max(df.menhaden$month, na.rm=T)), las =1, yaxt = "n" , cex.axis = 1.5, breaks = seq(from = 1.5, to = 12.5, by = 1))

legend("topleft", "d.", bty="n", cex = 1.5, inset = c(-0.05,-.15))
axis(side = 2, at = c(seq(from = 0 , to = 100000, by = 10000)), labels = c("0","","2","","4","","6","","8", "","10"), las = 1,  cex.axis = 1.5)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 400, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 400, x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
mtext( "Month"  , side = 1 , line =2.5)
#title(ylab="Frequency", line=2 , adj = .21)
par(xpd=F)


plot.new()
par(mar=c(2, 4 , 2 , 1))

# Distribution
plot(NA, ylim = c(28.57 , 29.93) , xlim = c( -94.98 , -88.2) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )

vis.gam(model , view = c("longitude", "latitude") ,color = "gray", plot.type = "contour", type = "response", labcex = .7, xlab="",ylab="", too.far = .5, main=NA, nCol = 100 , contour.col = 1, n.grid = 100 , nlevels = 6, axes=F, add=T)

map("usa",fill=T,col="grey",add=T)
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
plot(state.lines ,add= T)
legend("topleft", "e.", bty = "n", cex=1.5) 
box(which = "plot", lty = "solid")


dev.off()



#########################################################################
################### Short model #########################################

load("menhaden_data_post_processing_6_20.RData")
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
spatially.explicit.bam.short=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))
model <- spatially.explicit.bam.short


###########
# Plot it


###########

model.sum<-summary(model)
coef <- c(model.sum$p.coeff)
coef[2:length(coef)] <- coef[2:length(coef)] + coef[1]
sd <- c(model.sum$p.table[,2])
year <- c(2002:2011)
d <- data.frame(std.dev = sd, coefficients = coef, year = year)


############
# Plot
############

tiff(file="GAMM_SHORT_Distribution_Seasonal_Plot.tiff" , height= 110,  width=160 , pointsize=10 , family = "serif" , units = "mm", res =300)
nf <- layout(matrix(c(1,2,3,4,5,5,6,6,7,7) , 5 , 2 , byrow=TRUE) ,  heights=c(1 , .4 , .2 , 1.5,.2) , widths = c(1.885,1.885),  TRUE)
par(  oma=c(0 , 1 , 0 , 0))
par(xpd=F)

##################
# Figures
#################

par( mar=c(0 , 4 , .5 , 1))

# Year
plot(x = year, y = exp(coef), las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5)
#arrows(year,  coef - sd, year , coef + sd, code = 3, length = .001, col = 1)
mtext(substitute(paste(italic("Kn" ))),side=2, adj = .5, line= 3.5)
legend("topleft", "a.", bty="n", cex = 1.5, inset = -0.05)

par( mar=c(0 , 3.5 , .5 , 1))


# Week
plot(model , scale=0 , pch=19 , scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=2 , xlim=c(min(df.menhaden.short$month, na.rm=T) , max(df.menhaden.short$month, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1, cex.axis = 1.5)
legend("topleft", "b.", bty="n", cex = 1.5, inset = -0.05)



############
#Histogram
############
# Year

par( mar=c(0 , 4 , .5, 1))

hist(~(df.menhaden.short$year ) , xlab=NA , ylab=NA ,  main=NULL ,  xlim=c(2002.5 ,2011.5), las =1, yaxt = "n" , cex.axis = 1.5,breaks = c(seq(from = 2001.5,to=2011.5,by=1)), ylim = c(0,6000))
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

hist(~(df.menhaden.short$month ) , xlab=NA , ylab=NA ,  main=NULL ,  xlim=c(min(df.menhaden.short$month, na.rm=T) , max(df.menhaden.short$month, na.rm=T)), las =1, yaxt = "n" , cex.axis = 1.5, breaks = seq(from = 1.5, to = 12.5, by = 1))

legend("topleft", "d.", bty="n", cex = 1.5, inset = c(-0.05,-.15))
axis(side = 2, at = c(seq(from = 0 , to = 6000, by = 1000)), labels = c("0","1","2","3","4","5","6"), las = 1,  cex.axis = 1.5)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 200, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 200, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 200, x1 = par("usr")[2], y1 = par("usr")[4] + 200, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
mtext( "Month"  , side = 1 , line =2.5)
#title(ylab="Frequency", line=2 , adj = .21)
par(xpd=F)


plot.new()
par(mar=c(2, 4 , 2 , 1))

# Distribution
plot(NA, ylim = c(28.57 , 29.93) , xlim = c( -94.98 , -88.2) , yaxs='i', xaxs='i' , xaxt = "n", yaxt = "n", xlab = NA, ylab=NA )

vis.gam(model , view = c("longitude", "latitude") ,color = "gray", plot.type = "contour", type = "response", labcex = .7, xlab="",ylab="", too.far = .5, main=NA, nCol = 100 , contour.col = 1, n.grid = 100 , nlevels = 6, axes=F, add=T)

map("usa",fill=T,col="grey",add=T)
axis(2, at =seq(29, 30, 1), labels = c("29ºN", "30ºN"), las =1, col =1 , cex.axis = 1.5)
axis(1, at =seq(-94, -84 , 2)   , labels = c("94ºW", "92ºW","90ºW", "88ºW", "86ºW" ,"84ºW") , col =1, cex.axis = 1.5)
plot(state.lines ,add= T)
legend("topleft", "e.", bty = "n", cex=1.5) 
box(which = "plot", lty = "solid")
dev.off()
