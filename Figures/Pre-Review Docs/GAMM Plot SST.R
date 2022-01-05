library(mgcv)
library(fossil)
library(sp)
library(maptools)
library(FSA)

load("menhaden_data_post_processing_6_20.RData")
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
sst.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_0) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))
model <-sst.bam.lag.0

################
# Plot it
################

tiff(file="GAMM_SST_Plot.tiff" , height=2.6 ,  width=3 , pointsize=9 , family = "serif" , units = "in", res =600)
par(xpd=F)
nf <- layout(matrix(c(1:2) , 2 , 1 , byrow=TRUE) ,  heights=c(1 , .6 ) ,  TRUE)
par( mar=c(0 , 2 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))
plot(model , scale=0 , pch=19 ,  scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=2 , xlim=c(min(df.menhaden.short$SST_9km_lag_0, na.rm=T) , max(df.menhaden.short$SST_9km_lag_0, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1)
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


# Without the

tiff(file="GAMM_SST_Plot_No_Hist.tiff" , height=2.6 ,  width=3 , pointsize=10 , family = "serif" , units = "in", res =600)

par( mar=c(3 , 2 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))
plot(model , scale=0 , pch=19 ,  scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=2 , xlim=c(min(df.menhaden.short$SST_9km_lag_0, na.rm=T) , max(df.menhaden.short$SST_9km_lag_0, na.rm=T)), xlab = NA,  las =1)
title(ylab=substitute(paste("centered log(",italic("Kn" ),")")), line=.8 , outer=T , adj = .6)
mtext( expression(paste("SST (",degree,"C)")) , side = 1 , line =2)

dev.off()