library(fossil)
library(sp)
library(mgcv)
library(maptools)
library(FSA)



load("Data/Gulf_Menhaden_Data.RData")
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag2) + s(latitude, longitude, by = monthly.v.wind.lag2), data = df.menhaden, method = "fREML")
model <- final_model

#tiff(file="Figures/Fig_5_GAM_MRD_Plot.tiff" , height=2.9 ,  width=3.34646 , pointsize=10 , family = "serif" , units = "in", res =600)
postscript("Figures/Fig_5_GAM_MRD_Plot.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height=2.9 ,  width=3.34646, pointsize = 10, family = "serif" )
par(xpd=F)
nf <- layout(matrix(c(1:2) , 2 , 1 , byrow=TRUE) ,  heights=c(1 , .6 ) ,  TRUE)
par( mar=c(0 , 2 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))
plot(model , scale=0 , pch=19 , scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=4 , xlim=c(min(df.menhaden$individual.monthly.miss.scaled, na.rm=T) , max(df.menhaden$individual.monthly.miss.scaled, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1)
legend("topleft", "a.", bty="n", inset = -0.05)
title(ylab=substitute(paste("Effect on ",italic("Kn" ))), line= -.3 , outer=T , adj = .7)

par( mar=c(3 , 2 , .5 , 1))

hist(~(df.menhaden$individual.monthly.miss.scaled) , xlab=NA , ylab=NA ,  main=NULL ,   xlim=c(min(df.menhaden$individual.monthly.miss.scaled, na.rm=T) , max(df.menhaden$individual.monthly.miss.scaled, na.rm=T)), las =1, yaxt = "n" )
legend("topleft", "b.", bty="n", inset = c(-0.05,-.15))
axis(side = 2, at = c(seq(from = 0 , to = 25000, by = 5000)), labels = c("0","","1","","2",""), las = 1)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 400, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 400, x1 = par("usr")[2], y1 = par("usr")[4] + 400, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
mtext( "Mississippi River discharge" , side = 1 , line =2)
title(ylab="Frequency", line=.3 , outer=T , adj = .21)
title(ylab="(x 10,000)", line=-.8 , outer=T , adj = .21)

dev.off()

