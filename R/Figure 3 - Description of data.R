load("Data/Gulf_Menhaden_Data.RData")

library(Hmisc)
library(plyr)
library(lattice)
library(FSA)
library(dplyr)


LW.dat <- cbind(df.menhaden$fork_len,df.menhaden$weight)
weight.length.fit <- nls(LW.dat[,2] ~ a* LW.dat[,1] ^b, 
                         start = list(a = 0.00003, b = 3))
weight.length.fit.sum <- summary(weight.length.fit)
weight.length.fit.sum

a.est <- weight.length.fit.sum$parameters[1,1] 
b.est <- weight.length.fit.sum$parameters[2,1]



#tiff(file="Figures/Figure_3_description_of_data.tiff", height= 100,  width=110 , pointsize=10 , family = "serif" , units = "mm", res =300)
postscript("Figures/Figure_3_description_of_data.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = 3.34646, height = 4.015752, pointsize = 10, family = "serif" )
par( mar=c(.2 , 5 , .2 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 0 , 0 , 0))
layout(matrix(c(1,2,3,4,5), 5, 1, byrow = TRUE ), heights = c(1,1,1,1,.2))

# Number of samples
count <- plyr::count(df.menhaden,c("year"))
count <- rbind(count, c(1997, NA))
count <- count[order(count$year),]
plot(x = count$year, y = count$freq, las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5, lwd= 2, yaxt = "n")
axis(side =2, at =c(seq(from=2000,to=10000,by=2000)), labels = c(2,NA,6,NA,8), cex.axis = 1.5, las = 1)
abline(h = mean(count$freq), lty = 2)
mtext("Annual samples", side = 2 , line =3.8)
mtext("(x 1,000)" , side = 2 , line =2.6)
legend("topleft", "a.", bty="n", cex = 1.5)

####################
# Plot means and sd
###################
# Relative condition
d <- ddply(df.menhaden,~year,summarise,mean=mean(relative.condition),sd=sd(relative.condition))
d <- rbind(d, c(1997, NA, NA))
d <- d[order(d$year),]
plot(x = d$year, y = d$mean, las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5, ylim = c(85,115), lwd= 2, yaxt= "n")
axis(side =2, at = c(seq(from=85, to = 115, by = 5)), labels = c(NA,90,NA,100,NA,110,NA), cex.axis = 1.5, las = 1)
polygon(c(d$year[which(d$year < 1997)], rev(d$year[which(d$year < 1997)])), c(d$mean[which(d$year < 1997)] - d$sd[which(d$year < 1997)], rev(d$mean[which(d$year < 1997)] + d$sd[which(d$year < 1997)])), 
        col = "grey90", border = NA) 
polygon(c(d$year[which(d$year > 1997)], rev(d$year[which(d$year > 1997)])), c(d$mean[which(d$year > 1997)] - d$sd[which(d$year > 1997)], rev(d$mean[which(d$year > 1997)] + d$sd[which(d$year > 1997)])), 
        col = "grey90", border = NA) 
lines(x = d$year, y = d$mean, lwd= 2)
abline(h = 100, lty = 2)
mtext(substitute(paste(italic("Kn" ))) , side = 2 , line =2.8)
legend("topleft", "b.", bty="n", cex = 1.5)
box(which = "plot")

d <- ddply(df.menhaden,~year,summarise,mean=mean(mei),sd=sd(mei))
plot(x = d$year, y = d$mean, las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5, ylim = c(2,-2.5), lwd= 2)
polygon(c(d$year, rev(d$year)), c(d$mean - d$sd, rev(d$mean + d$sd)), 
        col = "grey90", border = NA) 
lines(x = d$year, y = d$mean, lwd= 2)
abline(h = 0, lty = 2)
mtext("ENSO" , side = 2 , line =2.8)
legend("topleft", "c.", bty="n", cex = 1.5)
box(which = "plot")

d <- ddply(df.menhaden,~year,summarise,mean=mean(monthly.miss),sd=sd(monthly.miss))
plot(x = d$year, y = d$mean, las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",yaxt = "n",  cex.axis = 1.5, ylim = c(0,30000), lwd= 2)
polygon(c(d$year, rev(d$year)), c(d$mean - d$sd, rev(d$mean + d$sd)), 
        col = "grey90", border = NA) 
lines(x = d$year, y = d$mean, lwd= 2)
abline(h = mean(d$mean), lty = 2)
mtext("River Discharge" , side = 2 , line =3.8)
mtext("(x 10,000)" , side = 2 , line =2.6)
legend("topleft", "d.", bty="n", cex = 1.5)
axis(side =2, at =c(seq(from=0,to=30000,by=5000)), labels = c(0,NA,"1",NA,"2",NA,"3"), cex.axis = 1.5, las = 1)
box(which = "plot")
axis(side =1, at =c(seq(from=1964,to=2011,by=2)), labels = c(NA,NA,NA,"1970",NA,NA,NA,NA,"1980",NA,NA,NA,NA,"1990",NA,NA,NA,NA,"2000",NA,NA,NA,NA,"2010"), cex.axis = 1.5)
dev.off()







#tiff(file="Figures/Fig_2_Weight-at-length_all.tiff", width = 3.34646, height = 4.015752, units = "in", pointsize = 10, family = "serif" )
pdf("Figures/Fig_2_Weight-at-length_all.pdf", onefile = FALSE, paper = "special", width = 3.34646, height = 4.015752, pointsize = 10, family = "serif" )
par( mar=c(3 , 3 , 0.5 , .5) , tcl=-.1 , mgp=c(1 ,  .5 ,  0) ,  oma=c(0.4 , 0 , 0 , 0.1))
layout(matrix(c(1:4), 2, 2, byrow = TRUE ))
par(xpd=F)


# Histogram of Fork Length

hist(~as.numeric(df.menhaden$fork_len), main=NA, xlab = NA , col ="white", cex.axis = 1, las =1, ylab = NA, yaxt = "n", xaxt= "n", xlim = c(150, 250))
axis(side = 1, at = seq (from = 150,to = 310, by = 50))
axis(side = 2, at = seq (from = 0,to = 80000, by = 10000), labels = c(0,NA,"2",NA,"4",NA,"6",NA,"8"), las = 1)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 1000, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 1000, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 1000, x1 = par("usr")[2], y1 = par("usr")[4] + 1000, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
legend("topleft", "a.", bty="n", inset = -0.05)
mtext("Fork length (mm)", side = 1 , line =2)
mtext("Frequency (x 10,000)", side = 2 , line =2)

par(xpd=F)
# Histogram of Weight
hist(~as.numeric(df.menhaden$weight), main=NA, xlab = NA , col ="white", cex.axis = 1, las =1, ylab = NA, yaxt = "n", xaxt= "n", xlim = c(50, 300))
axis(side = 1, at = seq (from = 50,to = 600, by = 50), labels = c(NA,100,NA,200,NA,300,NA,400,NA,500,NA,600))
axis(side = 2, at = seq (from = 0,to = 80000, by = 10000), labels = c(0,NA,"2",NA,"4",NA,"6",NA,"8"), las = 1)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 1000, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 1000, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 1000, x1 = par("usr")[2], y1 = par("usr")[4] + 1000, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
legend("topleft", "b.", bty="n", inset = -0.05)
mtext("Weight (g)", side = 1 , line =2)
mtext("Frequency (x 10,000)", side = 2 , line =2)


par(xpd = F)
# Plot weight-at-length
smoothScatter(x = df.menhaden$fork_len, y=df.menhaden$weight, xlab = NA, ylab= NA, colramp = colorRampPalette(c("white", 1)), las = 1, cex.axis = 1, xlim = c(150, 310))
axis(side = 1, at = seq (from = 150,to = 310, by = 50))
axis(side = 2, at = seq (from = 50,to = 600, by = 50), labels = NA)

curve(a.est * x ^ b.est , from = round(150), to = round(max(df.menhaden$fork_len)), add = T)
mtext( "Fork length (mm)" , side = 1 , line =2)
mtext( "Weight (g)" , side = 2 , line =2)
a.e <- round(a.est,8)
b.e <- round(b.est,2)
#legend("bottomright", c(expression(paste(italic('a'), " = ", "1.8e-05")), expression(paste(italic('b'), " = ", "3.03"))), bty = "n")
legend("topleft", "c.", bty="n", inset = -0.05)

# Histogram of relative condition

hist(~as.numeric(df.menhaden$relative.condition), main=NA, xlab = NA , col ="white", cex.axis = 1, las =1, ylab = NA, yaxt = "n", xaxt= "n")
axis(side = 1, at = seq (from = 60,to = 160, by = 20), labels = c(60,NA,100,NA,140,NA))
axis(side = 2, at = seq (from = 0,to = 60000, by = 10000), labels = c(0,NA,"2",NA,"4",NA,"6"), las = 1)
par(xpd=T)
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 1000, col =1) # left line
segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 1000, col =1) # right line
segments(x0 = par("usr")[1], y0= par("usr")[4] + 1000, x1 = par("usr")[2], y1 = par("usr")[4] + 1000, col =1) # top line
segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
legend("topleft", "d.", bty="n", inset = -0.05)
mtext(substitute(paste(italic("Kn" ))), side = 1 , line =2)
mtext("Frequency (x 10,000)", side = 2 , line =2)
dev.off()


