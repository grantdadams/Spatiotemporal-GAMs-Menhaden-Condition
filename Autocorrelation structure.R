check <- start_event(df.menhaden, column = "date", event = c("Julian"),label.event = "Event")
library(mgcv)
river.bam.vc.lag.0=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log"))

library(plyr) 

resid <- resid(river.bam.vc.lag.0)
x <- cbind(resid, df.menhaden)
x <- x[,c("week","date","year","resid","month","relative.condition","longitude")]

d <- ddply(x,~date,summarise, mean=mean(resid) ,sd = sd(resid))
d$scaled <- d$mean / d$sd
acf(d$mean, ci =F, main = NA, lag.max = 30)

d$date <- as.Date(d$date)
min(x$date)

dat1 <-data.frame(date = seq(from=(min(as.Date(d$date))), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d1 <- merge(dat1, d, by = "date")
acf1 <- acf(d1$mean, ci =F, main = NA, lag.max = 15, plot = F)

dat2 <-data.frame(date = seq(from=(min(as.Date(d$date)+1)), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d2 <- merge(dat2, d, by = "date")
acf2 <- acf(d2$mean, ci =F, main = NA, lag.max = 15, plot = F)

dat3 <-data.frame(date = seq(from=(min(as.Date(d$date)+2)), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d3 <- merge(dat3, d, by = "date")
acf3 <- acf(d3$mean, ci =F, main = NA, lag.max = 15, plot = F)

dat4 <-data.frame(date = seq(from=(min(as.Date(d$date)+3)), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d4 <- merge(dat3, d, by = "date")
acf4 <- acf(d4$mean, ci =F, main = NA, lag.max = 15, plot = F)

dat5 <-data.frame(date = seq(from=(min(as.Date(d$date)+4)), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d5 <- merge(dat5, d, by = "date")
acf5 <- acf(d5$mean, ci =F, main = NA, lag.max = 15, plot = F)

dat6 <-data.frame(date = seq(from=(min(as.Date(d$date)+5)), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d6 <- merge(dat6, d, by = "date")
acf6 <- acf(d6$mean, ci =F, main = NA, lag.max = 15, plot = F)

dat7 <-data.frame(date = seq(from=(min(as.Date(d$date)+6)), to=(max(as.Date(d$date))), by = 7), nothing = NA)
d7 <- merge(dat7, d, by = "date")
acf7 <- acf(d7$mean, ci =F, main = NA, lag.max = 15, plot = F)

acf.dat <- data.frame(acf1 = (acf1$acf), acf2 =(acf2$acf), acf3 = (acf3$acf), acf4 =(acf4$acf), acf5 = (acf5$acf), acf6 =(acf6$acf), acf7 = (acf7$acf))

plot(acf1$acf[2:16], type = "n", ylim = c(-.3,.3), xlab = "Lag", ylab = "rho")
for ( i in 1:7){
  lines(acf.dat[2:16,i], col = 11, lwd = 2)
}
lines(rowMeans(acf.dat[2:16,]), lwd= 5, col = 1, lty =2)
abline(h=0, )



####### Pacf
acf.dat <- data.frame(acf1 = (acf1$acf), acf2 =(acf2$acf), acf3 = (acf3$acf), acf4 =(acf4$acf), acf5 = (acf5$acf), acf6 =(acf6$acf), acf7 = (acf7$acf))

plot(acf1$acf[2:16], type = "n", ylim = c(-.3,.3), xlab = "Lag", ylab = "rho")
for ( i in 1:7){
  lines(acf.dat[2:16,i], col = 11, lwd = 2)
}
lines(rowMeans(acf.dat[2:16,]), lwd= 5, col = 1, lty =2)
abline(h=0, )




lines(acf1$acf)
lines(acf2$acf)
lines(acf3$acf)
lines(acf4$acf)
lines(acf5$acf)
lines(acf6$acf)
lines(acf7$acf)

