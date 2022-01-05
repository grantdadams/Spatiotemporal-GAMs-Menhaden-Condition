library(mgcv)
library(fossil)
library(sp)
library(maptools)
library(Hmisc)
library(FSA)
library(rgdal)


load("Data/Gulf_Menhaden_Data.RData")
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag2) + s(latitude, longitude, by = monthly.v.wind.lag2), data = df.menhaden, method = "fREML")
model <- final_model

# Load state lines
state.lines <- readOGR(".", "state_lines")
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

##################################
# Check correlations with hypoxia
##################################
# hypo.dat <- read.csv("hypoxia_extent_data.csv")
# hypo.dat <- merge(d, hypo.dat, by = "year")
# summary(lm(coefficients ~ hypoxia_extent_sq_km, data = hypo.dat))
# 
# df.menhaden <- merge(df.menhaden, hypo.dat, by = "year")
# df.menhaden.hypo = df.menhaden[which(df.menhaden$month %in% c(6,7,8)),]
# 
# final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag0) + s(latitude, longitude, by = monthly.v.wind.lag1), data = df.menhaden, method = "fREML")
# df.menhaden$residuals <- resid(final_model)
# 
# df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,"hypoxia_extent_sq_km"]),]
# df.menhaden.hypo = df.menhaden.short[which(df.menhaden.short$month %in% c(6,7,8)),]
# 
# hypo.gam <- gam(residuals ~ s(hypoxia_extent_sq_km, k = 9), data = df.menhaden.hypo )
# resid.gam <- gam(residuals ~ 1, data = df.menhaden.hypo )
# plot(hypo.gam, rug = T, xlab = "Areal extent of the hypoxic zone (km2)", ylab = "Kn")


############
# Plot
############

tiff(file="Figures/Fig_4_GAM_Year_Month_Distribution_Plot.tiff" , height= 5.15/2,  width=3.34646 , pointsize=8 , family = "serif" , units = "in", res =300)
pdf("Figures/Fig_4_GAM_Year_Month_Distribution_Plot.pdf", onefile = FALSE, paper = "special", height= 5.15/2,  width=3.34646, pointsize = 8, family = "serif" )
nf <- layout(matrix(c(1,2,3,4,5,5,6,6,7,7) , 5 , 2 , byrow=TRUE) ,  heights=c(1 , .4 , .2 , 1.5,.2) , widths = c(1.885,1.885),  TRUE)
par(  oma=c(0 , 0 , 0 , 0))
par(xpd=F)

##################
# Figures
#################

par( mar=c(0 , 2.5 , .5 , .1))

# Year
plot(x = year, y = (coef), las = 1, type = "l",  xlab = NA, ylab= NA, xaxt = "n",  cex.axis = 1.5)
#arrows(year,  coef - sd, year , coef + sd, code = 3, length = .001, col = 1)
mtext(substitute(paste(italic("Kn" ))),side=2, adj = .5, line= 3)
legend("topleft", "a.", bty="n", cex = 1.5, inset = -0.05)

par( mar=c(0 , 3.2, .5 , 0.1))


# Month
plot(model , scale=0 , pch=19 , scheme=1 , col=1 , shade.col='gray90' ,  ylab=NA , select=1 , xlim=c(min(df.menhaden$month, na.rm=T) , max(df.menhaden$month, na.rm=T)), xlab = NA, rug =F, xaxt="n", las =1, cex.axis = 1.5)
legend("topleft", "b.", bty="n", cex = 1.5, inset = -0.05)
mtext(substitute(paste("Effect on ",italic("Kn" ))),side=2, adj = .5, line= 2)



############
#Histogram
############
# Year

par( mar=c(0 , 2.5 , .5 , .1))

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
par( mar=c(0 , 3.2, .5 , .1))
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
par(mar=c(0.1, 2.5 , 2 , .1))

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

