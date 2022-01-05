#################
# Data Load
#################

load("C:/Users/w966213/Documents/Prospectus Chapter 1/R Scripts/Prospectus Chp 1 R Project/Adams_g_September_15_2016_full_data_frame_for_mgcv_bug.RData")

library(lubridate)
library(FSA)
df.menhaden$julian_day <- yday(df.menhaden$date)
#df.menhaden$season <- ifelse(df.menhaden$julian_day < 180, "spring", "fall" )
df.menhaden$Julian <- julian(as.Date(df.menhaden$date))

df.menhaden <- df.menhaden[with(df.menhaden, order(date)), ]
df.menhaden.short<- df.menhaden[which(df.menhaden$year>=2003),]
df.menhaden$cohort <- df.menhaden$year - df.menhaden$age
df.menhaden <- df.menhaden[which(!is.na(df.menhaden$cohort)),]


#####################
# Final Models
#####################
#source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")
library(qpcR)
library(mgcv)

base.bam=bam(relative.condition~ s(longitude,latitude) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log"))

base.bam.short=bam(relative.condition~ s(longitude,latitude) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log"))

river.bam.vc.lag.0=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log"))

mei.bam.lag.10=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.10) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log") )

wind.bam.vc.lag.1=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log"))

sst.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_0) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log"))

chl.bam.vc.lag.5=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log"))

#########################
# Diagnostics
#########################
library(geoR)
library(maptools)
library(raster)
library(sp)
library(itsadug)
library(gstat)

# Put coordinates into meters
coords = data.frame(longitude = df.menhaden$longitude, latitude = df.menhaden$latitude)
coordinates(coords)= ~longitude+latitude
degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") # 
meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
crs(coords)<-crs(degree.crs) # Set CRS 
coords<-spTransform(coords, crs(meters.crs)) # Convert CRS to meters based
coords <- data.frame(coords)


coords.short = data.frame(longitude = df.menhaden.short$longitude, latitude = df.menhaden.short$latitude)
coordinates(coords.short)= ~longitude+latitude
degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") # 
meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")
crs(coords.short)<-crs(degree.crs) # Set CRS 
coords.short<-spTransform(coords.short, crs(meters.crs)) # Convert CRS to meters based
coords.short <- data.frame(coords.short)


###################
# Calculate VG
###################

# Base 1964-2011
model <- base.bam
gb <- data.frame(data = residuals(model, type = "d"), coords = coords)
coordinates(gb) = ~coords.longitude+coords.latitude
bvg <- gstat::variogram(data~1, gb)

# Base 2003-2011
model <- base.bam.short
gb <- data.frame(data = residuals(model, type = "d"), coords = coords.short)
coordinates(gb) = ~coords.longitude+coords.latitude
bsvg <- gstat::variogram(data~1, gb)

#River
model <- river.bam.vc.lag.0
gb <- data.frame(data = residuals(model, type = "d"), coords = coords)
coordinates(gb) = ~coords.longitude+coords.latitude
rvg <- gstat::variogram(data~1, gb)

# ENSO
model <- mei.bam.lag.10
gb <- data.frame(data = residuals(model, type = "d"), coords = coords)
coordinates(gb) = ~coords.longitude+coords.latitude
ensovg <- gstat::variogram(data~1, gb)

# Wind
model <- wind.bam.vc.lag.1
gb <- data.frame(data = residuals(model, type = "d"), coords = coords)
coordinates(gb) = ~coords.longitude+coords.latitude
wvg <- gstat::variogram(data~1, gb)

# SST
model <- sst.bam.lag.0
gb <- data.frame(data = residuals(model, type = "d"), coords = coords.short)
coordinates(gb) = ~coords.longitude+coords.latitude
sstvg <- gstat::variogram(data~1, gb)

# CHL
model <- chl.bam.vc.lag.5
gb <- data.frame(data = residuals(model, type = "d"), coords = coords.short)
coordinates(gb) = ~coords.longitude+coords.latitude
chlvg <- gstat::variogram(data~1, gb)


##################################################
# Plot it ########################################
##################################################


tiff(file="GAMM_Spatial_Autocorrelation.tiff" , height= 4,  width=7 , pointsize=10 , family = "serif" , res =600, units = "in")
#### River\
nf <- layout(matrix(c(1:12) , 3 , 4 , byrow=TRUE) , widths = c(1,1,1,1), heights = c(.8,.8,.2),  TRUE)
par( mar=c(3 , 5 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))

# Base 1964-2011
vg <- bvg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.008))
mtext("Base GAMM", side=2,line = 4.5, cex = .75)
mtext("(1964-2011)", side=2,line = 3.5, cex = .75)

# Base 2003-2011
vg <-bsvg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.006))
mtext("Base GAMM", side=2,line = 4.5, cex = .75)
mtext("(2003-2011)", side=2,line = 3.5, cex = .75)

#River
vg <- rvg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.008))
mtext("River discharge GAMM", side=2,line = 4.5, cex = .75)
mtext("(1964-2011)", side=2,line = 3.5, cex = .75)

# ENSO
vg <-ensovg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.008))
mtext("ENSO GAMM", side=2,line = 4.5, cex = .75)
mtext("(1964-2011)", side=2,line = 3.5, cex = .75)

# Wind
vg <- wvg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.008))
mtext("Wind GAMM", side=2,line = 4.5, cex = .75)
mtext("(1964-2011)", side=2,line = 3.5, cex = .75)

# SST
vg <- sstvg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.006))
mtext("SST GAMM", side=2,line = 4.5, cex = .75)
mtext("(1964-2011)", side=2,line = 3.5, cex = .75)


# CHL
vg <-chlvg
vg <- as.data.frame(vg)
plot(vg$dist,vg$gamma, xlab = "Distance (m)", ylab = "Semivariance", type = "l", ylim = c(0, 0.006))
mtext("Chlorophyll GAMM", side=2,line = 4.5, cex = .75)
mtext("(2003-2011)", side=2,line = 3.5, cex = .75)


dev.off()