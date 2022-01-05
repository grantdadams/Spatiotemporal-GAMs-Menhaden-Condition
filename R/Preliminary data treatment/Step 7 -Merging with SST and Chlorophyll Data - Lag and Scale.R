# This file takes raster CHL and SST data and merges it with XY data.

# data.frame.menhaden <- read.csv("Data\\NMFS Menhaden survey data set 1964-2011 with location step 6.csv")

raster.men.grid=raster(xmn=min(-95), xmx=max(-84), ymn=min(28), ymx=max(33), resolution =1/6, crs = NA)
poly<- rasterToPolygons(raster.men.grid, na.rm=F)
poly.cut<- crop(coastline, poly)

# - Load CHL and SST data
chl_dir <- "Data/OceanColor Data/Aqua Modis Chlorophyll Monthly 9km"

chl.raster_data=list.files(path=chl_dir, pattern= "chlor")
chl.stack<- stack(chl.raster_data)
chl.stack<- crop(chl.stack, extent(poly))
#chl.stack<- mask(chl.stack, poly.cut, inverse = T)
chl.extraction<-extract(chl.stack, poly, fun=mean, method="simple", na.rm=T, sp=T, weights=TRUE)


sst_dir <- "Data/OceanColor Data/Aqua Modis SST Monthly 9km"
sst.raster_data=list.files(path=sst_dir, pattern = "SST_9")
sst.stack<- stack(sst.raster_data)
sst.stack<- crop(sst.stack, extent(poly))
#sst.stack<- mask(sst.stack, poly.cut, inverse = T)
sst.stack[which(sst.stack[]<5)] <- NA # Remove extreme values
sst.extraction<-extract(sst.stack, poly, fun=mean, method="simple", na.rm=T, sp=T, weights=TRUE)


# - Merge data
coordinates(data.frame.menhaden)<- ~longitude+latitude
chl.data<- over(data.frame.menhaden, chl.extraction, na.rm=T)
sst.data<- over(data.frame.menhaden, sst.extraction, na.rm=T)






# - Set up data lags
number.of.monthly.lags = 6

library(lubridate)
library(plyr)
fish5 <- data.frame(data.frame.menhaden)
fish5$month_date<- as.Date( paste( fish5$month, 1, fish5$year, sep = "."), format = "%m.%d.%Y")

for (i in 0:number.of.monthly.lags){
  chl.headers <- data.frame(chl.date=names(chl.stack))
  sst.headers <- data.frame(sst.date=names(sst.stack))
  chl.headers$month_date<- seq(from = as.Date("2002-08-01") - months(i) , length.out = dim(sst.stack)[3], by="month") # First raster is from August 2008
  sst.headers$month_date<- seq(from = as.Date("2002-08-01") - months(i), length.out = dim(sst.stack)[3], by="month") # First raster is from August 2008
  
  fish5<- merge(fish5, chl.headers, by= "month_date", all.x=T)
  fish5<- merge(fish5, sst.headers, by= "month_date", all.x=T)
  fish6 <- subset(fish5, is.na(fish5$sst.date)==F)
  fish7 <- subset(fish5, is.na(fish5$sst.date)==T)
  
  # Non-scaled
  idx2<- match(fish6$chl.date, colnames(chl.data))
  fish6$new.chl<- sapply( 1: nrow(fish6), function(i) chl.data[i, idx2[i]])
  names(fish6)[names(fish6)=="new.chl"] <- paste("chl_9km_lag_", i, sep = "")
  
  idx<- match(fish6$sst.date, colnames(sst.data))
  fish6$new.sst<- sapply( 1: nrow(fish6), function(i) sst.data[i, idx[i]])
  names(fish6)[names(fish6)=="new.sst"] <- paste("SST_9km_lag_", i, sep = "")
   
  fish6 <- subset(fish6, select = - chl.date)
  fish6 <- subset(fish6, select = - sst.date)
  fish7 <- subset(fish7, select = - chl.date)
  fish7 <- subset(fish7, select = - sst.date)
  #Scale the Data
  
  fish6$scaled.chl<- scale(log(fish6[, which(names(fish6) == paste("chl_9km_lag_", i, sep = ""))] +1))  
  names(fish6)[names(fish6)=="scaled.chl"] <- paste("log_+1_chl_9km_scaled_lag_", i, sep = "")
  
  fish6$sst.scaled<- scale( fish6[, which(names(fish6) == paste("SST_9km_lag_", i, sep = ""))])
  names(fish6)[names(fish6)=="sst.scaled"] <- paste("SST_9km_scaled_lag_", i, sep = "")
  
  fish5 <- rbind.fill(fish6, fish7)
  
}

data.frame.menhaden <- fish5
# write.csv(data.frame.menhaden, "Data\\NMFS Menhaden survey data set 1964-2011 with location step 7")



