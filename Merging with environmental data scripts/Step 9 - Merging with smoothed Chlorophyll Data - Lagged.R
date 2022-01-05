setwd("C:/Users/w966213/Documents/Prospectus Chapter 1/R Scripts/Prospectus Chp 1 R Project")
raster.men.grid=raster(xmn=min(-95), xmx=max(-84), ymn=min(28), ymx=max(33), resolution =1/6, crs = NA)
poly<- rasterToPolygons(raster.men.grid, na.rm=F)
#coastline<- readShapePoly("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Shapefiles\\GSHHS_shp\\f\\GSHHS_f_L1.shp")
poly.cut<- crop(coastline, poly)

df.menhaden <- read.csv("df_menhaden_full5.csv")

setwd("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 6.csv")


chl.raster_data=list.files(path=getwd(), pattern= "MF_RASTA")
load(chl.raster_data[1])
names(masked.raster)<-substr(chl.raster_data[1],1,15)
chl.stack<- masked.raster
 for ( i in 2:length(chl.raster_data)){
   load(chl.raster_data[i])
   names(masked.raster)<-substr(chl.raster_data[i],1,15)
   chl.stack<- stack(chl.stack, masked.raster)
 }
chl.stack<- crop(chl.stack, extent(poly))
#chl.stack<- mask(chl.stack, poly.cut, inverse = T)
chl.extraction<-extract(chl.stack, poly, fun=mean, method="simple", na.rm=T, sp=T, weights=TRUE)

coordinates(df.menhaden)<- ~longitude+latitude

chl.data<- over(df.menhaden, chl.extraction, na.rm=T)






#### Lags


number.of.monthly.lags = 6

library(lubridate)
fish5 <- data.frame(df.menhaden)
fish5$month_date<- as.Date( paste( fish5$month, 1, fish5$year, sep = "."), format = "%m.%d.%Y")

for (i in 0:number.of.monthly.lags){
  chl.headers <- data.frame(chl.date=names(chl.stack))
  chl.headers$month_date<- seq(from = as.Date(strptime(substr(names(chl.stack)[1], 9,15),format = "%Y%j")) - months(i) , length.out = dim(chl.stack)[3], by="month") # First raster is from August 2008
  
  fish5<- merge(fish5, chl.headers, by= "month_date", all.x=T)
  fish6 <- subset(fish5, is.na(fish5$chl.date)==F)
  fish7 <- subset(fish5, is.na(fish5$chl.date)==T)
  
  # Non-scaled
  idx2<- match(fish6$chl.date, colnames(chl.data))
  fish6$smoothed.chl<- sapply( 1: nrow(fish6), function(i) chl.data[i, idx2[i]])
  names(fish6)[names(fish6)=="smoothed.chl"] <- paste("smoothed.chl.lag.", i, sep = "")
   
  fish6 <- subset(fish6, select = - chl.date)
  fish7 <- subset(fish7, select = - chl.date)
  #Scale the Data
  
  fish5 <- rbind.fill(fish6, fish7)
  
}

fish5 <- subset(fish5, select = - c(optional.2, remove, X , lwr, upr, fit, X.1,X.2,X.3,X.4,X.5,X.6,X.7, optional.4 ))
fish5<- data.frame(fish5)
setwd("C:/Users/w966213/Documents/Prospectus Chapter 1/R Scripts/Prospectus Chp 1 R Project")
write.csv(fish5, "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 7.csv")


