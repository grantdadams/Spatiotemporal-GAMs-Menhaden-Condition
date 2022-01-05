
# #####
# ##### 
# #####
data.frame.menhaden=read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location.csv")
data.frame.menhaden$date=as.Date(data.frame.menhaden$date)
fish <- data.frame.menhaden
#####
##### Removal of Spurious Data
#####

##### No length or weight data
data.frame.menhaden <- data.frame.menhaden[which(!is.na(data.frame.menhaden$weight)),]
data.frame.menhaden <- data.frame.menhaden[which(!is.na(data.frame.menhaden$fork_len)),]

##### Remove all fish from land
data.frame.menhaden<- data.frame.menhaden[complete.cases(data.frame.menhaden[,c("latitude","longitude")]),]

##### Remove data outside the study area
data.frame.menhaden <- subset(data.frame.menhaden,data.frame.menhaden$latitude<35 & data.frame.menhaden$latitude>28.5 & data.frame.menhaden$longitude>-95 & data.frame.menhaden$longitude< -84)

#Subset Lake Ponchatrain
library(dplyr)
df.menhaden.out <- subset(data.frame.menhaden, data.frame.menhaden$longitude < -90 - (1/6) & data.frame.menhaden$latitude > 30 )
df.menhaden.out2 <- rbind(df.menhaden.out, subset(data.frame.menhaden, data.frame.menhaden$longitude < -90  & data.frame.menhaden$latitude < (29 - (3/6) )))
data.frame.menhaden <- anti_join(data.frame.menhaden, df.menhaden.out2)


data.frame.menhaden$fork_len=as.numeric(data.frame.menhaden$fork_len)
data.frame.menhaden$weight=as.numeric(data.frame.menhaden$weight)

##### Remove samples older than age 10
data.frame.menhaden<- subset(data.frame.menhaden, data.frame.menhaden$age<10)

##### Remove samples weighing less than one gram
data.frame.menhaden<-subset(data.frame.menhaden, data.frame.menhaden$weight>1 )

##### Remove samples longer than 500 m
data.frame.menhaden<-subset(data.frame.menhaden, data.frame.menhaden$fork_len<500)

##########
# Remove by Depth
##########

fish<-data.frame.menhaden

library(raster)
library(maptools)

# Create study area polygon
raster.men.grid=raster(xmn=min(-95), xmx=max(-84), ymn=min(25), ymx=max(33), resolution =1/6, crs = NA)
poly<- rasterToPolygons(raster.men.grid, na.rm=F)

# Coastline
coastline<- readShapePoly("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Shapefiles\\GSHHS_shp\\f\\GSHHS_f_L1.shp")
poly.cut<- crop(coastline, poly)

# Depth data
depth<-raster("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\GEBCO depth data\\RN-1122_1450372174253\\GEBCO2014_-98.9743_17.5321_-78.7179_31.9552_30Sec_Geotiff.tif")

depth.cropped<- crop(depth, poly)
depth.masked<- mask(depth.cropped, poly.cut, inverse = T)
depth.extraction<-extract(depth.masked, poly, fun=mean, method="simple", na.rm=T, sp=T, weights=TRUE)

coordinates(data.frame.menhaden)<- ~ longitude+latitude

depth.data<- over(data.frame.menhaden, depth.extraction)
data.frame.menhaden<- cbind(data.frame(data.frame.menhaden), depth.data)

# Remove fish from depths over 100 m
data.frame.menhaden<-subset(data.frame.menhaden, data.frame.menhaden$GEBCO2014_.98.9743_17.5321_.78.7179_31.9552_30Sec_Geotiff>-100)
colnames(data.frame.menhaden)[which(colnames(data.frame.menhaden)=="GEBCO2014_.98.9743_17.5321_.78.7179_31.9552_30Sec_Geotiff")]<-"depth"

write.csv(data.frame.menhaden, file= "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 3.csv" )


