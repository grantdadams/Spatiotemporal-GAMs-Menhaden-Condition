
library(raster)
library(maptools)


raster.men.grid=raster(xmn=min(-95), xmx=max(-84), ymn=min(28), ymx=max(33), resolution =1/6, crs = NA)
poly<- rasterToPolygons(raster.men.grid, na.rm=F)
# 
# shapefile(poly, "C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\Menhaden gulfcoast grid large.shp", overwrite=T)



coastline<- readShapePoly("C:\\Users\\w966213\\Documents\\GIS Shapefiles\\US Shapefiles\\GSHHS_shp\\f\\GSHHS_f_L1.shp")
poly.cut<- crop(coastline, poly)




setwd("C:/Users/w966213/Documents/OceanColor Data/Aqua Modis SST Annual 9km")

sst.raster_data=list.files(path=getwd(), pattern = "SST")
sst.stack<- stack(sst.raster_data)
sst.stack<- crop(sst.stack, extent(poly))
sst.extraction<-extract(sst.stack, poly, fun=mean, method="simple", na.rm=T, sp=T, weights=TRUE)
f


coordinates(df.menhaden)<- ~longitude+latitude
fish.data.sst<- over(df.menhaden, sst.extraction)
fish4<- cbind(df.menhaden, fish.data.sst)