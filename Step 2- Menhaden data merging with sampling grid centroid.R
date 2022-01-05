

#####
##### Merging the Spatial Data
#####
#####I am merging with data with a 1/6 x 1/6 degree grid copied from the Beaufort Lab Grid
#####A location ID was given to each grid cell following the map and data
#####The grid was then cut to Global Self-consistent, Hierarchical, High-resolution Shoreline shapefile 
#####Remaining grid cells were then reconstructed back to the original 1/6 x 1/6 degree size and the centroid was calculated to give lat and long coordinates

data.frame.men <- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011.csv")

##change spatial data to be conforming
data.frame.men$location=gsub("A",1,data.frame.men$location)
data.frame.men$location=gsub("B",2,data.frame.men$location)
data.frame.men$location=gsub("C",3,data.frame.men$location)
data.frame.men$location=gsub("D",4,data.frame.men$location)
data.frame.men$location=gsub("E",5,data.frame.men$location)
data.frame.men$location=gsub("F",6,data.frame.men$location)
data.frame.men$location=replace(data.frame.men$location,data.frame.men$location=="",NA)


# Create grid and centroids
#raster.men.grid=raster(xmn=min(-95), xmx=max(-84), ymn=min(25), ymx=max(33), resolution =1/6, crs = NA)
#poly<- rasterToPolygons(raster.men.grid, na.rm=F)


men.spatial.data=read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\Digitized NMFS Sampling Grids\\Centroid of menhaden grid cut to GSHHS US coastline with location ID COMPLETE CASES.csv")
men.spatial.data<- men.spatial.data[, c(1,2, 16)]
data.frame.men1=merge(data.frame.men, men.spatial.data,by="location",all.x=T)
data.frame.men1$longitude=as.numeric(data.frame.men1$longitude)



write.csv(data.frame.men1,file="C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location.csv")
