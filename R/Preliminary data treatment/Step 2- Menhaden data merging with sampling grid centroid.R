

#####
##### Merging the Spatial Data
#####
#####I am merging with data with a 1/6 x 1/6 degree grid copied from the Beaufort Lab Grid
#####A location ID was given to each grid cell following the map and data
#####The grid was then cut to Global Self-consistent, Hierarchical, High-resolution Shoreline shapefile 
#####Remaining grid cells were then reconstructed back to the original 1/6 x 1/6 degree size and the centroid was calculated to give lat and long coordinates

# data.frame.menhaden <- read.csv("Data\\NMFS Menhaden survey data set 1964-2011.csv")

##change spatial data to be conforming
data.frame.menhaden$location=gsub("A",1,data.frame.menhaden$location)
data.frame.menhaden$location=gsub("B",2,data.frame.menhaden$location)
data.frame.menhaden$location=gsub("C",3,data.frame.menhaden$location)
data.frame.menhaden$location=gsub("D",4,data.frame.menhaden$location)
data.frame.menhaden$location=gsub("E",5,data.frame.menhaden$location)
data.frame.menhaden$location=gsub("F",6,data.frame.menhaden$location)
data.frame.menhaden$location=replace(data.frame.menhaden$location,data.frame.menhaden$location=="",NA)


# Get spatial grid and centroids from QGIS calculations
men.spatial.data=read.csv("Data\\Digitized NMFS Sampling Grids\\Centroid of menhaden grid cut to GSHHS US coastline with location ID COMPLETE CASES.csv")
men.spatial.data<- men.spatial.data[, c(1,2, 16)]
data.frame.menhaden=merge(data.frame.menhaden, men.spatial.data,by="location",all.x=T)
data.frame.menhaden$longitude=as.numeric(data.frame.menhaden$longitude)



# write.csv(data.frame.menhaden,file="C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location.csv")
