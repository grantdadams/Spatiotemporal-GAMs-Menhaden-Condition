# This script takes monthly u and v wind rasters converted from netCDF files using the "wind_ncdf_2_full_raster_2.0" script and merges them with a data set. The first step takes the raster files and converts them into a data.frame with each row corresponding to a specific month. The second step takes the weighted average of the u and v wind data for the sampling cells for each month and adds it to your dataset. Additionally it lags the wind data.

# Inputs: Data set and the wind data. Additionally a sampling grid needs to be specified in the set-up section.

########
# Set up
########
# Load data
df.menhaden<- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 5.csv")

# Define sampling grid
max.latidude <- 33
min.latitude <- 28
max.longitude <- -84
min.longitude <- -95
cell.resolution <- 1/6

# Set the number of monthly lags
number.of.lags = 6

library(matlab)
library(raster)
library(sp)
library(lubridate)
library(chron)


######################################
# Step 1: Convert raster to data.frame
######################################


#########
# U-wind
#########

setwd("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\NCEP_NCAR_wind_monthly\\FULL_NC.UWND.Monthly")
e <- extent(-98, -82, 26, 34)
file.name <- dir(pattern = "RAST")
u.wind.data <- data.frame(matrix(NA,nrow = length(dir(pattern = "RAST")), ncol=38))
for (j in 1:length(file.name)) {
  matlab::tic()
  load(dir(pattern = "RAST")[j])
  u.wind.data[j,1] <- month.day.year(getZ(masked.raster))$year
  u.wind.data[j,2] <- month.day.year(getZ(masked.raster))$month
  masked.raster<- crop(masked.raster, e)
  u.wind.data[j,2+(1:ncell(masked.raster))] <- masked.raster[1:ncell(masked.raster)]
  print(j)
  matlab::toc()
}

for( k in (3+ncell(masked.raster)) : (2+ (ncell(masked.raster)*2)))   {
  u.wind.data[,k]<-scale(u.wind.data[,k-ncell(masked.raster)], center = mean(rowMeans(u.wind.data[,3:(3+ncell(masked.raster))], na.rm=T)))
}

colnames(u.wind.data)<-c("year","month",1:ncell(masked.raster), paste("scaled", 1:ncell(masked.raster), sep = ""))


#########
# V-wind
#########
setwd("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\NCEP_NCAR_wind_monthly\\FULL_NC.VWND.Monthly")
e <- extent(-98, -82, 26, 34)
file.name <- dir(pattern = "RAST")
v.wind.data <- data.frame(matrix(NA,nrow = length(dir(pattern = "RAST")), ncol=38))
for (j in 1:length(file.name)) {
  matlab::tic()
  load(dir(pattern = "RAST")[j])
  v.wind.data[j,1] <- month.day.year(getZ(masked.raster))$year
  v.wind.data[j,2] <- month.day.year(getZ(masked.raster))$month
  masked.raster<- crop(masked.raster, e)
  v.wind.data[j,2+(1:ncell(masked.raster))] <- masked.raster[1:ncell(masked.raster)]
  print(j)
  matlab::toc()
}

for( k in (3+ncell(masked.raster)) : (2+ (ncell(masked.raster)*2))){ 
  v.wind.data[,k]<-scale(v.wind.data[,k-ncell(masked.raster)],  center = mean(rowMeans(u.wind.data[,3:(3+ncell(masked.raster))], na.rm=T)))
}

colnames(v.wind.data)<-c("year","month",1:ncell(masked.raster), paste("scaled", 1:ncell(masked.raster), sep = ""))



######################################
# Step 2: Merge wind data with dataset
######################################
df.menhaden$monthly.u.wind.scaled <- rep(NA,times = dim(df.menhaden)[1]) # Create empty columns to fill
df.menhaden$monthly.v.wind.scaled <- rep(NA,times = dim(df.menhaden)[1])

coordinates(df.menhaden)<- ~longitude+latitude # Turn data.frame into SpatialPointsDataFrame

# Create a SpatialPolygons file for sampling grid
sampling.grid <- raster(xmn=min(min.longitude), xmx=max(max.longitude), ymn=min(min.latitude), ymx=max(max.latidude), resolution = cell.resolution, crs=NA)
poly2<- rasterToPolygons(sampling.grid, na.rm=F )

# Create an index for the wind cell # and weights in that overlap the sampling grid
ind.r<- masked.raster
ind.r[1:ncell(ind.r)]<- 1:ncell(ind.r) # Create a cell number index for the wind rasters
ex.list<- extract(ind.r, poly2, weights=T) # Extract which cell numbers and the weight fall into the individual sampling grids


require(matlab)
for ( k in 0:number.of.lags){
  df.menhaden$monthly.u.wind <- rep(NA,times = dim(df.menhaden)[1])
  df.menhaden$monthly.v.wind <- rep(NA,times = dim(df.menhaden)[1])
  men.jul <- julian(as.numeric(df.menhaden$month),1,as.numeric(df.menhaden$year), c(1,0,1800)) 
  u.wind.jul <- julian(as.numeric((u.wind.data$month) - k),1,as.numeric(u.wind.data$year), c(1,0,1800))
  for ( i in which(u.wind.jul %in% men.jul))  {
    tic()
    ind.date <- which(men.jul == u.wind.jul[i]) # Index of rows for date i
    ind.cell <- cellFromXY(raster.men.grid,df.menhaden[ind.date,]) # The cells of the sampling grid from which data from date i fall
    wind.ind <- ex.list[ind.cell] # Index the wind cells using the sampling grid cells
    weights <- c()
    cells <- c()
    for (j in 1: length(wind.ind)){
      cell.ind <- wind.ind[[j]][,1]
      weight.ind <- wind.ind[[j]][,2]
      length(cell.ind) <- 4
      length(weight.ind) <- 4
      cells<- rbind(cells, cell.ind)
      weights <- rbind(weights, weight.ind)
    }
    
    
    weighted.u.wind.values=c()
    scaled.weighted.u.wind.values=c()
    weighted.v.wind.values=c()
    scaled.weighted.v.wind.values=c()
    
    for( z in 1: dim(weights)[1]){
      # U-wind
      weighted.u.wind.ind <- weighted.mean( (u.wind.data[i, na.omit(cells[z,])+2 ]) , na.omit(weights[z,]))
      scaled.u.wind <- weighted.mean( (u.wind.data[i, na.omit(cells[z,]) +2 + ncell(masked.raster) ]) , na.omit(weights[z,]))
      weighted.u.wind.values <- rbind( weighted.u.wind.values, weighted.u.wind.ind)
      scaled.weighted.u.wind.values <- rbind( scaled.weighted.u.wind.values, scaled.u.wind )
      
      #V-wind
      weighted.v.wind.ind <- weighted.mean( (v.wind.data[i, na.omit(cells[z,])+2 ]) , na.omit(weights[z,]))
      scaled.v.wind <- weighted.mean( (v.wind.data[i, na.omit(cells[z,]) +2 + ncell(masked.raster) ]) , na.omit(weights[z,]))
      weighted.v.wind.values <- rbind( weighted.v.wind.values, weighted.v.wind.ind)
      scaled.weighted.v.wind.values <- rbind( scaled.weighted.v.wind.values, scaled.v.wind )
      
    }

    df.menhaden$monthly.u.wind[ind.date] <- weighted.u.wind.values
    df.menhaden$monthly.v.wind[ind.date] <- weighted.v.wind.values
    toc()
    
  }
  df.menhaden$monthly.v.wind.scaled <- scale(df.menhaden$monthly.v.wind)
  df.menhaden$monthly.u.wind.scaled <- scale(df.menhaden$monthly.u.wind)
  names(df.menhaden)[names(df.menhaden)=="monthly.u.wind"] <- paste("monthly.u.wind.lag", k, sep = "")
  names(df.menhaden)[names(df.menhaden)=="monthly.u.wind.scaled"] <- paste("monthly.u.wind.scaled.lag", k, sep = "")
  names(df.menhaden)[names(df.menhaden)=="monthly.v.wind"] <- paste("monthly.v.wind.lag", k, sep = "")
  names(df.menhaden)[names(df.menhaden)=="monthly.v.wind.scaled"] <- paste("monthly.v.wind.scaled.lag", k, sep = "")
} 
  
df.menhaden<-data.frame(df.menhaden)

write.csv(df.menhaden, "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 6.csv")


##### Non-weighted values

# 
# 
# df.menhaden$monthly.u.wind<-NA
# df.menhaden$monthly.u.wind.scaled<-NA
# 
# df.menhaden$monthly.v.wind <- NA
# df.menhaden$monthly.v.wind.scaled <- NA
# 
# for ( i in 1:length(df.menhaden))  {
#   df.menhaden$monthly.u.wind[i] <- u.wind.data[ which( u.wind.data$year == df.menhaden$year[i] & u.wind.data$month == df.menhaden$month[i] ) , as.character( cellFromXY( masked.raster, df.menhaden[i,]))] # takes values from the row of the wind data frame which has the year and month of the menhaden data and the column which corresponds to the raster cell number in which the menhaden sampling location falls
#   df.menhaden$monthly.u.wind.scaled[i] <- u.wind.data[ which( u.wind.data$year == df.menhaden$year[i] & u.wind.data$month == df.menhaden$month[i]) , paste( "scaled", cellFromXY( masked.raster, df.menhaden[i,] ), sep = "")]
#   
#   df.menhaden$monthly.v.wind[i] <- v.wind.data[ which(v.wind.data$year==df.menhaden$year[i] & v.wind.data$month==df.menhaden$month[i]) , as.character(cellFromXY(masked.raster,df.menhaden[i,]))]
#   df.menhaden$monthly.v.wind.scaled[i] <- v.wind.data[ which(v.wind.data$year==df.menhaden$year[i] & v.wind.data$month==df.menhaden$month[i]) , paste("scaled", cellFromXY(masked.raster,df.menhaden[i,]), sep = "")]
#   
#   
#   
#   print(i)
#   
# }

