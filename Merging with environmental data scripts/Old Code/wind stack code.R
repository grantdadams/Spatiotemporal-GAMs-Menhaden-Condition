# This fuction takes monthly and daily u-wind and v-wind raster data and returns and cuts it to a desired extend and creates a data frame of the cell values 



# Extract monthly values of u.wind  for the given study area extent

setwd("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\Monthly Wind data\\FULL_NC.UWND.Monthly")
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

for( k in 21:38){
  u.wind.data[,k]<-scale(u.wind.data[,k-ncell(masked.raster)])
}

colnames(u.wind.data)<-c("year","month",1:ncell(masked.raster), paste("scaled", 1:ncell(masked.raster), sep = ""))



###### Set up the v-wind
setwd("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\Monthly Wind data\\FULL_NC.VWND.Monthly")
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

for( k in 21:38){
  v.wind.data[,k]<-scale(v.wind.data[,k-ncell(masked.raster)])
}

colnames(v.wind.data)<-c("year","month",1:ncell(masked.raster), paste("scaled", 1:ncell(masked.raster), sep = ""))




##### Merge the data over

df.menhaden<- read.csv("C:/Users/w966213/Documents/Mehaden Data/menhaden_data_with_atch_location_MODIS_Aqua_SST_CHL_9km_depth.csv")
coordinates(df.menhaden)<- ~longitude+latitude

df.menhaden$monthly.u.wind<-NA
df.menhaden$monthly.u.wind.scaled<-NA

df.menhaden$monthly.v.wind <- NA
df.menhaden$monthly.v.wind.scaled <- NA

for ( i in 1:length(df.menhaden))  {
  df.menhaden$monthly.u.wind[i] <- u.wind.data[ which( u.wind.data$year == df.menhaden$year[i] & u.wind.data$month == df.menhaden$month[i] ) , as.character( cellFromXY( masked.raster, df.menhaden[i,]))] # takes values from the row of the wind data frame which has the year and month of the menhaden data and the column which corresponds to the raster cell number in which the menhaden sampling location falls
  df.menhaden$monthly.u.wind.scaled[i] <- u.wind.data[ which( u.wind.data$year == df.menhaden$year[i] & u.wind.data$month == df.menhaden$month[i]) , paste( "scaled", cellFromXY( masked.raster, df.menhaden[i,] ), sep = "")]
  
  df.menhaden$monthly.v.wind[i] <- v.wind.data[ which(v.wind.data$year==df.menhaden$year[i] & v.wind.data$month==df.menhaden$month[i]) , as.character(cellFromXY(masked.raster,df.menhaden[i,]))]
  df.menhaden$monthly.v.wind.scaled[i] <- v.wind.data[ which(v.wind.data$year==df.menhaden$year[i] & v.wind.data$month==df.menhaden$month[i]) , paste("scaled", cellFromXY(masked.raster,df.menhaden[i,]), sep = "")]
  
  
  
  print(i)
  
}


















#Daily wind vectors

setwd("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\FULL_NC_WND_1DAY\\FULL_NC.VWND.1DAY\\")

e <- extent(-92, -82, 26, 34)
file.name <- dir(pattern = "RAST")
v.wind.data <- matrix(NA,nrow = length(dir(pattern = "RAST")), ncol=16)
for (j in 1:length(file.name)) {
  matlab::tic()
  v.wind.data[j,1] <- as.numeric(substr(file.name[j],start = 11, stop = 14))
  v.wind.data[j,2] <- as.numeric(substr(file.name[j],start = 16, stop = 17))
  v.wind.data[j,3] <- as.numeric(substr(file.name[j],start = 19, stop = 20))
  v.wind.data[j,4] <- as.numeric(substr(file.name[j],start = 35, stop = 43))
  load(dir(pattern = "RAST")[i])
  masked.raster<- crop(masked.raster, e)
  closeAllConnections()
  v.wind.data[j,5:ncell(masked.raster)] <- masked.raster[5:ncell(masked.raster)]
  print(j)
  matlab::toc()
}



write.csv(data.frame(v.wind.data), file = "C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\FULL_NC_WND_1DAY\\FULL_NC.UWND.1DAY\\v_wind_data.csv")


 ##### Attempting to work it by combining both folders
# 
# raster.wd <- setwd("C:\\Users\\w966213\\Documents\\NGOM Oceanographic Data\\Monthly Wind data\\FULL_NC.UWND.Monthly")
# raster.vect.1 <- dir(netCDF.wd, pattern = "RAST")
# raster.vect.uwnd <- dir(netCDF.wd, pattern = "UWND")
# raster.vect.vwnd <- dir(netCDF.wd, pattern = "VWND")
# 
# ind <- c()
# ind[[1]] <- intersect(raster.vect.1,raster.vect.uwnd)
# ind[[2]] <- intersect(raster.vect.1,raster.vect.vwnd) 
# ind.name <- c("uwnd","vwnd")
# 
# for (z in 1:length(ind)) {
#   raster.file.path <- paste(raster.wd,ind[[z]],sep = "\\")
#   for (k in 1:length(raster.file.path)) {
#     load(raster.file.path[k]) 
#     




# e <- extent(-95, -84, 28.5, 31)
# u.wind.stack<-stack()
# file.name <- dir(pattern = "RAST")
# meta.data <- matrix(NA,length(file.name),4)
# for (j in 1:length(file.name)) {
#   matlab::tic()
#   meta.data[j,1] <- as.numeric(substr(file.name[j],start = 11, stop = 14))
#   meta.data[j,2] <- as.numeric(substr(file.name[j],start = 16, stop = 17))
#   meta.data[j,3] <- as.numeric(substr(file.name[j],start = 19, stop = 20))
#   meta.data[j,4] <- as.numeric(substr(file.name[j],start = 35, stop = 43))
#   load(file.name[j])
#   masked.raster<- crop(masked.raster, e)
#   u.wind.stack <- stack(u.wind.stack, masked.raster)
#   print(j)
#   matlab::toc()
# }
# 
# save.name <- paste(dir.name,paste("RAST","STACK",paste(date.code,"NC",toupper(ind.name[z]),"1DAY",jul.code,"RData",sep = "."),sep = "_"),sep = "\\")
# save(masked.raster,file = save.name)

