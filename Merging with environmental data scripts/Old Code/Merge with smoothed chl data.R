
df.menhaden <- read.csv("C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 6.csv")

# Load Smoothed Raster Files
setwd("C:\\Users\\w966213\\Documents\\OceanColor Data\\Aqua Modis Chlorophyll Monthly 9km\\MF_Aqua Modis Chlorophyll Monthly 9km_03\\")




file.name <- dir(pattern = "MF_NA")
file.name <- sort(file.name)
load(file.name[1])
u.wind.data <- data.frame(matrix(NA,nrow = length(file.name), ncol=(ncell(masked.raster)+2)))
for (j in 1:length(file.name)) {
  load(file.name[j])
  u.wind.data[j,1] <- month.day.year(getZ(masked.raster))$year
  u.wind.data[j,2] <- month.day.year(getZ(masked.raster))$month
  u.wind.data[j,2+(1:ncell(masked.raster))] <- masked.raster[1:ncell(masked.raster)]
  print(j)
  matlab::toc()
}


colnames(u.wind.data)<-c("year","month",1:ncell(masked.raster))

raster.men.grid=raster(xmn=min(-95), xmx=max(-84), ymn=min(28), ymx=max(33), resolution =1/6, crs=NA)
poly2<- rasterToPolygons(raster.men.grid, na.rm=F )

ind.r<- masked.raster
ind.r[1:ncell(ind.r)]<- 1:ncell(ind.r)
ex.list<- extract(ind.r, poly2, weights=T)


#df.menhaden$na.chl.replaced.by.smoothed <- rep(NA,times = dim(df.menhaden)[1])


men.jul <- julian(as.numeric(df.menhaden$month),1,as.numeric(df.menhaden$year), c(1,0,1800)) 
u.wind.jul <- julian(as.numeric(u.wind.data$month),1,as.numeric(u.wind.data$year), c(1,0,1800))



coordinates(df.menhaden)<- ~longitude+latitude
#poly.cell.ind <- over(df.menhaden,poly2,returnList = T)

require(matlab)
for (c in 0:6){
  u.wind.jul <- julian(as.numeric(u.wind.data$month),1,as.numeric(u.wind.data$year), c(1,0,1800))
  df.menhaden$smoothed.chl <- NA
  for ( i in which(u.wind.jul %in% men.jul))  {
    tic()
    ind.date <- which(men.jul == u.wind.jul[i])
    ind.cell <- cellFromXY(raster.men.grid,df.menhaden[ind.date,])
    wind.ind <- ex.list[ind.cell]
    
    weights <- c()
    cells <- c()
    for (j in 1: length(wind.ind)){
      cell.ind <- wind.ind[[j]][,1]
      weight.ind <- wind.ind[[j]][,2]
      length(cell.ind) <- 20
      length(weight.ind) <- 20
      cells<- rbind(cells, cell.ind)
      weights <- rbind(weights, weight.ind)
    }
    
    
    weighted.u.wind.values=c()
    
    
    for( z in 1: dim(weights)[1]){
      # U-wind
      weighted.u.wind.ind <- weighted.mean( (u.wind.data[(i-c), na.omit(cells[z,])+2 ]) , na.omit(weights[z,]))
      weighted.u.wind.values <- rbind( weighted.u.wind.values, weighted.u.wind.ind)
      
      
    }
    
    
    df.menhaden$smoothed.chl[ind.date] <- weighted.u.wind.values
    
    
  }
  
  
  names(df.menhaden)[names(df.menhaden)=="smoothed.chl"] <- paste("smoothed.chl.lag.", c, sep = "")
  toc()
  
}

df.menhaden <- subset(df.menhaden, select = - c(optional.2, remove, X , lwr, upr, fit, X.1,X.2,X.3,X.4,X.5,X.6,X.7, optional.4 ))


setwd()

write.csv(df.menhaden,file= "C:\\Users\\w966213\\Documents\\Gulf Mehaden Data\\NMFS Menhaden Surveys 1964-2011 mid-treatment\\NMFS Menhaden survey data set 1964-2011 with location step 7.csv")
