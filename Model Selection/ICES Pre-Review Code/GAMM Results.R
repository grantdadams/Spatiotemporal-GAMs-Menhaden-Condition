#########################
# Model Selection Results
#########################
load("fREML GAMMs/bam_Model_Selection_Wind_Results.RData")
load("fREML GAMMs/bam_Model_Selection_MEI_Results.RData")
load("fREML GAMMs/bam_Model_Selection_SST_Results.RData")
load("fREML GAMMs/bam_Model_Selection_CHL_Results.RData")
load("fREML GAMMs/bam_Model_Selection_AMO_Results.RData")
load("fREML GAMMs/bam_Model_Selection_NAO_Results.RData")
load("fREML GAMMs/bam_Model_Selection_River_Discharge_Results.RData")


complete_results <- rbind(bam_Model_Selection_River_Discharge_Results, bam_Model_Selection_MEI_Results, bam_Model_Selection_Wind_Results,bam_Model_Selection_SST_Results)


write.csv(complete_results, file = "BAMM_Modelling_Results.csv")


#################
# Data Load
#################
load("menhaden_data_post_processing_6_20.RData")
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]


#####################
# Final Models
#####################
#source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")
library(qpcR)
library(mgcv)

spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k =6) + factor(year)  , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

spatially.explicit.bam.short=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year) , data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("individual.monthly.miss.")) == "individual.monthly.miss.")
df.menhaden2 <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
river.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( individual.monthly.miss.scaled) + s(month, k = 6) + factor(year)  , data= df.menhaden2, method = "fREML", family = Gamma(link = "log") )

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("mei")) == "mei")
df.menhaden2 <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
mei.bam.lag.10=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.10) + s(month, k =6) + factor(year)  , data= df.menhaden2, method = "fREML", family = Gamma(link = "log") )

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("monthly.u.wind.")) == "monthly.u.wind.")
df.menhaden2 <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
wind.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag2) + s( latitude , longitude, by = monthly.v.wind.lag2) + s(month, k = 6) + factor(year) , data= df.menhaden2, method = "ML", family = Gamma(link = "log"))

sst.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_0) + s(month, k = 6) + factor(year) , data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_2 ) + s(month, k = 6) + factor(year) , data= df.menhaden.short, method = "ML", family = Gamma(link = "log"))

#############
# Stats
#############
model.list <- list(spatially.explicit.bam , spatially.explicit.bam.short , river.bam.lag.0, mei.bam.lag.10, wind.bam.vc.lag.2, sst.bam.lag.0, chl.bam.vc.lag.2)

summary_stats_fn <- function(){
  n.mod <- length(model.list)
  a_y <-c()
  g_lat_long <-c()
  s_month <- c()
  pred <- c(NA,NA)
  mod_aic <- c()
  mod_de <- c()

  for(i in 1:n.mod){
    a_y[i] <- round(summary(model.list[[i]])$p.coeff[1],1)
    g_lat_long[i] <-round(summary(model.list[[i]])$s.table["s(longitude,latitude)",1],1)
    s_month[i] <-  round(summary(model.list[[i]])$s.table["s(month)",1],1)
    
    if(i > 2){
      pred[i] <- round(summary(model.list[[i]])$s.table[2,1],1)
    }
    
    mod_aic[i] <- round(AIC(model.list[[i]]))
    mod_de[i] <- round(summary(model.list[[i]])$dev.expl , 4) * 100
  }
  stats <- data.frame(a_y = a_y, g_lat_long= g_lat_long, s_month = s_month, pred = pred, mod_aic= mod_aic, mod_de = mod_de)
  return(stats)
}

stats <- summary_stats_fn()



###########################
# Correlation of predictors
###########################

cor.matrix <- cbind(df.menhaden.short$monthly.miss.scaled, df.menhaden.short$mei.lag.10, df.menhaden.short$monthly.u.wind.lag1, df.menhaden.short$monthly.v.wind.lag1, df.menhaden.short$SST_9km_lag_0, df.menhaden.short$chl_9km_lag_5 )
colnames(cor.matrix) <- c("monthly.miss.scaled", "mei.lag.10", "monthly.u.wind.lag1", "monthly.v.wind.lag1", "SST_9km_lag_0", "chl_9km_lag_5")
cor(cor.matrix)


# Global Models
global.bam=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s( mei.lag.10) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s( SST_9km_lag_0) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )
summary(global.bam)

global.bam.big=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s( mei.lag.10) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden, method = "REML", family = Gamma(link = "log") )
summary(global.bam.big)
acf(resid(global.bam.big))


#########################
# Diagnostics
#########################
library(geoR)
library(maptools)
library(raster)
library(sp)
library(itsadug)
library(dplyr)
library(plyr)

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







########
# Plot
########



tiff(file="GAMM_Diagnositcs.tiff" , height= 300,  width=200 , pointsize=10 , family = "serif" , units = "mm", res =300)
#### River
par(mfrow=c(5,3), mar=c(3 , 3 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))


model.text <- c("River Discharge", "MEI", "Wind", "SST", "Chlorophyll" )

for(i in 3:length(model.list)){
  qq.gam(model.list[[i]])
  mtext(model.text[i], 2,  line = 3.3, cex = .8)
  resid <- resid(model.list[[i]])
  x <- cbind(resid, df.menhaden)
  x <- x[,c("week","date","year","resid","month","relative.condition")]
  d <- ddply(x,~year+week,summarise, mean=mean(relative.condition))
  acf(d$mean, ci =F, main = NA, lag.max = 30)
  
  if(summary(model.list[[i]])$n > 300000){
    gb <- data.frame(data = residuals(model.list[[i]], type = "d"), coords = coords)
    for (i in 1:100){
      samples <- sample(c(1:nrow(df.menhaden)), 30000, replace = F)
      gb.1 <- gb[samples]
      if ( i ==1){
        plot(variog(gb.1, max.dist = 100000), type = "l", ylim = c(0,0.01), xlab = "Distance (m)") 
      } else{
        lines(variog(gb.1, max.dist = 100000))
        
      }
    }
  } else {
    gb <- list(data = residuals(model.list[[i]], type = "d"), coords = coords.short)
    plot(variog(gb, max.dist = 100000), type = "l", xlab = "Distance (m)")
  }
}


qq.gam(river.bam.vc.lag.0)
mtext("River Discharge", 2,  line = 3.3, cex = .8)
resid <- resid(river.bam.vc.lag.0)
x <- cbind(resid, df.menhaden)
x <- x[,c("week","date","year","resid","month","relative.condition")]
d <- ddply(x,~year+week,summarise, mean=mean(relative.condition))
acf(d$mean, ci =F, main = NA, lag.max = 30)

gb <- data.frame(data = residuals(river.bam.vc.lag.0, type = "d"), coords = coords)
for (i in 1:100){
  samples <- sample(c(1:nrow(df.menhaden)), 30000, replace = F)
  gb.1 <- gb
  gb.1[[1]] <-gb[[1]][samples]
  gb.1[[2]] <- gb[[2]][samples,]
  if ( i ==1){
   plot(variog(gb.1, max.dist = 100000), type = "l", ylim = c(0,0.01), xlab = "Distance (m)") 
  } else{
    lines(variog(gb.1, max.dist = 100000))
    
  }
}



#### MEI
qq.gam(mei.bam.lag.10)
mtext("MEI", 2,  line = 3.3, cex = .8)
acf(resid(mei.bam.lag.10), ci =F, main = NA)

gb <- list(data = residuals(mei.bam.lag.10, type = "d"), coords = coords)
for (i in 1:100){
  samples <- sample(c(1:nrow(df.menhaden)), 30000, replace = F)
  gb.1 <- gb
  gb.1[[1]] <-gb[[1]][samples]
  gb.1[[2]] <- gb[[2]][samples,]
  if ( i ==1){
    plot(variog(gb.1, max.dist = 100000), type = "l", ylim = c(0,0.01), xlab = "Distance (m)") 
  } else{
    lines(variog(gb.1, max.dist = 100000))
    
  }
}



#### Wind
qq.gam(wind.bam.vc.lag.1)
mtext("Wind", 2,  line = 3.3, cex = .8)
acf(resid(wind.bam.vc.lag.1), ci =F, main = NA)

gb <- list(data = residuals(wind.bam.vc.lag.1, type = "d"), coords = coords)
for (i in 1:100){
  samples <- sample(c(1:nrow(df.menhaden)), 30000, replace = F)
  gb.1 <- gb
  gb.1[[1]] <-gb[[1]][samples]
  gb.1[[2]] <- gb[[2]][samples,]
  if ( i ==1){
    plot(variog(gb.1, max.dist = 100000), type = "l", ylim = c(0,0.01), xlab = "Distance (m)") 
  } else{
    lines(variog(gb.1, max.dist = 100000))
    
  }
}


### SST
qq.gam(sst.bam.lag.0)
mtext("SST", 2,  line = 3.3, cex = .8)
acf(resid(sst.bam.lag.0), ci =F, main = NA)

gb <- list(data = residuals(sst.bam.lag.0, type = "d"), coords = coords.short)
plot(variog(gb, max.dist = 100000), type = "l", xlab = "Distance (m)")




### CHL
qq.gam(chl.bam.vc.lag.5)
mtext("Chlorophyll", 2,  line = 3.3, cex = .8)
acf(resid(chl.bam.vc.lag.5), ci =F, main = NA)

gb <- list(data = residuals(chl.bam.vc.lag.5, type = "d"), coords = coords.short)
plot(variog(gb, max.dist = 100000), type = "l", xlab = "Distance (m)")

dev.off()


