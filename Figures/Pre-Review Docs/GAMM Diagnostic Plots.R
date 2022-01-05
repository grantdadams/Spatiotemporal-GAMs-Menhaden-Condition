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

spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k =6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

spatially.explicit.bam.short=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("individual.monthly.miss.")) == "individual.monthly.miss.")
df.menhaden2 <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
river.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( individual.monthly.miss.scaled) + s(month, k = 6) + factor(year) , data= df.menhaden2, method = "fREML", family = Gamma(link = "log") )

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("mei")) == "mei")
df.menhaden2 <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
mei.bam.lag.10=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.10) + s(month, k =6) + factor(year) , data= df.menhaden2, method = "fREML", family = Gamma(link = "log") )

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("monthly.u.wind.")) == "monthly.u.wind.")
df.menhaden2 <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]
wind.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag2) + s( latitude , longitude, by = monthly.v.wind.lag2) + s(month, k = 6) + factor(year), data= df.menhaden2, method = "ML", family = Gamma(link = "log"))

sst.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_0) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_2 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "ML", family = Gamma(link = "log"))


##################################################
# Plot it ########################################
##################################################

model.list <- list(spatially.explicit.bam , spatially.explicit.bam.short , river.bam.lag.0, mei.bam.lag.10, wind.bam.vc.lag.2, sst.bam.lag.0)

tiff(file="GAMM_Diagnositcs.tiff" , height= 8,  width=7 , pointsize=7.5 , family = "serif" , res =300, units = "in")
#### River\
nf <- layout(matrix(c(1:40) , 8 , 5 , byrow=TRUE) , widths = c(.2,1,1,1,1),  TRUE)
par( mar=c(3 , 3 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))

library(FSA)

##############
# Base Model
##############
model.text <- c("Spatially-explicit GAMM", "Spatially-explicit GAMM", "River discharge GAMM", "ENSO GAMM", "Wind GAMM", "SST GAMM", "Chlorophyll GAMM")
year.text <- c("(1964-2011)","(2003-2011)", "(1964-2011)","(1964-2011)","(1964-2011)", "(2003-2011)", "(2003-2011)")

for (i in 1:length(model.list)) {
  plot.new()
  model <- model.list[[i]]
  type <- "pearson"  ## "pearson" & "response" are other valid choices
  pearson.resid <- residuals(model, type = type)
  linpred <- napredict(model$na.action, model$linear.predictors)
  observed.y <- napredict(model$na.action, model$y)
  
  # Hist of residuals
  par(xpd=F)
  hist(~pearson.resid, main=NA , col ="white", ylab = "Frequency (× 10,000)", yaxt = "n", xlab = "Pearson residuals")
  axis(side = 2, at = c(0,20000,40000,60000,80000,100000) , labels = c (0,2,4,6,8,10), las =1)
  #mtext("Pearson residuals",side=1, line = 1.5, adj = .45 )
  par(xpd=T)
  segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[1] , y1 = par("usr")[4] + 200, col =1) # left line
  segments(x0 = par("usr")[2], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[4] + 200, col =1) # right line
  segments(x0 = par("usr")[1], y0= par("usr")[4] + 200, x1 = par("usr")[2], y1 = par("usr")[4] + 200, col =1) # top line
  segments(x0 = par("usr")[1], y0= par("usr")[3], x1 = par("usr")[2], y1 = par("usr")[3], col =1) # bottom line
  legend("topleft", "a.", bty="n")
  mtext(model.text[i], side=2,line = 4.5, cex = .75)
  mtext(year.text[i], side=2,line = 3.5, cex = .75)
  
  
  # QQ Plot
  par(xpd=F)
  qqnorm(pearson.resid, cex = .5, ylab = "Pearson residuals", main = NA, xlab="Theoretical quantiles"); qqline(pearson.resid)
  legend("topleft", "b.", bty="n") 
  
  
  # Response vs fitted
  smoothScatter(fitted(model), observed.y, xlab = "Fitted Values", colramp = colorRampPalette(c("white", 1)), ylab = "Response", yaxt ="n")
  abline(lm(observed.y ~ fitted(model)), col = "grey30")
  
  axis(side =2 , at= seq(from = 70, to = 150, by = 10), labels = c(70,"",90,"",110, "",130, "",150) ,las=1)
  legend("topleft", "c.", bty="n") 
  
  # Resid vs Linear predictor
  
  smoothScatter(linpred, pearson.resid, xlab = "Linear predictor", ylab = "Pearson residuals", colramp = colorRampPalette(c("white", 1)), las =1)
  abline(h = 0, col = "grey30")
  legend("topleft", "d.", bty="n") 
  
}





dev.off()















