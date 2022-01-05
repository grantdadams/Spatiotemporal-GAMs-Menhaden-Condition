#################
# Data Load
#################
rm(list=ls())
load("menhaden_data_post_processing_6_20.RData")
library(mgcv)
library(qpcR)




#####################
# Final Models
#####################
library(qpcR)
library(mgcv)

# Fit full model and get residuals
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag0) + s(latitude, longitude, by = monthly.v.wind.lag1), data = df.menhaden, method = "fREML")

df.menhaden$residuals <- resid(final_model)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

model <- bam(residuals ~ s(SST_9km_lag_0), data= df.menhaden.short, method = "fREML")


##################################################
# Plot it ########################################
##################################################

model.list <- list(final_model , model)

#tiff(file="Figures/Fig_S1_GAM_Diagnositcs.tiff" , height= 5/1.045881,  width=3.34646*2 , pointsize=7.5 , family = "serif" , res =300, units = "in")

postscript("Figures/Fig_S1_GAM_Diagnositcs.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height= 5/1.045881*(2/3),  width=3.34646*2, pointsize = 7.5, family = "serif" )
nf <- layout(matrix(c(1:10) , 2 , 5 , byrow=TRUE) , widths = c(.2,1,1,1,1),  TRUE)
par(mar=c(3 , 3 , 1 , 1) , tcl=-.25 , mgp=c(2 ,  .5 ,  0) ,  oma=c(0 , 2 , 0 , 0))

library(FSA)

##############
# Base Model
##############
model.text <- c("Relative condition GAM", "Residual condition GAM")
year.text <- c("(1964-2011)","(2003-2011)")

for (i in 1:length(model.list)) {
  plot.new()
  model <- model.list[[i]]
  type <- "pearson"  ## "pearson" & "response" are other valid choices
  pearson.resid <- residuals(model, type = type)
  linpred <- napredict(model$na.action, model$linear.predictors)
  observed.y <- napredict(model$na.action, model$y)
  
  # Hist of residuals
  par(xpd=F)
  hist(~pearson.resid, main=NA , col ="white", ylab = "Frequency", xlab = "Pearson residuals")
  #axis(side = 2, at = c(0,20000,40000,60000,80000,100000) , labels = c (0,2,4,6,8,10), las =1)
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
  smoothScatter(fitted(model), observed.y, xlab = "Fitted Values", colramp = colorRampPalette(c("white", 1)), ylab = "Response")
  abline(lm(observed.y ~ fitted(model)), col = "grey30")
  legend("topleft", "c.", bty="n") 
  
  # Resid vs Linear predictor
  
  smoothScatter(linpred, pearson.resid, xlab = "Linear predictor", ylab = "Pearson residuals", colramp = colorRampPalette(c("white", 1)), las =1)
  abline(h = 0, col = "grey30")
  legend("topleft", "d.", bty="n") 
}





dev.off()















