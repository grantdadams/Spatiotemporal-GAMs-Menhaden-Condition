source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")

library(mgcv)
library(qpcR)

df.menhaden.short<- df.menhaden[which(df.menhaden$year>=2003),]

base.gamm=gamm(relative.condition~ s(longitude,latitude) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

#################################
# SST
#################################
# Non-spatially dependent 
sst.gamm.lag.0=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_0) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.lag.1=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_1)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.lag.2=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_2) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.lag.3=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_3) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.lag.4=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_4) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.lag.5=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_5) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.lag.6=gamm(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_6) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)


# Spatially dependent
sst.gamm.vc.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_0 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.vc.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_1 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.vc.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_2 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.vc.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_3 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.vc.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_4 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.vc.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_5 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

sst.gamm.vc.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_6 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

###################
# Summary Stats SST
###################
# Deviance explained
de <- c(summary(base.gamm)$dev.expl, summary(sst.gamm.lag.0)$dev.expl, summary(sst.gamm.lag.1)$dev.expl, summary(sst.gamm.lag.2)$dev.expl, summary(sst.gamm.lag.3)$dev.expl, summary(sst.gamm.lag.4)$dev.expl, summary(sst.gamm.lag.5)$dev.expl, summary(sst.gamm.lag.6)$dev.expl, summary(sst.gamm.vc.lag.0)$dev.expl, summary(sst.gamm.vc.lag.1)$dev.expl, summary(sst.gamm.vc.lag.2)$dev.expl, summary(sst.gamm.vc.lag.3)$dev.expl, summary(sst.gamm.vc.lag.4)$dev.expl, summary(sst.gamm.vc.lag.5)$dev.expl, summary(sst.gamm.vc.lag.6)$dev.expl)

# AIC
aic <- c(AIC(sst.gamm.lag.0), AIC(sst.gamm.lag.1), AIC(sst.gamm.lag.2), AIC(sst.gamm.lag.3), AIC(sst.gamm.lag.4), AIC(sst.gamm.lag.5), AIC(sst.gamm.lag.6), AIC(sst.gamm.vc.lag.0), AIC(sst.gamm.vc.lag.1), AIC(sst.gamm.vc.lag.2), AIC(sst.gamm.vc.lag.3), AIC(sst.gamm.vc.lag.4), AIC(sst.gamm.vc.lag.5), AIC(sst.gamm.vc.lag.6))

# Delta AIC
daic <- akaike.weights(c(AIC(sst.gamm.lag.0), AIC(sst.gamm.lag.1), AIC(sst.gamm.lag.2), AIC(sst.gamm.lag.3), AIC(sst.gamm.lag.4), AIC(sst.gamm.lag.5), AIC(sst.gamm.lag.6), AIC(sst.gamm.vc.lag.0), AIC(sst.gamm.vc.lag.1), AIC(sst.gamm.vc.lag.2), AIC(sst.gamm.vc.lag.3), AIC(sst.gamm.vc.lag.4), AIC(sst.gamm.vc.lag.5), AIC(sst.gamm.vc.lag.6)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(sst.gamm.lag.0), AIC(sst.gamm.lag.1), AIC(sst.gamm.lag.2), AIC(sst.gamm.lag.3), AIC(sst.gamm.lag.4), AIC(sst.gamm.lag.5), AIC(sst.gamm.lag.6), AIC(sst.gamm.vc.lag.0), AIC(sst.gamm.vc.lag.1), AIC(sst.gamm.vc.lag.2), AIC(sst.gamm.vc.lag.3), AIC(sst.gamm.vc.lag.4), AIC(sst.gamm.vc.lag.5), AIC(sst.gamm.vc.lag.6)))$weights

# Names
names <- c("base.gamm", "sst.gamm.lag.0", "sst.gamm.lag.1", "sst.gamm.lag.2", "sst.gamm.lag.3", "sst.gamm.lag.4", "sst.gamm.lag.5", "sst.gamm.lag.6", "sst.gamm.vc.lag.0", "sst.gamm.vc.lag.1", "sst.gamm.vc.lag.2", "sst.gamm.vc.lag.3", "sst.gamm.vc.lag.4", "sst.gamm.vc.lag.5", "sst.gamm.vc.lag.6")

GAMM_Model_Selection_SST_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic)

save(GAMM_Model_Selection_SST_Results, file = "GAMM_Model_Selection_SST_Results.RData")



#######################
# Chlorophyll
#######################

# Non-spatially dependent 
chl.gamm.lag.0=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_0) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.lag.1=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_1) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.lag.2=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_2) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.lag.3=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_3) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.lag.4=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_4) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.lag.5=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_5) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.lag.6=gamm(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_6) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)


# Spatially dependent
chl.gamm.vc.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_0 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.vc.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_1 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.vc.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_2 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.vc.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_3 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.vc.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_4 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.vc.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

chl.gamm.vc.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_6 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden.short, method = "REML" , select = T)

###########################
# Summary Stats Chlorophyll
###########################
# Deviance explained
de <- c(summary(base.gamm)$dev.expl, summary(chl.gamm.lag.0)$dev.expl, summary(chl.gamm.lag.1)$dev.expl, summary(chl.gamm.lag.2)$dev.expl, summary(chl.gamm.lag.3)$dev.expl, summary(chl.gamm.lag.4)$dev.expl, summary(chl.gamm.lag.5)$dev.expl, summary(chl.gamm.lag.6)$dev.expl, summary(chl.gamm.vc.lag.0)$dev.expl, summary(chl.gamm.vc.lag.1)$dev.expl, summary(chl.gamm.vc.lag.2)$dev.expl, summary(chl.gamm.vc.lag.3)$dev.expl, summary(chl.gamm.vc.lag.4)$dev.expl, summary(chl.gamm.vc.lag.5)$dev.expl, summary(chl.gamm.vc.lag.6)$dev.expl)

# AIC
aic <- c(AIC(base.gamm), AIC(chl.gamm.lag.0), AIC(chl.gamm.lag.1), AIC(chl.gamm.lag.2), AIC(chl.gamm.lag.3), AIC(chl.gamm.lag.4), AIC(chl.gamm.lag.5), AIC(chl.gamm.lag.6), AIC(chl.gamm.vc.lag.0), AIC(chl.gamm.vc.lag.1), AIC(chl.gamm.vc.lag.2), AIC(chl.gamm.vc.lag.3), AIC(chl.gamm.vc.lag.4), AIC(chl.gamm.vc.lag.5), AIC(chl.gamm.vc.lag.6))

# Delta AIC
daic <- akaike.weights(AIC(base.gamm), c(AIC(chl.gamm.lag.0), AIC(chl.gamm.lag.1), AIC(chl.gamm.lag.2), AIC(chl.gamm.lag.3), AIC(chl.gamm.lag.4), AIC(chl.gamm.lag.5), AIC(chl.gamm.lag.6), AIC(chl.gamm.vc.lag.0), AIC(chl.gamm.vc.lag.1), AIC(chl.gamm.vc.lag.2), AIC(chl.gamm.vc.lag.3), AIC(chl.gamm.vc.lag.4), AIC(chl.gamm.vc.lag.5), AIC(chl.gamm.vc.lag.6)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.gamm), AIC(chl.gamm.lag.0), AIC(chl.gamm.lag.1), AIC(chl.gamm.lag.2), AIC(chl.gamm.lag.3), AIC(chl.gamm.lag.4), AIC(chl.gamm.lag.5), AIC(chl.gamm.lag.6), AIC(chl.gamm.vc.lag.0), AIC(chl.gamm.vc.lag.1), AIC(chl.gamm.vc.lag.2), AIC(chl.gamm.vc.lag.3), AIC(chl.gamm.vc.lag.4), AIC(chl.gamm.vc.lag.5), AIC(chl.gamm.vc.lag.6)))$weights

# Names
names <- c("base.gamm", "chl.gamm.lag.0", "chl.gamm.lag.1", "chl.gamm.lag.2", "chl.gamm.lag.3", "chl.gamm.lag.4", "chl.gamm.lag.5", "chl.gamm.lag.6", "chl.gamm.vc.lag.0", "chl.gamm.vc.lag.1", "chl.gamm.vc.lag.2", "chl.gamm.vc.lag.3", "chl.gamm.vc.lag.4", "chl.gamm.vc.lag.5", "chl.gamm.vc.lag.6")

GAMM_Model_Selection_CHL_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic)

save(GAMM_Model_Selection_CHL_Results, file = "GAMM_Model_Selection_CHL_Results.RData")


