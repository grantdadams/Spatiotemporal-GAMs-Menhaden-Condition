source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")

library(qpcR)
library(mgcv)

base.gamm=gamm(relative.condition~ s(longitude,latitude) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

#################################
# River Discharge
#################################
# Non-spatially dependent 
sst.gamm.lag.0=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.lag.1=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.lag1.scaled)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.lag.2=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.lag2.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.lag.3=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.lag3.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.lag.4=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.lag4.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.lag.5=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.lag5.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.lag.6=gamm(relative.condition~ s(longitude,latitude) + s( monthly.miss.lag6.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.spring=gamm(relative.condition~ s(longitude,latitude) + s(mean.monthly.March.to.May.miss.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.winter=gamm(relative.condition~ s(longitude,latitude) + s(miss.discharge.Nov.to.March.scaled) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)


# Spatially dependent
sst.gamm.vc.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= monthly.miss.lag1.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= monthly.miss.lag2.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= monthly.miss.lag3.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= monthly.miss.lag4.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= monthly.miss.lag5.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= monthly.miss.lag6.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.spring=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mean.monthly.March.to.May.miss.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

sst.gamm.vc.winter=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= miss.discharge.Nov.to.March.scaled ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

#####################
# Summary Stats River
#####################
# Deviance explained
de <- c(summary(sst.gamm.lag.0)$dev.expl, summary(sst.gamm.lag.1)$dev.expl, summary(sst.gamm.lag.2)$dev.expl, summary(sst.gamm.lag.3)$dev.expl, summary(sst.gamm.lag.4)$dev.expl, summary(sst.gamm.lag.5)$dev.expl, summary(sst.gamm.lag.6)$dev.expl, summary(sst.gamm.spring)$dev.expl, summary(sst.gamm.winter)$dev.expl, summary(sst.gamm.vc.lag.0)$dev.expl, summary(sst.gamm.vc.lag.1)$dev.expl, summary(sst.gamm.vc.lag.2)$dev.expl, summary(sst.gamm.vc.lag.3)$dev.expl, summary(sst.gamm.vc.lag.4)$dev.expl, summary(sst.gamm.vc.lag.5)$dev.expl, summary(sst.gamm.vc.lag.6)$dev.expl, summary(sst.gamm.vc.spring)$dev.expl, summary(sst.gamm.vc.winter)$dev.expl)

# AIC
aic <- c(summary(base.gamm)$dev.expl, AIC(sst.gamm.lag.0), AIC(sst.gamm.lag.1), AIC(sst.gamm.lag.2), AIC(sst.gamm.lag.3), AIC(sst.gamm.lag.4), AIC(sst.gamm.lag.5), AIC(sst.gamm.lag.6), AIC(sst.gamm.spring), AIC(sst.gamm.winter), AIC(sst.gamm.vc.lag.0), AIC(sst.gamm.vc.lag.1), AIC(sst.gamm.vc.lag.2), AIC(sst.gamm.vc.lag.3), AIC(sst.gamm.vc.lag.4), AIC(sst.gamm.vc.lag.5), AIC(sst.gamm.vc.lag.6), AIC(sst.gamm.vc.spring), AIC(sst.gamm.vc.winter))

# Delta AIC
daic <- akaike.weights(c(AIC(base.gamm), AIC(sst.gamm.lag.0), AIC(sst.gamm.lag.1), AIC(sst.gamm.lag.2), AIC(sst.gamm.lag.3), AIC(sst.gamm.lag.4), AIC(sst.gamm.lag.5), AIC(sst.gamm.lag.6), AIC(sst.gamm.spring), AIC(sst.gamm.winter), AIC(sst.gamm.vc.lag.0), AIC(sst.gamm.vc.lag.1), AIC(sst.gamm.vc.lag.2), AIC(sst.gamm.vc.lag.3), AIC(sst.gamm.vc.lag.4), AIC(sst.gamm.vc.lag.5), AIC(sst.gamm.vc.lag.6), AIC(sst.gamm.vc.spring), AIC(sst.gamm.vc.winter)))$deltaAIC

# AIC weights
waic <- akaike.weights(c(AIC(base.gamm), AIC(sst.gamm.lag.0), AIC(sst.gamm.lag.1), AIC(sst.gamm.lag.2), AIC(sst.gamm.lag.3), AIC(sst.gamm.lag.4), AIC(sst.gamm.lag.5), AIC(sst.gamm.lag.6), AIC(sst.gamm.spring), AIC(sst.gamm.winter), AIC(sst.gamm.vc.lag.0), AIC(sst.gamm.vc.lag.1), AIC(sst.gamm.vc.lag.2), AIC(sst.gamm.vc.lag.3), AIC(sst.gamm.vc.lag.4), AIC(sst.gamm.vc.lag.5), AIC(sst.gamm.vc.lag.6), AIC(sst.gamm.vc.spring), AIC(sst.gamm.vc.winter)))$weights

# Names
names <- c("base.gamm",  "sst.gamm.lag.0", "sst.gamm.lag.1", "sst.gamm.lag.2", "sst.gamm.lag.3", "sst.gamm.lag.4", "sst.gamm.lag.5", "sst.gamm.lag.6", "sst.gamm.spring", "sst.gamm.winter", "sst.gamm.vc.lag.0", "sst.gamm.vc.lag.1", "sst.gamm.vc.lag.2", "sst.gamm.vc.lag.3", "sst.gamm.vc.lag.4", "sst.gamm.vc.lag.5", "sst.gamm.vc.lag.6", "sst.gamm.vc.spring", "sst.gamm.vc.winter")

GAMM_Model_Selection_River_Discharge_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic)

save(GAMM_Model_Selection_River_Discharge_Results, file = "GAMM_Model_Selection_River_Discharge_Results.RData")

