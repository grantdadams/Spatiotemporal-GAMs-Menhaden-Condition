source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")

library(mgcv)
library(qpcR)

base.gamm=gamm(relative.condition~ s(longitude,latitude) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

#################################
# wind
#################################
# Non-spatially dependent 
wind.gamm.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag0) + s(monthly.v.wind.scaled.lag0) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag1) + s(monthly.v.wind.scaled.lag1) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag2) + s(monthly.v.wind.scaled.lag2) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag3) + s(monthly.v.wind.scaled.lag3) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag4) + s(monthly.v.wind.scaled.lag4) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag5) + s(monthly.v.wind.scaled.lag5) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.scaled.lag6) + s(monthly.v.wind.scaled.lag6) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)


# Spatially dependent
wind.gamm.vc.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag0) + s( latitude , longitude, by = monthly.v.wind.scaled.lag0) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.vc.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag1) + s( latitude , longitude, by = monthly.v.wind.scaled.lag1) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.vc.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag2) + s( latitude , longitude, by = monthly.v.wind.scaled.lag2) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.vc.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag3) + s( latitude , longitude, by = monthly.v.wind.scaled.lag3) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.vc.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag4) + s( latitude , longitude, by = monthly.v.wind.scaled.lag4)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.vc.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag5) + s( latitude , longitude, by = monthly.v.wind.scaled.lag5)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

wind.gamm.vc.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.scaled.lag6) + s( latitude , longitude, by = monthly.v.wind.scaled.lag6)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

###################
# Summary Stats wind
###################
# Deviance explained
de <- c(summary(base.gamm)$dev.expl ,summary(wind.gamm.lag.0)$dev.expl, summary(wind.gamm.lag.1)$dev.expl, summary(wind.gamm.lag.2)$dev.expl, summary(wind.gamm.lag.3)$dev.expl, summary(wind.gamm.lag.4)$dev.expl, summary(wind.gamm.lag.5)$dev.expl, summary(wind.gamm.lag.6)$dev.expl, summary(wind.gamm.vc.lag.0)$dev.expl, summary(wind.gamm.vc.lag.1)$dev.expl, summary(wind.gamm.vc.lag.2)$dev.expl, summary(wind.gamm.vc.lag.3)$dev.expl, summary(wind.gamm.vc.lag.4)$dev.expl, summary(wind.gamm.vc.lag.5)$dev.expl, summary(wind.gamm.vc.lag.6)$dev.expl)

# AIC
aic <- c(AIC(base.gamm), AIC(wind.gamm.lag.0), AIC(wind.gamm.lag.1), AIC(wind.gamm.lag.2), AIC(wind.gamm.lag.3), AIC(wind.gamm.lag.4), AIC(wind.gamm.lag.5), AIC(wind.gamm.lag.6), AIC(wind.gamm.vc.lag.0), AIC(wind.gamm.vc.lag.1), AIC(wind.gamm.vc.lag.2), AIC(wind.gamm.vc.lag.3), AIC(wind.gamm.vc.lag.4), AIC(wind.gamm.vc.lag.5), AIC(wind.gamm.vc.lag.6))

# Delta AIC
daic <- waic <- akaike.weights(c(AIC(base.gamm), AIC(wind.gamm.lag.0), AIC(wind.gamm.lag.1), AIC(wind.gamm.lag.2), AIC(wind.gamm.lag.3), AIC(wind.gamm.lag.4), AIC(wind.gamm.lag.5), AIC(wind.gamm.lag.6), AIC(wind.gamm.vc.lag.0), AIC(wind.gamm.vc.lag.1), AIC(wind.gamm.vc.lag.2), AIC(wind.gamm.vc.lag.3), AIC(wind.gamm.vc.lag.4), AIC(wind.gamm.vc.lag.5), AIC(wind.gamm.vc.lag.6)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.gamm), AIC(wind.gamm.lag.0), AIC(wind.gamm.lag.1), AIC(wind.gamm.lag.2), AIC(wind.gamm.lag.3), AIC(wind.gamm.lag.4), AIC(wind.gamm.lag.5), AIC(wind.gamm.lag.6), AIC(wind.gamm.vc.lag.0), AIC(wind.gamm.vc.lag.1), AIC(wind.gamm.vc.lag.2), AIC(wind.gamm.vc.lag.3), AIC(wind.gamm.vc.lag.4), AIC(wind.gamm.vc.lag.5), AIC(wind.gamm.vc.lag.6)))$weights

# Names 
names <- c("base.gamm","wind.gamm.lag.0", "wind.gamm.lag.1", "wind.gamm.lag.2", "wind.gamm.lag.3", "wind.gamm.lag.4", "wind.gamm.lag.5", "wind.gamm.lag.6", "wind.gamm.vc.lag.0", "wind.gamm.vc.lag.1", "wind.gamm.vc.lag.2", "wind.gamm.vc.lag.3", "wind.gamm.vc.lag.4", "wind.gamm.vc.lag.5", "wind.gamm.vc.lag.6")

GAMM_Model_Selection_Wind_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic)

save(GAMM_Model_Selection_Wind_Results, file = "GAMM_Model_Selection_Wind_Results.RData")