source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")

library(mgcv)
library(qpcR)

base.gamm=gamm(relative.condition~ s(longitude,latitude) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

#################################
# mei
#################################
# Non-spatially dependent 
mei.gamm.lag.0=gamm(relative.condition~ s(longitude,latitude) + s( mei) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.1=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.1)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.2=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.2) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.3=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.3) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.4=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.4) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.5=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.5) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.6=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.6) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.7=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.7)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.8=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.8) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.9=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.9) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.10=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.10) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.11=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.11) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.lag.12=gamm(relative.condition~ s(longitude,latitude) + s( mei.lag.12) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)


# Spatially dependent
mei.gamm.vc.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.1 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.2 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.3 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.4 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.5 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.6 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.7=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.7 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.8=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.8 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.9=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.9 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.10=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.10 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.11=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.11 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

mei.gamm.vc.lag.12=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= mei.lag.12 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

###################
# Summary Stats MEI
###################
# Deviance explained
de <- c(summary(base.gamm)$dev.expl, summary(mei.gamm.lag.0)$dev.expl, summary(mei.gamm.lag.1)$dev.expl, summary(mei.gamm.lag.2)$dev.expl, summary(mei.gamm.lag.3)$dev.expl, summary(mei.gamm.lag.4)$dev.expl, summary(mei.gamm.lag.5)$dev.expl, summary(mei.gamm.lag.6)$dev.expl, summary(mei.gamm.lag.7)$dev.expl, summary(mei.gamm.lag.8)$dev.expl, summary(mei.gamm.lag.9)$dev.expl, summary(mei.gamm.lag.10)$dev.expl, summary(mei.gamm.lag.11)$dev.expl, summary(mei.gamm.lag.12)$dev.expl, summary(mei.gamm.vc.lag.0)$dev.expl, summary(mei.gamm.vc.lag.1)$dev.expl, summary(mei.gamm.vc.lag.2)$dev.expl, summary(mei.gamm.vc.lag.3)$dev.expl, summary(mei.gamm.vc.lag.4)$dev.expl, summary(mei.gamm.vc.lag.5)$dev.expl, summary(mei.gamm.vc.lag.6)$dev.expl, summary(mei.gamm.vc.lag.7)$dev.expl, summary(mei.gamm.vc.lag.8)$dev.expl, summary(mei.gamm.vc.lag.9)$dev.expl, summary(mei.gamm.vc.lag.10)$dev.expl, summary(mei.gamm.vc.lag.11)$dev.expl, summary(mei.gamm.vc.lag.12)$dev.expl)

# AIC
aic<- c(AIC(base.gamm), AIC(mei.gamm.lag.0), AIC(mei.gamm.lag.1), AIC(mei.gamm.lag.2), AIC(mei.gamm.lag.3), AIC(mei.gamm.lag.4), AIC(mei.gamm.lag.5), AIC(mei.gamm.lag.6),AIC(mei.gamm.lag.7), AIC(mei.gamm.lag.8), AIC(mei.gamm.lag.9), AIC(mei.gamm.lag.10), AIC(mei.gamm.lag.11), AIC(mei.gamm.lag.12), AIC(mei.gamm.vc.lag.0), AIC(mei.gamm.vc.lag.1), AIC(mei.gamm.vc.lag.2), AIC(mei.gamm.vc.lag.3), AIC(mei.gamm.vc.lag.4), AIC(mei.gamm.vc.lag.5), AIC(mei.gamm.vc.lag.6), AIC(mei.gamm.vc.lag.7), AIC(mei.gamm.vc.lag.8), AIC(mei.gamm.vc.lag.9), AIC(mei.gamm.vc.lag.10), AIC(mei.gamm.vc.lag.11), AIC(mei.gamm.vc.lag.12))

# Delta AIC
daic <- akaike.weights(AIC(base.gamm), c(AIC(mei.gamm.lag.0), AIC(mei.gamm.lag.1), AIC(mei.gamm.lag.2), AIC(mei.gamm.lag.3), AIC(mei.gamm.lag.4), AIC(mei.gamm.lag.5), AIC(mei.gamm.lag.6),AIC(mei.gamm.lag.7), AIC(mei.gamm.lag.8), AIC(mei.gamm.lag.9), AIC(mei.gamm.lag.10), AIC(mei.gamm.lag.11), AIC(mei.gamm.lag.12), AIC(mei.gamm.vc.lag.0), AIC(mei.gamm.vc.lag.1), AIC(mei.gamm.vc.lag.2), AIC(mei.gamm.vc.lag.3), AIC(mei.gamm.vc.lag.4), AIC(mei.gamm.vc.lag.5), AIC(mei.gamm.vc.lag.6), AIC(mei.gamm.vc.lag.7), AIC(mei.gamm.vc.lag.8), AIC(mei.gamm.vc.lag.9), AIC(mei.gamm.vc.lag.10), AIC(mei.gamm.vc.lag.11), AIC(mei.gamm.vc.lag.12)))$deltaAIC

# AIC Weights
waic <- akaike.weights(AIC(base.gamm), c(AIC(mei.gamm.lag.0), AIC(mei.gamm.lag.1), AIC(mei.gamm.lag.2), AIC(mei.gamm.lag.3), AIC(mei.gamm.lag.4), AIC(mei.gamm.lag.5), AIC(mei.gamm.lag.6),AIC(mei.gamm.lag.7), AIC(mei.gamm.lag.8), AIC(mei.gamm.lag.9), AIC(mei.gamm.lag.10), AIC(mei.gamm.lag.11), AIC(mei.gamm.lag.12), AIC(mei.gamm.vc.lag.0), AIC(mei.gamm.vc.lag.1), AIC(mei.gamm.vc.lag.2), AIC(mei.gamm.vc.lag.3), AIC(mei.gamm.vc.lag.4), AIC(mei.gamm.vc.lag.5), AIC(mei.gamm.vc.lag.6), AIC(mei.gamm.vc.lag.7), AIC(mei.gamm.vc.lag.8), AIC(mei.gamm.vc.lag.9), AIC(mei.gamm.vc.lag.10), AIC(mei.gamm.vc.lag.11), AIC(mei.gamm.vc.lag.12)))$weights

names <- c("base.gamm", "mei.gamm.lag.0", "mei.gamm.lag.1", "mei.gamm.lag.2", "mei.gamm.lag.3", "mei.gamm.lag.4", "mei.gamm.lag.5", "mei.gamm.lag.6","mei.gamm.lag.7", "mei.gamm.lag.8", "mei.gamm.lag.9", "mei.gamm.lag.10", "mei.gamm.lag.11", "mei.gamm.lag.12", "mei.gamm.vc.lag.0", "mei.gamm.vc.lag.1", "mei.gamm.vc.lag.2", "mei.gamm.vc.lag.3", "mei.gamm.vc.lag.4", "mei.gamm.vc.lag.5", "mei.gamm.vc.lag.6", "mei.gamm.vc.lag.7", "mei.gamm.vc.lag.8", "mei.gamm.vc.lag.9", "mei.gamm.vc.lag.10", "mei.gamm.vc.lag.11", "mei.gamm.vc.lag.12")

GAMM_Model_Selection_MEI_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic)

save(GAMM_Model_Selection_MEI_Results, file = "GAMM_Model_Selection_MEI_Results.RData")