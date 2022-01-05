source("C:\\Users\\w966213\\Documents\\Prospectus Chapter 1\\R Scripts\\Model Selection\\GAMM Model Selection Data Prep.R")

library(mgcv)
library(qpcR)

base.gamm=gamm(relative.condition~ s(longitude,latitude) + s(month , k =6) + factor(year), random = ~(1|collection), data= df.menhaden, method = "REML" , select = T)

#################################
# amo
#################################
# Non-spatially dependent 
amo.gamm.lag.0=gamm(relative.condition~ s(longitude,latitude) + s( amo) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.1=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.1)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.2=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.2) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.3=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.3) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.4=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.4) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.5=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.5) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.6=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.6) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.7=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.7)  + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.8=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.8) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.9=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.9) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.10=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.10) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.11=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.11) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.lag.12=gamm(relative.condition~ s(longitude,latitude) + s( amo.lag.12) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)


# Spatially dependent
amo.gamm.vc.lag.0=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.1=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.1 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.2=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.2 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.3=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.3 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.4=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.4 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.5=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.5 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.6=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.6 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.7=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.7 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.8=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.8 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.9=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.9 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.10=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.10 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.11=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.11 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

amo.gamm.vc.lag.12=gamm(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= amo.lag.12 ) + s(month , k =6) + factor(year), random = list(collection = ~1), data= df.menhaden, method = "REML" , select = T)

###################
# Summary Stats amo
###################
# Deviance explained
de <- c(summary(base.gamm)$dev.expl, summary(amo.gamm.lag.0)$dev.expl, summary(amo.gamm.lag.1)$dev.expl, summary(amo.gamm.lag.2)$dev.expl, summary(amo.gamm.lag.3)$dev.expl, summary(amo.gamm.lag.4)$dev.expl, summary(amo.gamm.lag.5)$dev.expl, summary(amo.gamm.lag.6)$dev.expl, summary(amo.gamm.lag.7)$dev.expl, summary(amo.gamm.lag.8)$dev.expl, summary(amo.gamm.lag.9)$dev.expl, summary(amo.gamm.lag.10)$dev.expl, summary(amo.gamm.lag.11)$dev.expl, summary(amo.gamm.lag.12)$dev.expl, summary(amo.gamm.vc.lag.0)$dev.expl, summary(amo.gamm.vc.lag.1)$dev.expl, summary(amo.gamm.vc.lag.2)$dev.expl, summary(amo.gamm.vc.lag.3)$dev.expl, summary(amo.gamm.vc.lag.4)$dev.expl, summary(amo.gamm.vc.lag.5)$dev.expl, summary(amo.gamm.vc.lag.6)$dev.expl, summary(amo.gamm.vc.lag.7)$dev.expl, summary(amo.gamm.vc.lag.8)$dev.expl, summary(amo.gamm.vc.lag.9)$dev.expl, summary(amo.gamm.vc.lag.10)$dev.expl, summary(amo.gamm.vc.lag.11)$dev.expl, summary(amo.gamm.vc.lag.12)$dev.expl)

# AIC
aic <- c(AIC(base.gamm), AIC(amo.gamm.lag.0), AIC(amo.gamm.lag.1), AIC(amo.gamm.lag.2), AIC(amo.gamm.lag.3), AIC(amo.gamm.lag.4), AIC(amo.gamm.lag.5), AIC(amo.gamm.lag.6),AIC(amo.gamm.lag.7), AIC(amo.gamm.lag.8), AIC(amo.gamm.lag.9), AIC(amo.gamm.lag.10), AIC(amo.gamm.lag.11), AIC(amo.gamm.lag.12), AIC(amo.gamm.vc.lag.0), AIC(amo.gamm.vc.lag.1), AIC(amo.gamm.vc.lag.2), AIC(amo.gamm.vc.lag.3), AIC(amo.gamm.vc.lag.4), AIC(amo.gamm.vc.lag.5), AIC(amo.gamm.vc.lag.6), AIC(amo.gamm.vc.lag.7), AIC(amo.gamm.vc.lag.8), AIC(amo.gamm.vc.lag.9), AIC(amo.gamm.vc.lag.10), AIC(amo.gamm.vc.lag.11), AIC(amo.gamm.vc.lag.12))

# Delta AIC
aic <- akaike.weights(c(AIC(base.gamm), AIC(amo.gamm.lag.0), AIC(amo.gamm.lag.1), AIC(amo.gamm.lag.2), AIC(amo.gamm.lag.3), AIC(amo.gamm.lag.4), AIC(amo.gamm.lag.5), AIC(amo.gamm.lag.6),AIC(amo.gamm.lag.7), AIC(amo.gamm.lag.8), AIC(amo.gamm.lag.9), AIC(amo.gamm.lag.10), AIC(amo.gamm.lag.11), AIC(amo.gamm.lag.12), AIC(amo.gamm.vc.lag.0), AIC(amo.gamm.vc.lag.1), AIC(amo.gamm.vc.lag.2), AIC(amo.gamm.vc.lag.3), AIC(amo.gamm.vc.lag.4), AIC(amo.gamm.vc.lag.5), AIC(amo.gamm.vc.lag.6), AIC(amo.gamm.vc.lag.7), AIC(amo.gamm.vc.lag.8), AIC(amo.gamm.vc.lag.9), AIC(amo.gamm.vc.lag.10), AIC(amo.gamm.vc.lag.11), AIC(amo.gamm.vc.lag.12)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.gamm), AIC(amo.gamm.lag.0), AIC(amo.gamm.lag.1), AIC(amo.gamm.lag.2), AIC(amo.gamm.lag.3), AIC(amo.gamm.lag.4), AIC(amo.gamm.lag.5), AIC(amo.gamm.lag.6),AIC(amo.gamm.lag.7), AIC(amo.gamm.lag.8), AIC(amo.gamm.lag.9), AIC(amo.gamm.lag.10), AIC(amo.gamm.lag.11), AIC(amo.gamm.lag.12), AIC(amo.gamm.vc.lag.0), AIC(amo.gamm.vc.lag.1), AIC(amo.gamm.vc.lag.2), AIC(amo.gamm.vc.lag.3), AIC(amo.gamm.vc.lag.4), AIC(amo.gamm.vc.lag.5), AIC(amo.gamm.vc.lag.6), AIC(amo.gamm.vc.lag.7), AIC(amo.gamm.vc.lag.8), AIC(amo.gamm.vc.lag.9), AIC(amo.gamm.vc.lag.10), AIC(amo.gamm.vc.lag.11), AIC(amo.gamm.vc.lag.12)))$weights

# Names
names <- c("base.gamm", "amo.gamm.lag.0", "amo.gamm.lag.1", "amo.gamm.lag.2", "amo.gamm.lag.3", "amo.gamm.lag.4", "amo.gamm.lag.5", "amo.gamm.lag.6","amo.gamm.lag.7", "amo.gamm.lag.8", "amo.gamm.lag.9", "amo.gamm.lag.10", "amo.gamm.lag.11", "amo.gamm.lag.12", "amo.gamm.vc.lag.0", "amo.gamm.vc.lag.1", "amo.gamm.vc.lag.2", "amo.gamm.vc.lag.3", "amo.gamm.vc.lag.4", "amo.gamm.vc.lag.5", "amo.gamm.vc.lag.6", "amo.gamm.vc.lag.7", "amo.gamm.vc.lag.8", "amo.gamm.vc.lag.9", "amo.gamm.vc.lag.10", "amo.gamm.vc.lag.11", "amo.gamm.vc.lag.12")

GAMM_Model_Selection_AMO_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic)

save(GAMM_Model_Selection_AMO_Results, file = "GAMM_Model_Selection_AMO_Results.RData")


