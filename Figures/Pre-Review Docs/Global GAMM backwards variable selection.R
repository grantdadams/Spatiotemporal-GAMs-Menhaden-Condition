
library(mgcv)
library(qpcR)

global.bam=bam(relative.condition~ s(longitude,latitude) +  s( mei.lag.10) + s(latitude , longitude, by = monthly.u.wind.lag1*monthly.miss.scaled) + s( latitude , longitude, by = monthly.v.wind.lag1) + s( SST_9km_lag_0) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )

# Minus rive
global.bam.mr=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.10) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s( SST_9km_lag_0) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )

# Minus ENSO
global.bam.me=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s( SST_9km_lag_0) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )

# Minus Wind
global.bam.mw=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s( mei.lag.10) + s( SST_9km_lag_0) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )

# Minus SST
global.bam.ms=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s( mei.lag.10) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s(longitude,latitude,by= chl_9km_lag_5 ) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )

# Minus CHL
global.bam.mc=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude, by= monthly.miss.scaled ) + s( mei.lag.10) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s( SST_9km_lag_0) + s(week) + factor(year) + s(collection, bs = "re"), data= df.menhaden.short, method = "REML", family = Gamma(link = "log") )


# compare 
AIC.vec <- c(AIC(global.bam),AIC(global.bam.mr),AIC(global.bam.me),AIC(global.bam.mw),AIC(global.bam.ms),AIC(global.bam.mc))
delta.aic.vec <- akaike.weights(c(AIC(global.bam),AIC(global.bam.mr),AIC(global.bam.me),AIC(global.bam.mw),AIC(global.bam.ms),AIC(global.bam.mc)))$deltaAIC
aic.weights.vec <- akaike.weights(c(AIC(global.bam),AIC(global.bam.mr),AIC(global.bam.me),AIC(global.bam.mw),AIC(global.bam.ms),AIC(global.bam.mc)))$weights

de <- c(summary(global.bam)$dev.expl, summary(global.bam.mr)$dev.expl, summary(global.bam.me)$dev.expl, summary(global.bam.mw)$dev.expl, summary(global.bam.ms)$dev.expl, summary(global.bam.mc)$dev.expl)

global.results <- data.frame(mod = c(("global.bam"),("global.bam.mr"),("global.bam.me"),("global.bam.mw"),("global.bam.ms"),("global.bam.mc")), AIC = AIC.vec, delta.AIC = delta.aic.vec, aic.weight = aic.weights.vec, de= de, relative.de = (de-max(de))*100)
                                  