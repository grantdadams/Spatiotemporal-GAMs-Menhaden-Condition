load("menhaden_data_post_processing_6_20.RData")
df.menhaden$vessel <- as.factor(df.menhaden$vessel)
library(mgcv)
library(qpcR)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam.short=bam(relative.condition~  s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

base.bam.short2=bam(relative.condition~  s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML")

spatially.explicit.bam.short=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

#################################
# SST
#################################
# Non-spatially dependent 
sst.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_0) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_1)  + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_2) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s( SST_9km_lag_3) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))


# Spatially dependent
sst.bam.vc.lag.0=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_0 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.vc.lag.1=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_1 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_2 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.vc.lag.3=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= SST_9km_lag_3 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

# Without location
# Non-spatially dependent 
sst.bam.lag.0.wol=bam(relative.condition~  s( SST_9km_lag_0) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.lag.1.wol=bam(relative.condition~  s( SST_9km_lag_1)  + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.lag.2.wol=bam(relative.condition~  s( SST_9km_lag_2) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.lag.3.wol=bam(relative.condition~  s( SST_9km_lag_3) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))


# Spatially dependent
sst.bam.vc.lag.0.wol=bam(relative.condition~  s(longitude,latitude,by= SST_9km_lag_0 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.vc.lag.1.wol=bam(relative.condition~  s(longitude,latitude,by= SST_9km_lag_1 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.vc.lag.2.wol=bam(relative.condition~  s(longitude,latitude,by= SST_9km_lag_2 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

sst.bam.vc.lag.3.wol=bam(relative.condition~  s(longitude,latitude,by= SST_9km_lag_3 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))



###################
# Summary Stats SST
###################
# Deviance explained
# spatially.explicit.bam.short
de <- c(summary(base.bam.short)$dev.expl, summary(spatially.explicit.bam.short)$dev.expl, summary(sst.bam.lag.0)$dev.expl, summary(sst.bam.lag.1)$dev.expl, summary(sst.bam.lag.2)$dev.expl, summary(sst.bam.lag.3)$dev.expl, summary(sst.bam.vc.lag.0)$dev.expl, summary(sst.bam.vc.lag.1)$dev.expl, summary(sst.bam.vc.lag.2)$dev.expl, summary(sst.bam.vc.lag.3)$dev.expl, summary(sst.bam.lag.0.wol)$dev.expl, summary(sst.bam.lag.1.wol)$dev.expl, summary(sst.bam.lag.2.wol)$dev.expl, summary(sst.bam.lag.3.wol)$dev.expl, summary(sst.bam.vc.lag.0.wol)$dev.expl, summary(sst.bam.vc.lag.1.wol)$dev.expl, summary(sst.bam.vc.lag.2.wol)$dev.expl, summary(sst.bam.vc.lag.3.wol)$dev.expl)

# sample size
sample_size <- c(summary(base.bam.short)$n, summary(spatially.explicit.bam.short)$n, summary(sst.bam.lag.0)$n, summary(sst.bam.lag.1)$n, summary(sst.bam.lag.2)$n, summary(sst.bam.lag.3)$n, summary(sst.bam.vc.lag.0)$n, summary(sst.bam.vc.lag.1)$n, summary(sst.bam.vc.lag.2)$n, summary(sst.bam.vc.lag.3)$n, summary(sst.bam.lag.0.wol)$n, summary(sst.bam.lag.1.wol)$n, summary(sst.bam.lag.2.wol)$n, summary(sst.bam.lag.3.wol)$n, summary(sst.bam.vc.lag.0.wol)$n, summary(sst.bam.vc.lag.1.wol)$n, summary(sst.bam.vc.lag.2.wol)$n, summary(sst.bam.vc.lag.3.wol)$n)

# AIC
aic <- c(AIC(base.bam.short), AIC(spatially.explicit.bam.short), AIC(sst.bam.lag.0), AIC(sst.bam.lag.1), AIC(sst.bam.lag.2), AIC(sst.bam.lag.3), AIC(sst.bam.vc.lag.0), AIC(sst.bam.vc.lag.1), AIC(sst.bam.vc.lag.2), AIC(sst.bam.vc.lag.3), AIC(sst.bam.lag.0.wol), AIC(sst.bam.lag.1.wol), AIC(sst.bam.lag.2.wol), AIC(sst.bam.lag.3.wol), AIC(sst.bam.vc.lag.0.wol), AIC(sst.bam.vc.lag.1.wol), AIC(sst.bam.vc.lag.2.wol), AIC(sst.bam.vc.lag.3.wol))

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam.short), AIC(spatially.explicit.bam.short), AIC(sst.bam.lag.0), AIC(sst.bam.lag.1), AIC(sst.bam.lag.2), AIC(sst.bam.lag.3), AIC(sst.bam.vc.lag.0), AIC(sst.bam.vc.lag.1), AIC(sst.bam.vc.lag.2), AIC(sst.bam.vc.lag.3), AIC(sst.bam.lag.0.wol), AIC(sst.bam.lag.1.wol), AIC(sst.bam.lag.2.wol), AIC(sst.bam.lag.3.wol), AIC(sst.bam.vc.lag.0.wol), AIC(sst.bam.vc.lag.1.wol), AIC(sst.bam.vc.lag.2.wol), AIC(sst.bam.vc.lag.3.wol)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.bam.short), AIC(spatially.explicit.bam.short), AIC(sst.bam.lag.0), AIC(sst.bam.lag.1), AIC(sst.bam.lag.2), AIC(sst.bam.lag.3), AIC(sst.bam.vc.lag.0), AIC(sst.bam.vc.lag.1), AIC(sst.bam.vc.lag.2), AIC(sst.bam.vc.lag.3), AIC(sst.bam.lag.0.wol), AIC(sst.bam.lag.1.wol), AIC(sst.bam.lag.2.wol), AIC(sst.bam.lag.3.wol), AIC(sst.bam.vc.lag.0.wol), AIC(sst.bam.vc.lag.1.wol), AIC(sst.bam.vc.lag.2.wol), AIC(sst.bam.vc.lag.3.wol)))$weights

# Names
names <- c("base.bam.short", "spatially.explicit.bam.short", "sst.bam.lag.0", "sst.bam.lag.1", "sst.bam.lag.2", "sst.bam.lag.3", "sst.bam.vc.lag.0", "sst.bam.vc.lag.1", "sst.bam.vc.lag.2", "sst.bam.vc.lag.3", "sst.bam.lag.0.wol", "sst.bam.lag.1.wol", "sst.bam.lag.2.wol", "sst.bam.lag.3.wol", "sst.bam.vc.lag.0.wol", "sst.bam.vc.lag.1.wol", "sst.bam.vc.lag.2.wol", "sst.bam.vc.lag.3.wol")

bam_Model_Selection_SST_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, sample_size = sample_size)

save(bam_Model_Selection_SST_Results, file = "bam_Model_Selection_SST_Results_ml.RData")

rm(base.bam.short, spatially.explicit.bam.short, sst.bam.lag.0, sst.bam.lag.1, sst.bam.lag.2, sst.bam.lag.3, sst.bam.vc.lag.0, sst.bam.vc.lag.1, sst.bam.vc.lag.2, sst.bam.vc.lag.3, sst.bam.lag.0.wol, sst.bam.lag.1.wol, sst.bam.lag.2.wol, sst.bam.lag.3.wol, sst.bam.vc.lag.0.wol, sst.bam.vc.lag.1.wol, sst.bam.vc.lag.2.wol, sst.bam.vc.lag.3.wol)



#######################
# Chlorophyll
#######################
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("chl_9km_lag")) == "chl_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam.short=bam(relative.condition~  s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

spatially.explicit.bam.short=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

#################################
# chl
#################################
# Non-spatially dependent 
chl.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_0) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_1)  + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_2) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s( chl_9km_lag_3) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))


# Spatially dependent
chl.bam.vc.lag.0=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_0 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.1=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_1 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_2 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.3=bam(relative.condition~ s(longitude,latitude) + s(longitude,latitude,by= chl_9km_lag_3 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

# Without location
# Non-spatially dependent 
chl.bam.lag.0.wol=bam(relative.condition~  s( chl_9km_lag_0) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.lag.1.wol=bam(relative.condition~  s( chl_9km_lag_1)  + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.lag.2.wol=bam(relative.condition~  s( chl_9km_lag_2) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.lag.3.wol=bam(relative.condition~  s( chl_9km_lag_3) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))


# Spatially dependent
chl.bam.vc.lag.0.wol=bam(relative.condition~  s(longitude,latitude,by= chl_9km_lag_0 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.1.wol=bam(relative.condition~  s(longitude,latitude,by= chl_9km_lag_1 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.2.wol=bam(relative.condition~  s(longitude,latitude,by= chl_9km_lag_2 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))

chl.bam.vc.lag.3.wol=bam(relative.condition~  s(longitude,latitude,by= chl_9km_lag_3 ) + s(month, k = 6) + factor(year), data= df.menhaden.short, method = "fREML", family = Gamma(link = "log"))



###################
# Summary Stats chl
###################
# Deviance explained
# spatially.explicit.bam.short
de <- c(summary(base.bam.short)$dev.expl, summary(spatially.explicit.bam.short)$dev.expl, summary(chl.bam.lag.0)$dev.expl, summary(chl.bam.lag.1)$dev.expl, summary(chl.bam.lag.2)$dev.expl, summary(chl.bam.lag.3)$dev.expl, summary(chl.bam.vc.lag.0)$dev.expl, summary(chl.bam.vc.lag.1)$dev.expl, summary(chl.bam.vc.lag.2)$dev.expl, summary(chl.bam.vc.lag.3)$dev.expl, summary(chl.bam.lag.0.wol)$dev.expl, summary(chl.bam.lag.1.wol)$dev.expl, summary(chl.bam.lag.2.wol)$dev.expl, summary(chl.bam.lag.3.wol)$dev.expl, summary(chl.bam.vc.lag.0.wol)$dev.expl, summary(chl.bam.vc.lag.1.wol)$dev.expl, summary(chl.bam.vc.lag.2.wol)$dev.expl, summary(chl.bam.vc.lag.3.wol)$dev.expl)

# sample size
sample_size <- c(summary(base.bam.short)$n, summary(spatially.explicit.bam.short)$n, summary(chl.bam.lag.0)$n, summary(chl.bam.lag.1)$n, summary(chl.bam.lag.2)$n, summary(chl.bam.lag.3)$n, summary(chl.bam.vc.lag.0)$n, summary(chl.bam.vc.lag.1)$n, summary(chl.bam.vc.lag.2)$n, summary(chl.bam.vc.lag.3)$n, summary(chl.bam.lag.0.wol)$n, summary(chl.bam.lag.1.wol)$n, summary(chl.bam.lag.2.wol)$n, summary(chl.bam.lag.3.wol)$n, summary(chl.bam.vc.lag.0.wol)$n, summary(chl.bam.vc.lag.1.wol)$n, summary(chl.bam.vc.lag.2.wol)$n, summary(chl.bam.vc.lag.3.wol)$n)

# AIC
aic <- c(AIC(base.bam.short), AIC(spatially.explicit.bam.short), AIC(chl.bam.lag.0), AIC(chl.bam.lag.1), AIC(chl.bam.lag.2), AIC(chl.bam.lag.3), AIC(chl.bam.vc.lag.0), AIC(chl.bam.vc.lag.1), AIC(chl.bam.vc.lag.2), AIC(chl.bam.vc.lag.3), AIC(chl.bam.lag.0.wol), AIC(chl.bam.lag.1.wol), AIC(chl.bam.lag.2.wol), AIC(chl.bam.lag.3.wol), AIC(chl.bam.vc.lag.0.wol), AIC(chl.bam.vc.lag.1.wol), AIC(chl.bam.vc.lag.2.wol), AIC(chl.bam.vc.lag.3.wol))

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam.short), AIC(spatially.explicit.bam.short), AIC(chl.bam.lag.0), AIC(chl.bam.lag.1), AIC(chl.bam.lag.2), AIC(chl.bam.lag.3), AIC(chl.bam.vc.lag.0), AIC(chl.bam.vc.lag.1), AIC(chl.bam.vc.lag.2), AIC(chl.bam.vc.lag.3), AIC(chl.bam.lag.0.wol), AIC(chl.bam.lag.1.wol), AIC(chl.bam.lag.2.wol), AIC(chl.bam.lag.3.wol), AIC(chl.bam.vc.lag.0.wol), AIC(chl.bam.vc.lag.1.wol), AIC(chl.bam.vc.lag.2.wol), AIC(chl.bam.vc.lag.3.wol)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.bam.short), AIC(spatially.explicit.bam.short), AIC(chl.bam.lag.0), AIC(chl.bam.lag.1), AIC(chl.bam.lag.2), AIC(chl.bam.lag.3), AIC(chl.bam.vc.lag.0), AIC(chl.bam.vc.lag.1), AIC(chl.bam.vc.lag.2), AIC(chl.bam.vc.lag.3), AIC(chl.bam.lag.0.wol), AIC(chl.bam.lag.1.wol), AIC(chl.bam.lag.2.wol), AIC(chl.bam.lag.3.wol), AIC(chl.bam.vc.lag.0.wol), AIC(chl.bam.vc.lag.1.wol), AIC(chl.bam.vc.lag.2.wol), AIC(chl.bam.vc.lag.3.wol)))$weights

# Names
names <- c("base.bam.short", "spatially.explicit.bam.short", "chl.bam.lag.0", "chl.bam.lag.1", "chl.bam.lag.2", "chl.bam.lag.3", "chl.bam.vc.lag.0", "chl.bam.vc.lag.1", "chl.bam.vc.lag.2", "chl.bam.vc.lag.3", "chl.bam.lag.0.wol", "chl.bam.lag.1.wol", "chl.bam.lag.2.wol", "chl.bam.lag.3.wol", "chl.bam.vc.lag.0.wol", "chl.bam.vc.lag.1.wol", "chl.bam.vc.lag.2.wol", "chl.bam.vc.lag.3.wol")

bam_Model_Selection_chl_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, sample_size = sample_size)

save(bam_Model_Selection_chl_Results, file = "bam_Model_Selection_chl_Results_ml.RData")

rm(base.bam.short, spatially.explicit.bam.short, chl.bam.lag.0, chl.bam.lag.1, chl.bam.lag.2, chl.bam.lag.3, chl.bam.vc.lag.0, chl.bam.vc.lag.1, chl.bam.vc.lag.2, chl.bam.vc.lag.3, chl.bam.lag.0.wol, chl.bam.lag.1.wol, chl.bam.lag.2.wol, chl.bam.lag.3.wol, chl.bam.vc.lag.0.wol, chl.bam.vc.lag.1.wol, chl.bam.vc.lag.2.wol, chl.bam.vc.lag.3.wol)