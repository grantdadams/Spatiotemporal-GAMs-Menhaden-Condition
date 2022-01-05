load("menhaden_data_post_processing_6_20.RData")
df.menhaden$vessel <- as.factor(df.menhaden$vessel)
library(qpcR)
library(mgcv)
# Complete cases
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("individual.monthly.miss")) == "individual.monthly.miss")
df.menhaden <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam=bam(relative.condition~ s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

#################################
# River Discharge
#################################

# With location
# Non-spatially dependent 
river.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( individual.monthly.miss.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s( individual.monthly.miss.lag1.scaled)  + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s( individual.monthly.miss.lag2.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s( individual.monthly.miss.lag3.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.spring=bam(relative.condition~ s(longitude,latitude) + s(mean.monthly.March.to.May.miss.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.winter=bam(relative.condition~ s(longitude,latitude) + s(miss.discharge.Nov.to.March.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")


#########################
# Without location
# Non-spatially dependent 
river.bam.lag.0.wol=bam(relative.condition~ s( individual.monthly.miss.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.lag.1.wol=bam(relative.condition~ s( individual.monthly.miss.lag1.scaled)  + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.lag.2.wol=bam(relative.condition~ s( individual.monthly.miss.lag2.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.lag.3.wol=bam(relative.condition~ s( individual.monthly.miss.lag3.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.spring.wol=bam(relative.condition~ s(mean.monthly.March.to.May.miss.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")

river.bam.winter.wol=bam(relative.condition~ s(miss.discharge.Nov.to.March.scaled) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML")


#####################
# Summary Stats River
#####################
# Deviance explained
de <- c(summary(base.bam)$dev.expl, summary(spatially.explicit.bam)$dev.expl, summary(river.bam.lag.0)$dev.expl, summary(river.bam.lag.1)$dev.expl, summary(river.bam.lag.2)$dev.expl, summary(river.bam.lag.3)$dev.expl, summary(river.bam.spring)$dev.expl, summary(river.bam.winter)$dev.expl, summary(river.bam.lag.0.wol)$dev.expl, summary(river.bam.lag.1.wol)$dev.expl, summary(river.bam.lag.2.wol)$dev.expl, summary(river.bam.lag.3.wol)$dev.expl, summary(river.bam.spring.wol)$dev.expl, summary(river.bam.winter.wol)$dev.expl)


# Sample size
sample_size <- c(summary(base.bam)$n, summary(spatially.explicit.bam)$n, summary(river.bam.lag.0)$n, summary(river.bam.lag.1)$n, summary(river.bam.lag.2)$n, summary(river.bam.lag.3)$n, summary(river.bam.spring)$n, summary(river.bam.winter)$n, summary(river.bam.lag.0.wol)$n, summary(river.bam.lag.1.wol)$n, summary(river.bam.lag.2.wol)$n, summary(river.bam.lag.3.wol)$n, summary(river.bam.spring.wol)$n, summary(river.bam.winter.wol)$n)

# AIC
aic <- c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(river.bam.lag.0), AIC(river.bam.lag.1), AIC(river.bam.lag.2), AIC(river.bam.lag.3), AIC(river.bam.spring), AIC(river.bam.winter), AIC(river.bam.lag.0.wol), AIC(river.bam.lag.1.wol), AIC(river.bam.lag.2.wol), AIC(river.bam.lag.3.wol), AIC(river.bam.spring.wol), AIC(river.bam.winter.wol))

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(river.bam.lag.0), AIC(river.bam.lag.1), AIC(river.bam.lag.2), AIC(river.bam.lag.3), AIC(river.bam.spring), AIC(river.bam.winter), AIC(river.bam.lag.0.wol), AIC(river.bam.lag.1.wol), AIC(river.bam.lag.2.wol), AIC(river.bam.lag.3.wol), AIC(river.bam.spring.wol), AIC(river.bam.winter.wol)))$deltaAIC

# AIC weights
waic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(river.bam.lag.0), AIC(river.bam.lag.1), AIC(river.bam.lag.2), AIC(river.bam.lag.3), AIC(river.bam.spring), AIC(river.bam.winter), AIC(river.bam.lag.0.wol), AIC(river.bam.lag.1.wol), AIC(river.bam.lag.2.wol), AIC(river.bam.lag.3.wol), AIC(river.bam.spring.wol), AIC(river.bam.winter.wol)))$weights

# Names
names <- c("base.bam", "spatially.explicit.bam", "river.bam.lag.0", "river.bam.lag.1", "river.bam.lag.2", "river.bam.lag.3", "river.bam.spring", "river.bam.winter", "river.bam.lag.0.wol", "river.bam.lag.1.wol", "river.bam.lag.2.wol", "river.bam.lag.3.wol", "river.bam.spring.wol", "river.bam.winter.wol")

bam_Model_Selection_River_Discharge_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, sample_size = sample_size)

save(bam_Model_Selection_River_Discharge_Results, file = "bam_Model_Selection_River_Discharge_Results_ML.RData")

rm(base.bam,  river.bam.lag.0, river.bam.lag.1, river.bam.lag.2, river.bam.lag.3, river.bam.lag.4, river.bam.lag.5, river.bam.lag.6, river.bam.spring, river.bam.winter, river.bam.vc.lag.0, river.bam.vc.lag.1, river.bam.vc.lag.2, river.bam.vc.lag.3, river.bam.vc.lag.4, river.bam.vc.lag.5, river.bam.vc.lag.6, river.bam.vc.spring, river.bam.vc.winter)

