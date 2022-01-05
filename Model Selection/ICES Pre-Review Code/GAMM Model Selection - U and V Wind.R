load("menhaden_data_post_processing_6_20.RData")
df.menhaden$vessel <- as.factor(df.menhaden$vessel)


library(mgcv)
library(qpcR)


ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("monthly.u.wind.")) == "monthly.u.wind.")
df.menhaden <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam=bam(relative.condition~  s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))


#################################
# wind
#################################
# Non-spatially dependent 
wind.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.lag0) + s(monthly.v.wind.lag0) + s(month, k = 6) + factor(year), data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.lag1) + s(monthly.v.wind.lag1) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.lag2) + s(monthly.v.wind.lag2) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s(monthly.u.wind.lag3) + s(monthly.v.wind.lag3) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

# Spatially dependent
wind.bam.vc.lag.0=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag0) + s( latitude , longitude, by = monthly.v.wind.lag0) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.vc.lag.1=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.vc.lag.2=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag2) + s( latitude , longitude, by = monthly.v.wind.lag2) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.vc.lag.3=bam(relative.condition~ s(longitude,latitude) + s(latitude , longitude, by = monthly.u.wind.lag3) + s( latitude , longitude, by = monthly.v.wind.lag3) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

# Without location
# Non-spatially dependent 
wind.bam.lag.0.wol=bam(relative.condition~  s(monthly.u.wind.lag0) + s(monthly.v.wind.lag0) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.lag.1.wol=bam(relative.condition~  s(monthly.u.wind.lag1) + s(monthly.v.wind.lag1) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.lag.2.wol=bam(relative.condition~  s(monthly.u.wind.lag2) + s(monthly.v.wind.lag2) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.lag.3.wol=bam(relative.condition~  s(monthly.u.wind.lag3) + s(monthly.v.wind.lag3) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

# Spatially dependent
wind.bam.vc.lag.0.wol=bam(relative.condition~  s(latitude , longitude, by = monthly.u.wind.lag0) + s( latitude , longitude, by = monthly.v.wind.lag0) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.vc.lag.1.wol=bam(relative.condition~  s(latitude , longitude, by = monthly.u.wind.lag1) + s( latitude , longitude, by = monthly.v.wind.lag1) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

wind.bam.vc.lag.2.wol=bam(relative.condition~  s(latitude , longitude, by = monthly.u.wind.lag2) + s( latitude , longitude, by = monthly.v.wind.lag2) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "ML", family = Gamma(link = "log"))

wind.bam.vc.lag.3.wol=bam(relative.condition~  s(latitude , longitude, by = monthly.u.wind.lag3) + s( latitude , longitude, by = monthly.v.wind.lag3) + s(month, k = 6) + factor(year) , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

###################
# Summary Stats wind
###################
# Deviance explained
de <- c(summary(base.bam)$dev.expl, summary(spatially.explicit.bam)$dev.expl, summary(wind.bam.lag.0)$dev.expl, summary(wind.bam.lag.1)$dev.expl, summary(wind.bam.lag.2)$dev.expl, summary(wind.bam.lag.3)$dev.expl, summary(wind.bam.vc.lag.0)$dev.expl, summary(wind.bam.vc.lag.1)$dev.expl, summary(wind.bam.vc.lag.2)$dev.expl, summary(wind.bam.vc.lag.3)$dev.expl, summary(wind.bam.lag.0.wol)$dev.expl, summary(wind.bam.lag.1.wol)$dev.expl, summary(wind.bam.lag.2.wol)$dev.expl, summary(wind.bam.lag.3.wol)$dev.expl, summary(wind.bam.vc.lag.0.wol)$dev.expl, summary(wind.bam.vc.lag.1.wol)$dev.expl, NA, summary(wind.bam.vc.lag.3.wol)$dev.expl)

# sample size
sample_size <- c(summary(base.bam)$n, summary(spatially.explicit.bam)$n, summary(wind.bam.lag.0)$n, summary(wind.bam.lag.1)$n, summary(wind.bam.lag.2)$n, summary(wind.bam.lag.3)$n, summary(wind.bam.vc.lag.0)$n, summary(wind.bam.vc.lag.1)$n, summary(wind.bam.vc.lag.2)$n, summary(wind.bam.vc.lag.3)$n, summary(wind.bam.lag.0.wol)$n, summary(wind.bam.lag.1.wol)$n, summary(wind.bam.lag.2.wol)$n, summary(wind.bam.lag.3.wol)$n, summary(wind.bam.vc.lag.0.wol)$n, summary(wind.bam.vc.lag.1.wol)$n, summary(wind.bam.vc.lag.2.wol)$n, summary(wind.bam.vc.lag.3.wol)$n)

# AIC
aic <- c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(wind.bam.lag.0), AIC(wind.bam.lag.1), AIC(wind.bam.lag.2), AIC(wind.bam.lag.3), AIC(wind.bam.vc.lag.0), AIC(wind.bam.vc.lag.1), AIC(wind.bam.vc.lag.2), AIC(wind.bam.vc.lag.3), AIC(wind.bam.lag.0.wol), AIC(wind.bam.lag.1.wol), AIC(wind.bam.lag.2.wol), AIC(wind.bam.lag.3.wol), AIC(wind.bam.vc.lag.0.wol), AIC(wind.bam.vc.lag.1.wol), NA, AIC(wind.bam.vc.lag.3.wol))

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(wind.bam.lag.0), AIC(wind.bam.lag.1), AIC(wind.bam.lag.2), AIC(wind.bam.lag.3), AIC(wind.bam.vc.lag.0), AIC(wind.bam.vc.lag.1), AIC(wind.bam.vc.lag.2), AIC(wind.bam.vc.lag.3), AIC(wind.bam.lag.0.wol), AIC(wind.bam.lag.1.wol), AIC(wind.bam.lag.2.wol), AIC(wind.bam.lag.3.wol), AIC(wind.bam.vc.lag.0.wol), AIC(wind.bam.vc.lag.1.wol), AIC(wind.bam.vc.lag.1.wol), AIC(wind.bam.vc.lag.3.wol)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(wind.bam.lag.0), AIC(wind.bam.lag.1), AIC(wind.bam.lag.2), AIC(wind.bam.lag.3), AIC(wind.bam.vc.lag.0), AIC(wind.bam.vc.lag.1), AIC(wind.bam.vc.lag.2), AIC(wind.bam.vc.lag.3), AIC(wind.bam.lag.0.wol), AIC(wind.bam.lag.1.wol), AIC(wind.bam.lag.2.wol), AIC(wind.bam.lag.3.wol), AIC(wind.bam.vc.lag.0.wol), AIC(wind.bam.vc.lag.1.wol), AIC(wind.bam.vc.lag.1.wol), AIC(wind.bam.vc.lag.3.wol)))$weights

# Names
names <- c("base.bam", "spatially.explicit.bam", "wind.bam.lag.0", "wind.bam.lag.1", "wind.bam.lag.2", "wind.bam.lag.3", "wind.bam.vc.lag.0", "wind.bam.vc.lag.1", "wind.bam.vc.lag.2", "wind.bam.vc.lag.3", "wind.bam.lag.0.wol", "wind.bam.lag.1.wol", "wind.bam.lag.2.wol", "wind.bam.lag.3.wol", "wind.bam.vc.lag.0.wol", "wind.bam.vc.lag.1.wol", "wind.bam.vc.lag.2.wol", "wind.bam.vc.lag.3.wol")

bam_Model_Selection_Wind_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, sample_size =sample_size)

save(bam_Model_Selection_Wind_Results, file = "bam_Model_Selection_Wind_Results_ML.RData")

rm(base.bam.short, spatially.explicit.bam.short, wind.bam.lag.0, wind.bam.lag.1, wind.bam.lag.2, wind.bam.lag.3, wind.bam.vc.lag.0, wind.bam.vc.lag.1, wind.bam.vc.lag.2, wind.bam.vc.lag.3, wind.bam.lag.0.wol, wind.bam.lag.1.wol, wind.bam.lag.2.wol, wind.bam.lag.3.wol, wind.bam.vc.lag.0.wol, wind.bam.vc.lag.1.wol, wind.bam.vc.lag.2.wol, wind.bam.vc.lag.3.wol)

