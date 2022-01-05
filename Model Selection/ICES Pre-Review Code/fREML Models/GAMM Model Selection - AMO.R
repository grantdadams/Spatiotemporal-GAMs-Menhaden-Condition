load("menhaden_data_post_processing_6_20.RData")
df.menhaden$vessel <- as.factor(df.menhaden$vessel)
library(mgcv)
library(qpcR)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("amo")) == "amo")
df.menhaden <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam=bam(relative.condition~  s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log"))

#################################
# amo
#################################
# Non-spatially dependent 
amo.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( amo) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.1)  + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.2) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.3) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.4=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.4) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.5=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.5) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.6=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.6) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.7=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.7)  + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.8=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.8) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.9=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.9) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.10=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.10) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.11=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.11) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.lag.12=bam(relative.condition~ s(longitude,latitude) + s( amo.lag.12) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )


# without locations
amo.bam.wol.lag.0=bam(relative.condition~  s( amo) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.1=bam(relative.condition~  s( amo.lag.1)  + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.2=bam(relative.condition~  s( amo.lag.2) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.3=bam(relative.condition~  s( amo.lag.3) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.4=bam(relative.condition~  s( amo.lag.4) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.5=bam(relative.condition~  s( amo.lag.5) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.6=bam(relative.condition~  s( amo.lag.6) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.7=bam(relative.condition~  s( amo.lag.7)  + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.8=bam(relative.condition~  s( amo.lag.8) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.9=bam(relative.condition~  s( amo.lag.9) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.10=bam(relative.condition~  s( amo.lag.10) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.11=bam(relative.condition~  s( amo.lag.11) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )

amo.bam.wol.lag.12=bam(relative.condition~  s( amo.lag.12) + s(month, k = 6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "fREML", family = Gamma(link = "log") )


###################
# Summary Stats amo
###################
# Deviance explained
de <- c(summary(base.bam)$dev.expl,summary(spatially.explicit.bam)$dev.expl, summary(amo.bam.lag.0)$dev.expl, summary(amo.bam.lag.1)$dev.expl, summary(amo.bam.lag.2)$dev.expl, summary(amo.bam.lag.3)$dev.expl, summary(amo.bam.lag.4)$dev.expl, summary(amo.bam.lag.5)$dev.expl, summary(amo.bam.lag.6)$dev.expl, summary(amo.bam.lag.7)$dev.expl, summary(amo.bam.lag.8)$dev.expl, summary(amo.bam.lag.9)$dev.expl, summary(amo.bam.lag.10)$dev.expl, summary(amo.bam.lag.11)$dev.expl, summary(amo.bam.lag.12)$dev.expl, summary(amo.bam.wol.lag.0)$dev.expl, summary(amo.bam.wol.lag.1)$dev.expl, summary(amo.bam.wol.lag.2)$dev.expl, summary(amo.bam.wol.lag.3)$dev.expl, summary(amo.bam.wol.lag.4)$dev.expl, summary(amo.bam.wol.lag.5)$dev.expl, summary(amo.bam.wol.lag.6)$dev.expl, summary(amo.bam.wol.lag.7)$dev.expl, summary(amo.bam.wol.lag.8)$dev.expl, summary(amo.bam.wol.lag.9)$dev.expl, summary(amo.bam.wol.lag.10)$dev.expl, summary(amo.bam.wol.lag.11)$dev.expl, summary(amo.bam.wol.lag.12)$dev.expl)

# Sample size
sample_size <- c(summary(base.bam)$n, summary(spatially.explicit.bam)$n, summary(amo.bam.lag.0)$n, summary(amo.bam.lag.1)$n, summary(amo.bam.lag.2)$n, summary(amo.bam.lag.3)$n, summary(amo.bam.lag.4)$n, summary(amo.bam.lag.5)$n, summary(amo.bam.lag.6)$n, summary(amo.bam.lag.7)$n, summary(amo.bam.lag.8)$n, summary(amo.bam.lag.9)$n, summary(amo.bam.lag.10)$n, summary(amo.bam.lag.11)$n, summary(amo.bam.lag.12)$n, summary(amo.bam.wol.lag.0)$n, summary(amo.bam.wol.lag.1)$n, summary(amo.bam.wol.lag.2)$n, summary(amo.bam.wol.lag.3)$n, summary(amo.bam.wol.lag.4)$n, summary(amo.bam.wol.lag.5)$n, summary(amo.bam.wol.lag.6)$n, summary(amo.bam.wol.lag.7)$n, summary(amo.bam.wol.lag.8)$n, summary(amo.bam.wol.lag.9)$n, summary(amo.bam.wol.lag.10)$n, summary(amo.bam.wol.lag.11)$n, summary(amo.bam.wol.lag.12)$n)

# AIC
aic<- c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(amo.bam.lag.0), AIC(amo.bam.lag.1), AIC(amo.bam.lag.2), AIC(amo.bam.lag.3), AIC(amo.bam.lag.4), AIC(amo.bam.lag.5), AIC(amo.bam.lag.6),AIC(amo.bam.lag.7), AIC(amo.bam.lag.8), AIC(amo.bam.lag.9), AIC(amo.bam.lag.10), AIC(amo.bam.lag.11), AIC(amo.bam.lag.12), AIC(amo.bam.wol.lag.0), AIC(amo.bam.wol.lag.1), AIC(amo.bam.wol.lag.2), AIC(amo.bam.wol.lag.3), AIC(amo.bam.wol.lag.4), AIC(amo.bam.wol.lag.5), AIC(amo.bam.wol.lag.6), AIC(amo.bam.wol.lag.7), AIC(amo.bam.wol.lag.8), AIC(amo.bam.wol.lag.9), AIC(amo.bam.wol.lag.10), AIC(amo.bam.wol.lag.11), AIC(amo.bam.wol.lag.12))

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(amo.bam.lag.0), AIC(amo.bam.lag.1), AIC(amo.bam.lag.2), AIC(amo.bam.lag.3), AIC(amo.bam.lag.4), AIC(amo.bam.lag.5), AIC(amo.bam.lag.6),AIC(amo.bam.lag.7), AIC(amo.bam.lag.8), AIC(amo.bam.lag.9), AIC(amo.bam.lag.10), AIC(amo.bam.lag.11), AIC(amo.bam.lag.12), AIC(amo.bam.wol.lag.0), AIC(amo.bam.wol.lag.1), AIC(amo.bam.wol.lag.2), AIC(amo.bam.wol.lag.3), AIC(amo.bam.wol.lag.4), AIC(amo.bam.wol.lag.5), AIC(amo.bam.wol.lag.6), AIC(amo.bam.wol.lag.7), AIC(amo.bam.wol.lag.8), AIC(amo.bam.wol.lag.9), AIC(amo.bam.wol.lag.10), AIC(amo.bam.wol.lag.11), AIC(amo.bam.wol.lag.12)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(amo.bam.lag.0), AIC(amo.bam.lag.1), AIC(amo.bam.lag.2), AIC(amo.bam.lag.3), AIC(amo.bam.lag.4), AIC(amo.bam.lag.5), AIC(amo.bam.lag.6),AIC(amo.bam.lag.7), AIC(amo.bam.lag.8), AIC(amo.bam.lag.9), AIC(amo.bam.lag.10), AIC(amo.bam.lag.11), AIC(amo.bam.lag.12), AIC(amo.bam.wol.lag.0), AIC(amo.bam.wol.lag.1), AIC(amo.bam.wol.lag.2), AIC(amo.bam.wol.lag.3), AIC(amo.bam.wol.lag.4), AIC(amo.bam.wol.lag.5), AIC(amo.bam.wol.lag.6), AIC(amo.bam.wol.lag.7), AIC(amo.bam.wol.lag.8), AIC(amo.bam.wol.lag.9), AIC(amo.bam.wol.lag.10), AIC(amo.bam.wol.lag.11), AIC(amo.bam.wol.lag.12)))$weights

names <- c("base.bam", "spatially.explicit.bam", "amo.bam.lag.0", "amo.bam.lag.1", "amo.bam.lag.2", "amo.bam.lag.3", "amo.bam.lag.4", "amo.bam.lag.5", "amo.bam.lag.6","amo.bam.lag.7", "amo.bam.lag.8", "amo.bam.lag.9", "amo.bam.lag.10", "amo.bam.lag.11", "amo.bam.lag.12", "amo.bam.wol.lag.0", "amo.bam.wol.lag.1", "amo.bam.wol.lag.2", "amo.bam.wol.lag.3", "amo.bam.wol.lag.4", "amo.bam.wol.lag.5", "amo.bam.wol.lag.6", "amo.bam.wol.lag.7", "amo.bam.wol.lag.8", "amo.bam.wol.lag.9", "amo.bam.wol.lag.10", "amo.bam.wol.lag.11", "amo.bam.wol.lag.12")

bam_Model_Selection_AMO_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, sample_size = sample_size)

save(bam_Model_Selection_AMO_Results, file = "bam_Model_Selection_AMO_Results_fREML.RData")

rm(base.bam, amo.bam.lag.0, amo.bam.lag.1, amo.bam.lag.2, amo.bam.lag.3, amo.bam.lag.4, amo.bam.lag.5, amo.bam.lag.6,amo.bam.lag.7, amo.bam.lag.8, amo.bam.lag.9, amo.bam.lag.10, amo.bam.lag.11, amo.bam.lag.12, amo.bam.vc.lag.0, amo.bam.vc.lag.1, amo.bam.vc.lag.2, amo.bam.vc.lag.3, amo.bam.vc.lag.4, amo.bam.vc.lag.5, amo.bam.vc.lag.6, amo.bam.vc.lag.7, amo.bam.vc.lag.8, amo.bam.vc.lag.9, amo.bam.vc.lag.10, amo.bam.vc.lag.11, amo.bam.vc.lag.12)

