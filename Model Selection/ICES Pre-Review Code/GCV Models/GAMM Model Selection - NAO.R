load("menhaden_data_post_processing_6_20.RData")
df.menhaden$vessel <- as.factor(df.menhaden$vessel)
library(mgcv)
library(qpcR)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("nao")) == "nao")
df.menhaden <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam=bam(relative.condition~  s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log"))

spatially.explicit.bam=bam(relative.condition~ s(longitude,latitude) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log"))

#################################
# nao
#################################
# Non-spatially dependent 
nao.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( nao) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.1)  + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.2) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.3) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.4=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.4) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.5=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.5) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.6=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.6) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.7=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.7)  + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.8=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.8) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.9=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.9) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.10=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.10) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.11=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.11) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.lag.12=bam(relative.condition~ s(longitude,latitude) + s( nao.lag.12) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )


# without locations
nao.bam.wol.lag.0=bam(relative.condition~  s( nao) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.1=bam(relative.condition~  s( nao.lag.1)  + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.2=bam(relative.condition~  s( nao.lag.2) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.3=bam(relative.condition~  s( nao.lag.3) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.4=bam(relative.condition~  s( nao.lag.4) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.5=bam(relative.condition~  s( nao.lag.5) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.6=bam(relative.condition~  s( nao.lag.6) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.7=bam(relative.condition~  s( nao.lag.7)  + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.8=bam(relative.condition~  s( nao.lag.8) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.9=bam(relative.condition~  s( nao.lag.9) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.10=bam(relative.condition~  s( nao.lag.10) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.11=bam(relative.condition~  s( nao.lag.11) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

nao.bam.wol.lag.12=bam(relative.condition~  s( nao.lag.12) + s(month, k = 6)+ factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )


###################
# Summary Stats nao
###################
# Deviance explained
de <- c(summary(base.bam)$dev.expl,summary(spatially.explicit.bam)$dev.expl, summary(nao.bam.lag.0)$dev.expl, summary(nao.bam.lag.1)$dev.expl, summary(nao.bam.lag.2)$dev.expl, summary(nao.bam.lag.3)$dev.expl, summary(nao.bam.lag.4)$dev.expl, summary(nao.bam.lag.5)$dev.expl, summary(nao.bam.lag.6)$dev.expl, summary(nao.bam.lag.7)$dev.expl, summary(nao.bam.lag.8)$dev.expl, summary(nao.bam.lag.9)$dev.expl, summary(nao.bam.lag.10)$dev.expl, summary(nao.bam.lag.11)$dev.expl, summary(nao.bam.lag.12)$dev.expl, summary(nao.bam.wol.lag.0)$dev.expl, summary(nao.bam.wol.lag.1)$dev.expl, summary(nao.bam.wol.lag.2)$dev.expl, summary(nao.bam.wol.lag.3)$dev.expl, summary(nao.bam.wol.lag.4)$dev.expl, summary(nao.bam.wol.lag.5)$dev.expl, summary(nao.bam.wol.lag.6)$dev.expl, summary(nao.bam.wol.lag.7)$dev.expl, summary(nao.bam.wol.lag.8)$dev.expl, summary(nao.bam.wol.lag.9)$dev.expl, summary(nao.bam.wol.lag.10)$dev.expl, summary(nao.bam.wol.lag.11)$dev.expl, summary(nao.bam.wol.lag.12)$dev.expl)

# Sample size
sample_size <- c(summary(base.bam)$n, summary(spatially.explicit.bam)$n, summary(nao.bam.lag.0)$n, summary(nao.bam.lag.1)$n, summary(nao.bam.lag.2)$n, summary(nao.bam.lag.3)$n, summary(nao.bam.lag.4)$n, summary(nao.bam.lag.5)$n, summary(nao.bam.lag.6)$n, summary(nao.bam.lag.7)$n, summary(nao.bam.lag.8)$n, summary(nao.bam.lag.9)$n, summary(nao.bam.lag.10)$n, summary(nao.bam.lag.11)$n, summary(nao.bam.lag.12)$n, summary(nao.bam.wol.lag.0)$n, summary(nao.bam.wol.lag.1)$n, summary(nao.bam.wol.lag.2)$n, summary(nao.bam.wol.lag.3)$n, summary(nao.bam.wol.lag.4)$n, summary(nao.bam.wol.lag.5)$n, summary(nao.bam.wol.lag.6)$n, summary(nao.bam.wol.lag.7)$n, summary(nao.bam.wol.lag.8)$n, summary(nao.bam.wol.lag.9)$n, summary(nao.bam.wol.lag.10)$n, summary(nao.bam.wol.lag.11)$n, summary(nao.bam.wol.lag.12)$n)

# AIC
aic<- c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(nao.bam.lag.0), AIC(nao.bam.lag.1), AIC(nao.bam.lag.2), AIC(nao.bam.lag.3), AIC(nao.bam.lag.4), AIC(nao.bam.lag.5), AIC(nao.bam.lag.6),AIC(nao.bam.lag.7), AIC(nao.bam.lag.8), AIC(nao.bam.lag.9), AIC(nao.bam.lag.10), AIC(nao.bam.lag.11), AIC(nao.bam.lag.12), AIC(nao.bam.wol.lag.0), AIC(nao.bam.wol.lag.1), AIC(nao.bam.wol.lag.2), AIC(nao.bam.wol.lag.3), AIC(nao.bam.wol.lag.4), AIC(nao.bam.wol.lag.5), AIC(nao.bam.wol.lag.6), AIC(nao.bam.wol.lag.7), AIC(nao.bam.wol.lag.8), AIC(nao.bam.wol.lag.9), AIC(nao.bam.wol.lag.10), AIC(nao.bam.wol.lag.11), AIC(nao.bam.wol.lag.12))

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(nao.bam.lag.0), AIC(nao.bam.lag.1), AIC(nao.bam.lag.2), AIC(nao.bam.lag.3), AIC(nao.bam.lag.4), AIC(nao.bam.lag.5), AIC(nao.bam.lag.6),AIC(nao.bam.lag.7), AIC(nao.bam.lag.8), AIC(nao.bam.lag.9), AIC(nao.bam.lag.10), AIC(nao.bam.lag.11), AIC(nao.bam.lag.12), AIC(nao.bam.wol.lag.0), AIC(nao.bam.wol.lag.1), AIC(nao.bam.wol.lag.2), AIC(nao.bam.wol.lag.3), AIC(nao.bam.wol.lag.4), AIC(nao.bam.wol.lag.5), AIC(nao.bam.wol.lag.6), AIC(nao.bam.wol.lag.7), AIC(nao.bam.wol.lag.8), AIC(nao.bam.wol.lag.9), AIC(nao.bam.wol.lag.10), AIC(nao.bam.wol.lag.11), AIC(nao.bam.wol.lag.12)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(nao.bam.lag.0), AIC(nao.bam.lag.1), AIC(nao.bam.lag.2), AIC(nao.bam.lag.3), AIC(nao.bam.lag.4), AIC(nao.bam.lag.5), AIC(nao.bam.lag.6),AIC(nao.bam.lag.7), AIC(nao.bam.lag.8), AIC(nao.bam.lag.9), AIC(nao.bam.lag.10), AIC(nao.bam.lag.11), AIC(nao.bam.lag.12), AIC(nao.bam.wol.lag.0), AIC(nao.bam.wol.lag.1), AIC(nao.bam.wol.lag.2), AIC(nao.bam.wol.lag.3), AIC(nao.bam.wol.lag.4), AIC(nao.bam.wol.lag.5), AIC(nao.bam.wol.lag.6), AIC(nao.bam.wol.lag.7), AIC(nao.bam.wol.lag.8), AIC(nao.bam.wol.lag.9), AIC(nao.bam.wol.lag.10), AIC(nao.bam.wol.lag.11), AIC(nao.bam.wol.lag.12)))$weights

names <- c("base.bam", "spatially.explicit.bam", "nao.bam.lag.0", "nao.bam.lag.1", "nao.bam.lag.2", "nao.bam.lag.3", "nao.bam.lag.4", "nao.bam.lag.5", "nao.bam.lag.6","nao.bam.lag.7", "nao.bam.lag.8", "nao.bam.lag.9", "nao.bam.lag.10", "nao.bam.lag.11", "nao.bam.lag.12", "nao.bam.wol.lag.0", "nao.bam.wol.lag.1", "nao.bam.wol.lag.2", "nao.bam.wol.lag.3", "nao.bam.wol.lag.4", "nao.bam.wol.lag.5", "nao.bam.wol.lag.6", "nao.bam.wol.lag.7", "nao.bam.wol.lag.8", "nao.bam.wol.lag.9", "nao.bam.wol.lag.10", "nao.bam.wol.lag.11", "nao.bam.wol.lag.12")

bam_Model_Selection_NAO_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, sample_size = sample_size)

save(bam_Model_Selection_NAO_Results, file = "bam_Model_Selection_NAO_Results.RData")

rm(base.bam, nao.bam.lag.0, nao.bam.lag.1, nao.bam.lag.2, nao.bam.lag.3, nao.bam.lag.4, nao.bam.lag.5, nao.bam.lag.6,nao.bam.lag.7, nao.bam.lag.8, nao.bam.lag.9, nao.bam.lag.10, nao.bam.lag.11, nao.bam.lag.12, nao.bam.vc.lag.0, nao.bam.vc.lag.1, nao.bam.vc.lag.2, nao.bam.vc.lag.3, nao.bam.vc.lag.4, nao.bam.vc.lag.5, nao.bam.vc.lag.6, nao.bam.vc.lag.7, nao.bam.vc.lag.8, nao.bam.vc.lag.9, nao.bam.vc.lag.10, nao.bam.vc.lag.11, nao.bam.vc.lag.12)

