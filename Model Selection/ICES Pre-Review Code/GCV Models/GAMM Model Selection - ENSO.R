load("menhaden_data_post_processing_6_20.RData")
df.menhaden$vessel <- as.factor(df.menhaden$vessel)
library(mgcv)
library(qpcR)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("mei")) == "mei")
df.menhaden <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

base.bam=bam(relative.condition~  s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log"))

spatially.explicit.bam2=bam(relative.condition~ s(longitude,latitude) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log"))

#################################
# meis
#################################
# Non-spatially dependent 
mei.bam.lag.0=bam(relative.condition~ s(longitude,latitude) + s( mei) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.1=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.1)  + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.2=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.2) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.3=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.3) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.4=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.4) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.5=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.5) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.6=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.6) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.7=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.7)  + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.8=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.8) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.9=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.9) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.10=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.10) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.11=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.11) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.lag.12=bam(relative.condition~ s(longitude,latitude) + s( mei.lag.12) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )


# without locations
mei.bam.wol.lag.0=bam(relative.condition~  s( mei) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.1=bam(relative.condition~  s( mei.lag.1)  + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.2=bam(relative.condition~  s( mei.lag.2) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.3=bam(relative.condition~  s( mei.lag.3) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.4=bam(relative.condition~  s( mei.lag.4) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.5=bam(relative.condition~  s( mei.lag.5) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.6=bam(relative.condition~  s( mei.lag.6) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.7=bam(relative.condition~  s( mei.lag.7)  + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.8=bam(relative.condition~  s( mei.lag.8) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.9=bam(relative.condition~  s( mei.lag.9) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.10=bam(relative.condition~  s( mei.lag.10) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.11=bam(relative.condition~  s( mei.lag.11) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )

mei.bam.wol.lag.12=bam(relative.condition~  s( mei.lag.12) + s(month, k =6) + factor(year) + s(vessel, bs = "re") , data= df.menhaden, method = "GCV.Cp", family = Gamma(link = "log") )


###################
# Summary Stats MEI
###################
# Deviance explained
de <- c(summary(base.bam)$dev.expl,summary(spatially.explicit.bam)$dev.expl, summary(mei.bam.lag.0)$dev.expl, summary(mei.bam.lag.1)$dev.expl, summary(mei.bam.lag.2)$dev.expl, summary(mei.bam.lag.3)$dev.expl, summary(mei.bam.lag.4)$dev.expl, summary(mei.bam.lag.5)$dev.expl, summary(mei.bam.lag.6)$dev.expl, summary(mei.bam.lag.7)$dev.expl, summary(mei.bam.lag.8)$dev.expl, summary(mei.bam.lag.9)$dev.expl, summary(mei.bam.lag.10)$dev.expl, summary(mei.bam.lag.11)$dev.expl, summary(mei.bam.lag.12)$dev.expl, summary(mei.bam.wol.lag.0)$dev.expl, summary(mei.bam.wol.lag.1)$dev.expl, summary(mei.bam.wol.lag.2)$dev.expl, summary(mei.bam.wol.lag.3)$dev.expl, summary(mei.bam.wol.lag.4)$dev.expl, summary(mei.bam.wol.lag.5)$dev.expl, summary(mei.bam.wol.lag.6)$dev.expl, summary(mei.bam.wol.lag.7)$dev.expl, summary(mei.bam.wol.lag.8)$dev.expl, summary(mei.bam.wol.lag.9)$dev.expl, summary(mei.bam.wol.lag.10)$dev.expl, summary(mei.bam.wol.lag.11)$dev.expl, summary(mei.bam.wol.lag.12)$dev.expl)

# Sample size
sample_size <- c(summary(base.bam)$n, summary(spatially.explicit.bam)$n, summary(mei.bam.lag.0)$n, summary(mei.bam.lag.1)$n, summary(mei.bam.lag.2)$n, summary(mei.bam.lag.3)$n, summary(mei.bam.lag.4)$n, summary(mei.bam.lag.5)$n, summary(mei.bam.lag.6)$n, summary(mei.bam.lag.7)$n, summary(mei.bam.lag.8)$n, summary(mei.bam.lag.9)$n, summary(mei.bam.lag.10)$n, summary(mei.bam.lag.11)$n, summary(mei.bam.lag.12)$n, summary(mei.bam.wol.lag.0)$n, summary(mei.bam.wol.lag.1)$n, summary(mei.bam.wol.lag.2)$n, summary(mei.bam.wol.lag.3)$n, summary(mei.bam.wol.lag.4)$n, summary(mei.bam.wol.lag.5)$n, summary(mei.bam.wol.lag.6)$n, summary(mei.bam.wol.lag.7)$n, summary(mei.bam.wol.lag.8)$n, summary(mei.bam.wol.lag.9)$n, summary(mei.bam.wol.lag.10)$n, summary(mei.bam.wol.lag.11)$n, summary(mei.bam.wol.lag.12)$n)

# AIC
aic<- c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(mei.bam.lag.0), AIC(mei.bam.lag.1), AIC(mei.bam.lag.2), AIC(mei.bam.lag.3), AIC(mei.bam.lag.4), AIC(mei.bam.lag.5), AIC(mei.bam.lag.6),AIC(mei.bam.lag.7), AIC(mei.bam.lag.8), AIC(mei.bam.lag.9), AIC(mei.bam.lag.10), AIC(mei.bam.lag.11), AIC(mei.bam.lag.12), AIC(mei.bam.wol.lag.0), AIC(mei.bam.wol.lag.1), AIC(mei.bam.wol.lag.2), AIC(mei.bam.wol.lag.3), AIC(mei.bam.wol.lag.4), AIC(mei.bam.wol.lag.5), AIC(mei.bam.wol.lag.6), AIC(mei.bam.wol.lag.7), AIC(mei.bam.wol.lag.8), AIC(mei.bam.wol.lag.9), AIC(mei.bam.wol.lag.10), AIC(mei.bam.wol.lag.11), AIC(mei.bam.wol.lag.12))

# GCV Score
gcv.score<- c(summary(base.bam)$sp.criterion, summary(spatially.explicit.bam)$sp.criterion, summary(mei.bam.lag.0)$sp.criterion, summary(mei.bam.lag.1)$sp.criterion, summary(mei.bam.lag.2)$sp.criterion, summary(mei.bam.lag.3)$sp.criterion, summary(mei.bam.lag.4)$sp.criterion, summary(mei.bam.lag.5)$sp.criterion, summary(mei.bam.lag.6)$sp.criterion,summary(mei.bam.lag.7)$sp.criterion, summary(mei.bam.lag.8)$sp.criterion, summary(mei.bam.lag.9)$sp.criterion, summary(mei.bam.lag.10)$sp.criterion, summary(mei.bam.lag.11)$sp.criterion, summary(mei.bam.lag.12)$sp.criterion, summary(mei.bam.wol.lag.0)$sp.criterion, summary(mei.bam.wol.lag.1)$sp.criterion, summary(mei.bam.wol.lag.2)$sp.criterion, summary(mei.bam.wol.lag.3)$sp.criterion, summary(mei.bam.wol.lag.4)$sp.criterion, summary(mei.bam.wol.lag.5)$sp.criterion, summary(mei.bam.wol.lag.6)$sp.criterion, summary(mei.bam.wol.lag.7)$sp.criterion, summary(mei.bam.wol.lag.8)$sp.criterion, summary(mei.bam.wol.lag.9)$sp.criterion, summary(mei.bam.wol.lag.10)$sp.criterion, summary(mei.bam.wol.lag.11)$sp.criterion, summary(mei.bam.wol.lag.12)$sp.criterion)

# Delta AIC
daic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(mei.bam.lag.0), AIC(mei.bam.lag.1), AIC(mei.bam.lag.2), AIC(mei.bam.lag.3), AIC(mei.bam.lag.4), AIC(mei.bam.lag.5), AIC(mei.bam.lag.6),AIC(mei.bam.lag.7), AIC(mei.bam.lag.8), AIC(mei.bam.lag.9), AIC(mei.bam.lag.10), AIC(mei.bam.lag.11), AIC(mei.bam.lag.12), AIC(mei.bam.wol.lag.0), AIC(mei.bam.wol.lag.1), AIC(mei.bam.wol.lag.2), AIC(mei.bam.wol.lag.3), AIC(mei.bam.wol.lag.4), AIC(mei.bam.wol.lag.5), AIC(mei.bam.wol.lag.6), AIC(mei.bam.wol.lag.7), AIC(mei.bam.wol.lag.8), AIC(mei.bam.wol.lag.9), AIC(mei.bam.wol.lag.10), AIC(mei.bam.wol.lag.11), AIC(mei.bam.wol.lag.12)))$deltaAIC

# AIC Weights
waic <- akaike.weights(c(AIC(base.bam), AIC(spatially.explicit.bam), AIC(mei.bam.lag.0), AIC(mei.bam.lag.1), AIC(mei.bam.lag.2), AIC(mei.bam.lag.3), AIC(mei.bam.lag.4), AIC(mei.bam.lag.5), AIC(mei.bam.lag.6),AIC(mei.bam.lag.7), AIC(mei.bam.lag.8), AIC(mei.bam.lag.9), AIC(mei.bam.lag.10), AIC(mei.bam.lag.11), AIC(mei.bam.lag.12), AIC(mei.bam.wol.lag.0), AIC(mei.bam.wol.lag.1), AIC(mei.bam.wol.lag.2), AIC(mei.bam.wol.lag.3), AIC(mei.bam.wol.lag.4), AIC(mei.bam.wol.lag.5), AIC(mei.bam.wol.lag.6), AIC(mei.bam.wol.lag.7), AIC(mei.bam.wol.lag.8), AIC(mei.bam.wol.lag.9), AIC(mei.bam.wol.lag.10), AIC(mei.bam.wol.lag.11), AIC(mei.bam.wol.lag.12)))$weights

names <- c("base.bam", "spatially.explicit.bam", "mei.bam.lag.0", "mei.bam.lag.1", "mei.bam.lag.2", "mei.bam.lag.3", "mei.bam.lag.4", "mei.bam.lag.5", "mei.bam.lag.6","mei.bam.lag.7", "mei.bam.lag.8", "mei.bam.lag.9", "mei.bam.lag.10", "mei.bam.lag.11", "mei.bam.lag.12", "mei.bam.wol.lag.0", "mei.bam.wol.lag.1", "mei.bam.wol.lag.2", "mei.bam.wol.lag.3", "mei.bam.wol.lag.4", "mei.bam.wol.lag.5", "mei.bam.wol.lag.6", "mei.bam.wol.lag.7", "mei.bam.wol.lag.8", "mei.bam.wol.lag.9", "mei.bam.wol.lag.10", "mei.bam.wol.lag.11", "mei.bam.wol.lag.12")

bam_Model_Selection_MEI_Results <- data.frame( model_name = names , deviance_explained = de, AIC = aic, Delta_AIC = daic, AIC_weight = waic, gcv.score = gcv.score, sample_size = sample_size)

save(bam_Model_Selection_MEI_Results, file = "bam_Model_Selection_MEI_Results.RData")

rm(base.bam, mei.bam.lag.0, mei.bam.lag.1, mei.bam.lag.2, mei.bam.lag.3, mei.bam.lag.4, mei.bam.lag.5, mei.bam.lag.6,mei.bam.lag.7, mei.bam.lag.8, mei.bam.lag.9, mei.bam.lag.10, mei.bam.lag.11, mei.bam.lag.12, mei.bam.wol.lag.0, mei.bam.wol.lag.1, mei.bam.wol.lag.2, mei.bam.wol.lag.3, mei.bam.wol.lag.4, mei.bam.wol.lag.5, mei.bam.wol.lag.6, mei.bam.wol.lag.7, mei.bam.wol.lag.8, mei.bam.wol.lag.9, mei.bam.wol.lag.10, mei.bam.wol.lag.11, mei.bam.wol.lag.12)
