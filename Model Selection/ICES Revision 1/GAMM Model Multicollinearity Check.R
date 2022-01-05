rm(list = ls())
library(mgcv)

load("menhaden_data_post_processing_6_20.RData")

# Fit full model and get residuals
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag2) + s(latitude, longitude, by = monthly.v.wind.lag2), data = df.menhaden, method = "fREML")

df.menhaden$residuals <- resid(final_model)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

model <- bam(residuals ~ s(SST_9km_lag_0), data= df.menhaden.short, method = "fREML")


cor(df.menhaden[, c("mei.lag.10", "individual.monthly.miss", "monthly.u.wind.lag2", "monthly.v.wind.lag2", "SST_9km_lag_0", "chl_9km_lag_0")], use = "pairwise.complete.obs")
