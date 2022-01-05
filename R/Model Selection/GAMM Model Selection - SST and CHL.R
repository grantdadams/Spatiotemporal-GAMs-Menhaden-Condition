rm(list=ls())
load("Data/Gulf_Menhaden_Data.RData")
library(mgcv)
library(qpcR)

# Fit full model and get residuals
final_model=bam(relative.condition ~ s(month, k = 6) + factor(year) + s(longitude, latitude) + s(mei.lag.10) + s(individual.monthly.miss.scaled) + s(latitude, longitude, by = monthly.u.wind.lag2) + s(latitude, longitude, by = monthly.v.wind.lag2), data = df.menhaden, method = "fREML")

df.menhaden$residuals <- resid(final_model)

ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

# Which variables do we want
sst.vars <- c(paste("SST_9km_lag_", 0:2, sep = ""))
chl.vars <- c(paste("chl_9km_lag_", 0:2, sep = ""))

# How do we model them
sst.vars <- c(1, paste("s(", sst.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", sst.vars, ")", sep = ""))
chl.vars <- c(1, paste("s(", chl.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", chl.vars, ")", sep = ""))

# Find all possible combinations of models
all.model.combinations <- expand.grid(sst.vars, chl.vars)

# Create the model formula
model.formulas <- list()
for(i in 1:nrow(all.model.combinations)){
  model.formulas[[i]] <- formula(paste("residuals", "~", paste(all.model.combinations[i,1], all.model.combinations[i,2], sep =" + ")))
}

# Fit the models
model.fits <- vector(mode="list", length(model.formulas))
results <- data.frame(Formula = rep(NA, length(model.formulas)), AIC = rep(NA, length(model.formulas)), BIC = rep(NA, length(model.formulas)), DEV_EXPL = rep(NA, length(model.formulas)))
results <- as.matrix(results)
start.time <- Sys.time()
for(i in 2:length(model.formulas)){
  print(paste("Fitting model",i))
  temp.mod <- bam(model.formulas[[i]], data= df.menhaden.short, method = "fREML", discrete = T, samfrac = 0.1)
  results[i,1] <- as.character(model.formulas[[i]])[[3]]
  results[i,2] <- AIC(temp.mod)
  results[i,3] <- BIC(temp.mod)
  results[i,4] <- summary(temp.mod)$dev.expl
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results <- as.data.frame(results)
results <- cbind(all.model.combinations, results)

write.csv(results, file = "bam_model_SST_CHL_selection_all_combinations.csv")
