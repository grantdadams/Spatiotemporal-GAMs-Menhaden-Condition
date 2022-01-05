rm(list=ls())
load("menhaden_data_post_processing_6_20.RData")
library(mgcv)
library(qpcR)

# Which variables do we want
mei.vars <- c("mei",paste("mei.lag.", 1:12, sep = ""))
dis.vars <- c("individual.monthly.miss.scaled", paste("individual.monthly.miss.lag", 1:2, ".scaled", sep = ""),"mean.monthly.March.to.May.miss.scaled", "miss.discharge.Nov.to.March.scaled")
uwind.vars <- c(paste("monthly.u.wind.lag", 0:2, sep = ""))
vwind.vars <- c(paste("monthly.v.wind.lag", 0:2, sep = ""))

# How do we model them
mei.vars <- c(1, paste("s(", mei.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", mei.vars, ")", sep = ""))
dis.vars <- c(1, paste("s(", dis.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", dis.vars, ")", sep = ""))
wind.vars <- c(1, paste("s(", uwind.vars, ") + ","s(", vwind.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", uwind.vars, ") + ", "s(latitude , longitude, by = ", vwind.vars, ")", sep = ""))

# Find all possible combinations of models
all.model.combinations <- expand.grid(mei.vars, dis.vars, wind.vars)

# Create the model formula
model.formulas <- list()
for(i in 1:nrow(all.model.combinations)){
  model.formulas[[i]] <- formula(paste("relative.condition", "~ s(month, k =6) + factor(year) + s(longitude,latitude) +", paste(all.model.combinations[i,1], all.model.combinations[i,2], all.model.combinations[i,3], sep =" + ")))
}

# Fit the models
library(mgcv)
model.fits <- vector(mode="list", length(model.formulas))
results <- data.frame(Formula = rep(NA, length(model.formulas)), AIC = rep(NA, length(model.formulas)), BIC = rep(NA, length(model.formulas)), DEV_EXPL = rep(NA, length(model.formulas)))
results <- as.matrix(results)
start.time <- Sys.time()
for(i in 1:length(model.formulas)){
  print(paste("Fitting model",i))
  temp.mod <- bam(model.formulas[[i]], data= df.menhaden, method = "fREML", discrete = T, samfrac = 0.1)
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

write.csv(results, file = "bam_model_selection_all_combinations.csv")
