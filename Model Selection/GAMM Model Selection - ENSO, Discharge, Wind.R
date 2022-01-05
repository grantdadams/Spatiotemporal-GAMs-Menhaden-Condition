rm(list=ls())
load("menhaden_data_post_processing_6_20.RData")
library(mgcv)
library(qpcR)
library(readxl)

# Which variables do we want
dis.vars <- c("individual.monthly.miss.scaled", paste("individual.monthly.miss.lag", 1:2, ".scaled", sep = ""),"mean.monthly.March.to.May.miss.scaled", "miss.discharge.Nov.to.March.scaled")
mei.vars <- c("mei",paste("mei.lag.", 1:12, sep = ""))
uwind.vars <- c(paste("monthly.u.wind.lag", 0:2, sep = ""))
vwind.vars <- c(paste("monthly.v.wind.lag", 0:2, sep = ""))

# How do we model them
dis.vars <- c(paste("s(", dis.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", dis.vars, ")", sep = ""))
mei.vars <- c(paste("s(", mei.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", mei.vars, ")", sep = ""))
wind.vars <- c(paste("s(", uwind.vars, ") + ","s(", vwind.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", uwind.vars, ") + ", "s(latitude , longitude, by = ", vwind.vars, ")", sep = ""))

# Find all possible combinations of models

all.model.vars.list <- list(list(dis.vars, mei.vars, wind.vars), list(dis.vars, wind.vars, mei.vars), list(mei.vars, dis.vars, wind.vars), list(mei.vars, wind.vars, dis.vars), list(wind.vars, dis.vars, mei.vars), list(wind.vars, mei.vars, dis.vars))

n_mods <-  length(c(dis.vars, mei.vars, wind.vars)) + 4

results <- data.frame(Formula = rep(NA, n_mods), LogLik = rep(NA, n_mods), EDF = rep(NA, n_mods),  AIC = rep(NA, n_mods), BIC = rep(NA, n_mods), DEV_EXPL = rep(NA, n_mods))

results <- list(results, results, results, results, results, results)

for(k in 1:length(results)){
  
  all.model.vars <- all.model.vars.list[[k]]
  
  # Indexing variables

  mod_set <- c(rep(0,4), rep(1, length(all.model.vars[[1]])), rep(2, length(all.model.vars[[2]])), rep(3, length(all.model.vars[[3]])))
  mod_subset <- c(rep(0,4), 1:length(all.model.vars[[1]]), 1:length(all.model.vars[[2]]), 1:length(all.model.vars[[3]]))
  mod_ind <- data.frame(ID = 1:n_mods, mod_set, mod_subset)
  
  # OBjects to store results
  model.formulas <- list()
  models <- list()
  
  # Create the model formula and run, if BIC lowers add variable
  model.formulas[[1]] <- formula(paste("relative.condition ~ 1"))
  model.formulas[[2]] <- formula(paste("relative.condition ~ factor(year)"))
  ind = 1
  
  for(i in 1:n_mods){
    if(i < 5) {print(paste("Fitting model",i))}
    if(i %in% c(1, 2)){
      models[[i]] <- lm(model.formulas[[i]], data= df.menhaden)
    } 
    if(i %in% c(3, 4)){
      model.formulas[[i]] <- formula(paste(c(model.formulas[[i-1]]), c("s(month, k =6)", "s(longitude,latitude)")[i-2], sep =" + "))
      models[[i]] <- bam(model.formulas[[i]], data = df.menhaden, method = "fREML")
      temp.mod <- models[[i]]
    }
    # Fit with environmental predictors
    if(i > 4 ){
      print(paste("Fitting model", mod_set[i] + 4,"submodel", mod_subset[i]))
      model.formulas[[i]] <- formula(paste(c(formula(temp.mod)), c(all.model.vars[[mod_set[i]]][mod_subset[i]]), sep =" + "))
      models[[i]] <- bam(model.formulas[[i]], data = df.menhaden, method = "fREML")
    }
    
    
    # Extract results
    results[[k]][i,1] <- as.character(formula(models[[i]]))[[3]]
    results[[k]][i,2] <- as.numeric(logLik(models[[i]]))
    if(i < 3){results[[k]][i,3] <- length(coef(models[[i]]))} # Number of parameters for LM
    if(i > 2){results[[k]][i,3] <- sum(models[[i]]$edf2)} # Number of parameters for GAM
    results[[k]][i,4] <- AIC(models[[i]])
    results[[k]][i,5] <- BIC(models[[i]])
    if(i > 2){results[[k]][i,6] <- summary(models[[i]])$dev.expl * 100}
    
    # Reasign temp model
    last_model <- max(mod_ind$mod_subset[which(mod_ind$mod_set == mod_ind$mod_set[i])]) # Is it the final model in the subset?
    if(mod_ind$mod_subset[i] == last_model & i > 4){
      temp.mod <- models[[which(results[[k]][which(mod_ind$ID <= i), "BIC"] == min(results[[k]][which(mod_ind$ID <= i), "BIC"], na.rm = T))]]
    }
  }
}

# Switch names
names <- data.frame(read_xlsx("name_switch.xlsx", sheet = 1))
for(i in 1:length(results)){
  for(j in 1:nrow(names)){
    results[[i]][,"Formula"] <- gsub(names[j,1], names[j,2], results[[i]][,"Formula"], fixed = T)
  }
}



 # Fit the models
write.csv(results, file = "bam_model_selection_all_v2.csv")

# -------------------------------------------------------------
# Fit SST and CHL
final.mod <- temp.mod
save(final.mod, file = "final_large_model.RData")
# Get residuals and subset data
df.menhaden$residuals <- resid(final.mod)
ind.miss.cols <- which(substr(colnames(df.menhaden), start = 0, stop = nchar("SST_9km_lag")) == "SST_9km_lag")
df.menhaden.short <- df.menhaden[complete.cases(df.menhaden[,ind.miss.cols]),]

# Which variables do we want
sst.vars <- c(paste("SST_9km_lag_", 0:4, sep = ""))
chl.vars <- c(paste("chl_9km_lag_", 0:4, sep = ""))

# How do we model them
sst.vars <- c(paste("s(", sst.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", sst.vars, ")", sep = ""))
chl.vars <- c(paste("s(", chl.vars, ")", sep = ""), paste("s(latitude , longitude, by = ", chl.vars, ")", sep = ""))

# Find all possible combinations of models
all.model.vars <- list(sst.vars, chl.vars)

# Indexing variables
n_mods <-  length(c(sst.vars, chl.vars)) + 1
mod_set <- c(0, rep(1, length(sst.vars)), rep(2, length(chl.vars)))
mod_subset <- c(0, 1:length(sst.vars), 1:length(chl.vars))
mod_ind <- data.frame(ID = 1:n_mods, mod_set, mod_subset)

# OBjects to store results
results <- data.frame(Formula = rep(NA, n_mods), LogLik = rep(NA, n_mods), EDF = rep(NA, n_mods),  AIC = rep(NA, n_mods), BIC = rep(NA, n_mods), DEV_EXPL = rep(NA, n_mods))
model.formulas <- list()
models <- list()

# Create the model formula and run, if BIC lowers add variable
model.formulas[[1]] <- formula(paste("relative.condition ~ 1"))
models[[1]] <- lm(model.formulas[[1]], data= df.menhaden.short)
temp.mod <- models[[1]] 

for(i in 1:n_mods){
  print(paste("Fitting model", mod_set[i],"submodel", mod_subset[i]))
  if(i > 1){
    model.formulas[[i]] <- formula(paste(c(formula(temp.mod)), c(all.model.vars[[mod_set[i]]][mod_subset[i]]), sep =" + "))
    models[[i]] <- bam(model.formulas[[i]], data = df.menhaden.short, method = "fREML")
  }
  # Extract results
  results[i,1] <- as.character(formula(models[[i]]))[[3]]
  results[i,2] <- as.numeric(logLik(models[[i]]))
  if(i < 3){results[i,3] <- length(coef(models[[i]]))} # Number of parameters for LM
  if(i > 2){results[i,3] <- sum(models[[i]]$edf2)} # Number of parameters for GAM
  results[i,4] <- AIC(models[[i]])
  results[i,5] <- BIC(models[[i]])
  if(i > 1){results[i,6] <- summary(models[[i]])$dev.expl * 100}
  
  # Reasign temp model
  last_model <- max(mod_ind$mod_subset[which(mod_ind$mod_set == mod_ind$mod_set[i])]) # Is it the final model in the subset?
  if(mod_ind$mod_subset[i] == last_model & i > 1){
    temp.mod <- models[[which(results[which(mod_ind$ID <= i),"AIC"] == min(results[which(mod_ind$ID <= i), "AIC"], na.rm = T))]]
  }
}
save(temp.mod, file = "final_short_model.RData")
write.csv(results, file = "bam_model_SST_CHL_forward_selection.csv")

