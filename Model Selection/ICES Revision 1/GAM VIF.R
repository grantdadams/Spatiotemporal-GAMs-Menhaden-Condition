vif.gam <- function(object){
  
  obj.sum <- mgcv::summary.gam(object)
  
  s2 <- object$sig2 # estimate of standard deviation of residuals
  X <- object$model # data used to fit the model
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- obj.sum$p.table[v,2]^2 # variance in estimates
  selected_col <- row.names(obj.sum$p.table)[v]
  selected_col <- gsub("TRUE", "", selected_col)
  varXj <- apply(X=X[, selected_col],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j
  
  VIF.df <- tibble(variable=names(VIF),
                   vif=VIF)
  
  return(VIF.df)
}