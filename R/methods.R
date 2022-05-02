imputation <- function(amplist, meth = "norm", m = 5, it = 5) {
  # for CCA
  if(meth == "none") {
    imp <- na.omit(amplist$amp$amp)
    average <- mean(imp$X1)
    rmse_cell <- NA
  } 
  # for actual imputation methods
  if (meth != "none") {
    if (meth == "mean" | meth == "norm.predict") {
      imp <- mice::mice(
        amplist$amp$amp,
        m = 1,
        method = meth,
        maxit = 1,
        print = FALSE
      )
    } else {
      imp <- mice::mice(
        amplist$amp$amp,
        m = m,
        method = meth,
        maxit = it,
        print = FALSE
      )
    }
    average <- mice::complete(imp, "long") %>% .$X1 %>% mean()
    rmse_cell <- apply(imp$imp$X1 - amplist$amp$data$X1[imp$where[, "X1"]], 2, rmse) %>% mean()
  }
  # output
  out <- list(
    n_obs = amplist$n_obs,
    n_var = amplist$n_var,
    corr = amplist$corr,
    mis_mech = amplist$mis_mech,
    mis_type = amplist$mis_type,
    mis_prop = amplist$mis_prop,    
    imp_meth = meth,
    n_imp = m,
    n_it = it,
    average = average,
    rmse_cell = rmse_cell,
    imp = imp
  )
  return(out)
}