evaluation <- function(implist){
  # for CCA
  if (is.data.frame(implist$imp)) {
    fit <- lm(X1 ~ ., implist$imp)
    rmse_pred <- rmse(fit$residuals)
  }
  # for actual imputation methods
  if (mice::is.mids(implist$imp)) {
    mira <- mice::complete(implist$imp, "all") %>% 
      purrr::map(~lm(X1 ~ ., .))
    fit <- suppressWarnings(mice::pool(mira))
    rmse_pred <- purrr::map_dbl(mira, ~rmse(.$residuals)) %>% mean()
  }
  # get estimated regression coefficients
  est <- fit %>% broom::tidy(conf.int = TRUE) %>%
    .[2, c("estimate", "conf.low", "conf.high")] 
  # output
  out <- data.frame(
    n_obs = implist$n_obs,
    n_var = implist$n_var,
    corr = implist$corr,
    mis_mech = implist$mis_mech,
    mis_type = implist$mis_type,
    mis_prop = implist$mis_prop,    
    imp_meth = implist$imp_meth,
    n_imp = implist$n_imp,
    n_it = implist$n_it,
    average = implist$average, 
    coeff = est$estimate, 
    ci_ll = est$conf.low,
    ci_ul = est$conf.high,
    rmse_pred = rmse_pred, 
    rmse_cell = implist$rmse_cell
  )
  return(out)
}