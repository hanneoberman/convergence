generation <- function(r = 0.4, n = 500, p = 2) {
  # generate data based on variance-covariance matrix
  vcov <- matrix(r, p, p)
  diag(vcov) <- 1
  dat <- data.frame(
    mvtnorm::rmvnorm(
      n,
      mean = rep(0, p),
      sigma = vcov
    )) %>% setNames(paste0("X", 1:p))
  # output
  out <- list(
    n_obs = n,
    n_var = p,
    corr = r,
    dat = dat)
  return(out)
}

amputation <- function(datlist, mech = "MAR", type = "MID", prop = 0.25) {
  # induce missingness
  amp <- mice::ampute(datlist$dat, 
                      prop = prop,
                      mech = mech,
                      type = type)
  # output
  out <- list(
    n_obs = datlist$n_obs,
    n_var = datlist$n_var,
    corr = datlist$corr,
    mis_mech = mech,
    mis_type = type,
    mis_prop = prop,
    amp = amp)
  return(out)
}