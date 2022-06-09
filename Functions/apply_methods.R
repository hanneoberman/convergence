# functions to apply missing data methods

# complete case analysis
apply_CCA <- function(amp) {
  # list-wise deletion
  est <- na.omit(amp$amp) %>% 
    # fit regression 
    lm(Y ~ X1 + X2 + X3 + X4, .) %>% 
    # clean results
    broom::tidy(conf.int = TRUE) %>% 
    # choose estimates
    select(term, estimate, conf.low, conf.high) %>% 
    # add method name and missingness
    cbind(it = 0, method = "CCA", mech = amp$mech, prop = amp$prop, .) 
  # output
  return(est)
}

# MICE imputation
apply_MICE <- function(amp, n_it) {
  # imputation with MICE
  imp1 <- mice::mice(amp$amp, method = "norm", maxit = 1, printFlag = FALSE)
  imps <- list(imp1, estimate_param(imp1))
  imps2 <- mice::mice.mids(imps[[1]], printFlag = FALSE)
  imps <- mice::mice.mids(imps)
  # fit regression on each imputation
  # est <- with(imp, lm(Y ~ X1 + X2 + X3 + X4)) %>% 
  #   # pool results
  #   mice::pool() %>% 
  #   # clean results
  #   broom::tidy(conf.int = TRUE) %>% 
  #   # select estimates
  #   select(term, estimate, conf.low, conf.high) %>% 
  #   # add simulation conditions
  #   cbind(method = "MICE", mech = amp$mech, prop = amp$prop, .)
  est <- estimate_param(imp1)
  ests <- cbind(method = "MICE", mech = amp$mech, prop = amp$prop, est)
  # output
  return(ests)
}

estimate_param <- function(imp, it = 1) {
  est <- with(imp, lm(Y ~ X1 + X2 + X3 + X4)) %>% 
    # pool results
    mice::pool() %>% 
    # clean results
    broom::tidy(conf.int = TRUE) %>% 
    # select estimates
    select(term, estimate, conf.low, conf.high) %>% 
    # add simulation conditions
    cbind(.it = it, .)
  return(est)
}

# combine into one function
apply_methods <- function(amps, betas, n_it) {
  # apply CCA to each incomplete dataset
  CCA <- purrr::map_dfr(amps, ~{apply_CCA(.)})
  # impute with MICE and estimate effects
  MICE <-  purrr::map_dfr(amps, ~{apply_MICE(., n_it)})
  # impute with Python and estimate effects
  ### [YOUR FUNCTION HERE] ###
  # combine estimates 
  ests <- rbind(CCA, MICE) %>% 
    cbind(truth = c(0, betas))
  # output
  return(ests)
}
