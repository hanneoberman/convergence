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
    cbind(method = "CCA", mech = amp$mech, prop = amp$prop, .it = 0, ., ac = NA, psrf = NA) 
  # rename "(Intercept)" to "Y" for easier processing
  est[est$term == "(Intercept)", "term"] <- "Y"
  # output
  return(est)
}

# MICE imputation
apply_MICE <- function(amp, n_it) {
  # first imputation with MICE
  imp1 <- mice::mice(amp$amp, method = "norm", maxit = 1, printFlag = FALSE)
  # add regression estimates
  implist <- list(imp1, cbind(.it = 1, estimate_param(imp1)))
  # iterate and estimate
  for (i in 2:n_it) {
    implist <- add_iteration(implist)
  }
  # calculate convergence diagnostics
  conv <- mice::convergence(implist[[1]])
  conv_sorted <- conv[order(conv$.it, conv$vrb), ]
  # output
  ests <- cbind(method = "MICE", mech = amp$mech, prop = amp$prop, implist[[2]], ac = conv_sorted$ac, psrf = conv_sorted$psrf)
  return(ests)
}

# internal function to calculate pooled regression estimates
estimate_param <- function(imp) {
  est <- with(imp, lm(Y ~ X1 + X2 + X3 + X4)) %>% 
    # pool results
    mice::pool() %>% 
    # clean results
    broom::tidy(conf.int = TRUE) %>% 
    # select estimates
    select(term, estimate, conf.low, conf.high)
  # rename "(Intercept)" to "Y" for easier processing
  est[est$term == "(Intercept)", "term"] <- "Y"
  # output
  return(est)
}

# internal function to iterate with MICE
add_iteration <- function(implist) {
  # iterate
  imp <- mice::mice.mids(implist[[1]], printFlag = FALSE) 
  # estimate
  est <- cbind(.it = imp$iteration, estimate_param(imp))
  # output
  implist <<- list(imp, rbind(implist[[2]], est))
}

# combine into one function
apply_methods <- function(amps, betas, n_it) {
  # apply CCA to each incomplete dataset
  CCA <- purrr::map_dfr(amps, ~{apply_CCA(.)})
  # impute with MICE and estimate effects
  MICE <-  purrr::map_dfr(amps, ~{apply_MICE(., n_it)})
  # combine estimates 
  ests <- rbind(CCA, MICE) %>% 
    cbind(truth = c(0, betas))
  # output
  return(ests)
}
