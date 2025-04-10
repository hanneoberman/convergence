# functions to apply missing data methods

# comparative truth, fully observed data
apply_full <- function(amp) {
  # list-wise deletion
  est <- # fit regression 
    lm(Y ~ X1 + X2 + X3 + X4, data = amp$data) |> 
    # clean results
    broom::tidy(conf.int = TRUE) |> 
    # choose estimates
    dplyr::select(term, estimate, conf.low, conf.high)
  # add method name and missingness
  est <- cbind(method = "full", mech = amp$mech, prop = amp$prop, .it = 0, est, ac_mean = NA, psrf_mean = NA, ac_sd = NA, psrf_sd = NA)
  # rename "(Intercept)" to "Y" for easier processing
  est[est$term == "(Intercept)", "term"] <- "Y"
  # output
  return(est)
}

# complete case analysis
apply_CCA <- function(amp) {
  # list-wise deletion
  est <- # fit regression 
    lm(Y ~ X1 + X2 + X3 + X4, data = na.omit(amp$amp)) |> 
    # clean results
    broom::tidy(conf.int = TRUE) |> 
    # choose estimates
    dplyr::select(term, estimate, conf.low, conf.high)
  # add method name and missingness
  est <- cbind(method = "CCA", mech = amp$mech, prop = amp$prop, .it = 0, est, ac_mean = NA, psrf_mean = NA, ac_sd = NA, psrf_sd = NA)
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
  conv_sd <- mice::convergence(implist[[1]], parameter = "sd")
  conv$ac_sd <- conv_sd$ac
  conv$psrf_sd <- conv_sd$psrf
  conv_sorted <- conv[order(conv$.it, conv$vrb), ]
  
  # output
  ests <- cbind(
    method = "MICE", 
    mech = amp$mech, 
    prop = amp$prop, 
    implist[[2]], 
    ac_mean = conv_sorted$ac, 
    psrf_mean = conv_sorted$psrf,
    ac_sd = conv_sorted$ac_sd, 
    psrf_sd = conv_sorted$psrf_sd)
  return(ests)
}

# internal function to calculate pooled regression estimates
estimate_param <- function(imp) {
  # run analysis on all imputations
  est <- with(imp, lm(Y ~ X1 + X2 + X3 + X4)) |> 
    # pool results
    mice::pool() |> 
    # clean results
    broom::tidy(conf.int = TRUE) |> 
    # select estimates
    dplyr::select(term, estimate, conf.low, conf.high)
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
  # comparative truth on complete dataset
  full <- purrr::map_dfr(amps, ~{apply_full(.x)})
  # apply CCA to each incomplete dataset
  CCA <- purrr::map_dfr(amps, ~{apply_CCA(.x)})
  # impute with MICE and estimate effects
  MICE <- purrr::map_dfr(amps, ~{apply_MICE(.x, n_it)})
  # combine estimates 
  ests <- rbind(full, CCA, MICE) |> 
    cbind(truth = c(0, betas))
  row.names(ests) <- NULL
  # output
  return(ests)
}

# # internal function to compute the autocorrelation
# calculate_ac <- function(x){
#   t = length(x)
#   ac <- c(NA, dplyr::cummean(dplyr::coalesce(
#       purrr::map_dbl(2:t, function(.itr) {
#         suppressWarnings(stats::cor(
#           x[.itr - 1], 
#           x[.itr], 
#           use = "pairwise.complete.obs"))
#       }), 0))) + 0 * x
# }
# 
# # convergence parameters
# collect_theta <- function(imp) {
#  a <-  mice::complete(imp, "all")
#  purrr::map_dbl(a, ~{princomp(., cor = TRUE) %>% .$sdev %>% .[1] %>% . ^ 2})
# }

# multivar_metrics <- function(implist){
# # compute AC
# ac <-
#   purrr::map(vrbs, function(.vrb) {
#     c(NA, dplyr::cummean(dplyr::coalesce(
#       purrr::map_dbl(2:t, function(.itr) {
#         suppressWarnings(stats::cor(
#           param[[.vrb]][.itr - 1, ],
#           param[[.vrb]][.itr, ],
#           use = "pairwise.complete.obs"
#         ))
#       }), 0
#     ))) + 0 * param[[.vrb]][, 1]
#   })
# 
# # compute PSRF
# psrf <- purrr::map_dfr(param, ~ {
#   purrr::map_dfr(1:t, function(.itr) {
#     data.frame(psrf = rstan::Rhat(.[1:.itr, ]))
#   })
# })
# }