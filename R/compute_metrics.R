# functions to calculate PSRF and AC
extract_thetas <- function(imp, parameter = "mean") {
  # extract the theta values from the MICE object
  vrbs <- names(data$data)
  if (parameter == "mean") {
    thetas <- lapply(vrbs, function(vrb) {
      aperm(data$chainMean[vrbs, , , drop = FALSE], c(2L, 3L, 1L))[, , vrb]
    })
  }
  if (parameter == "sd" | parameter == "variance") {
    thetas <- lapply(vrbs, function(vrb) {
      aperm(sqrt(data$chainVar)[vrbs, , , drop = FALSE], c(2, 3, 1))[, , vrb]
    })
  }
  names(thetas) <- vrbs
  return(thetas)
}

# compute PSRF, original and with Vehtari et al. (2019) modifications
calculate_psrf <- function(chains, type = "original") {
  # type == c("original", "bulk", "tail", "max")
  # do not compute on variables without imputations
  none_imputed <- all(is.nan(chains))
  if (none_imputed) {
    return(NA)
  }
  if (!none_imputed) {
    if (type == "original" | type == "max") {
      # original cf. Gelman and Rubin (1992)
      rhat <- rhat_original <- psrf_chain(chains) # original
    }
    if (type == "bulk" | type == "max") {
      # adjusted cf. Vehtari et al. (2019)
      rhat <- rhat_bulk <- chains |>
        split_chains() |>
        z_scale() |>
        psrf_chain() # bulk
    }
    if (type == "tail" | type == "max") {
      rhat <- rhat_tail <- chains |>
        fold_chains() |>
        split_chains() |>
        z_scale() |>
        psrf_chain() # tail
    }
    if (type == "max") {
      # return the maximum of the three per iteration
      # max(rhat_original, rhat_bulk, rhat_tail, na.rm = TRUE)
      max(rhat_bulk, rhat_tail, na.rm = TRUE)
    }
  }
  return(rhat)
}


# fold chains for rhat of tails
fold_chains <- function(chains) {
  # fold Markov chain for rhat of tails, adapted from rstan
  abs(chains - median(chains))
}

# split chains with maxit > 4 to detect trending
split_chains <- function(chains) {
  # split Markov chains, adapted from {rstan}
  
  # number of iterations
  tau <- dim(chains)[1]
  
  # do not split if result will be chains of length 1
  if (tau < 4)
    return(chains)
  else {
    # split each chain to get 2 * m chains
    lower <- 1:floor(tau / 2)
    upper <- ceiling((tau / 2) + 1):tau
    splits <- base::cbind(chains[lower, ], chains[upper, ])
    return(splits)
  }
}

# rank-normalize chains
z_scale <- function(chains) {
  # rank-normalize Markov chain, adapted from {rstan}
  m_it <- length(chains)
  r <- rank(chains, ties.method = 'average')
  z <- stats::qnorm((r - 1 / 2) / m_it)
  
  # output
  if (!is.null(dim(chains))) {
    # output should have the input dimensions
    z <- array(z, dim = dim(chains), dimnames = dimnames(chains))
  }
  return(z)
}

# compute rhat for 1 iteration for 1 variable
psrf_metric <- function(chains) {
  # compute potential scale reduction factor (rhat) for each variable in mids object
  # equations adapted from Vehtari et al. (2019)
  
  # number of iterations
  tau <- nrow(chains)
  
  # between chain variance
  var_between <-
    tau * var(apply(chains, 2, mean))
  
  # within chain variance
  var_within <- mean(apply(chains, 2, var))
  
  # rhat
  rhat <-
    sqrt((var_between / var_within + tau - 1) / tau)
  
  # output
  return(rhat)
}

# apply to each iteration
psrf_chain <- function(chains) {
  tau <- nrow(chains)
  rhats <- numeric(tau)
  for (it in seq_len(tau)) {
    rhats[it] <- psrf_metric(chains[1:it, , drop = FALSE])
  }
  return(rhats)
}

# test setup for dev
library(mice)
data <- mice(nhanes, maxit = 10, print = FALSE)
convergence(data, diagnostic = "psrf")
thetas <- extract_thetas(data)
# one variable
chains <- thetas[[2]]
psrf_metric(chains)
psrf_chain(chains)
calculate_psrf(chains, type = "original")
calculate_psrf(chains, type = "bulk")
calculate_psrf(chains, type = "tail")
calculate_psrf(chains, type = "max")
# all variables
lapply(thetas, psrf_metric)
lapply(thetas, psrf_chain)
lapply(thetas, calculate_psrf)

# check if results match with rstan
calculate_psrf(chains, type = "max")
rstan::Rhat(chains)
# NOPE

# Rhat at it 1 == Rhat of chains as iterations
# see https://github.com/stan-dev/rstan/blob/e79bc1746f61ebe0f40500b3f1540174d074540f/rstan/rstan/R/monitor.R#L184
a <- chains[1,]
rstan::Rhat(a)
dim(a) <- c(length(a), 1)
rstan::Rhat(a)

# rstan functions
bulk_rhat <- rstan:::rhat_rfun(rstan:::z_scale(rstan:::split_chains(chains)))
sims_folded <- abs(chains - median(chains))
tail_rhat <- rstan:::rhat_rfun(rstan:::z_scale(rstan:::split_chains(sims_folded)))
max(bulk_rhat, tail_rhat)
rstan:::rhat_rfun(chains)
# higher Rhat value on 'plain' data

# try to match with my functions
bulk_rhat2 <- z_scale(split_chains(chains)) |> calculate_psrf()
tail_rhat2 <- z_scale(split_chains(abs(chains - median(chains)))) |> calculate_psrf()
max(bulk_rhat2[length(bulk_rhat2)], tail_rhat2[length(tail_rhat2)])

# # compute autocorrelation
# # correlation between the t-th and (t-1)-th iteration in the MICE algorithm per variable
# # using base R functions only
#
# cor(thetas[["bmi"]][1:(tau-1), 1], thetas[["bmi"]][2:tau, 1], use = "pairwise.complete.obs")
# chains <- thetas[[2]]

# vrbs <- names(data$data)
# p <- length(vrbs)
# m <- as.integer(data$m)
# tau <- as.integer(data$iteration)
# out <- expand.grid(.it = seq_len(tau), vrb = vrbs)
