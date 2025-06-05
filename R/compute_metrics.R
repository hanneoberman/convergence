# functions to calculate PSRF and AC
vrbs <- names(data$data)
p <- length(vrbs)
m <- as.integer(data$m)
tau <- as.integer(data$iteration)
out <- expand.grid(.it = seq_len(tau), vrb = vrbs)

# # Setup
# vrbs <- names(data$data)
# t <- data$iteration
# m <- data$m
# out <- expand.grid(.it = seq_len(t), vrb = vrbs)
# 
# # Extract parameter array and align by variable name
# arr <- if (parameter == "mean") data$chainMean else sqrt(data$chainVar)
# arr_perm <- aperm(arr[vrbs, , , drop = FALSE], c(2L, 3L, 1L))  # [it, m, var]
# param <- lapply(vrbs, function(vrb) arr_perm[, , vrb])
# names(param) <- vrbs

## dev objects
# library(mice)
# data <- mice(nhanes)
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

# # compute autocorrelation
# # correlation between the t-th and (t-1)-th iteration in the MICE algorithm per variable
# # using base R functions only
# 
# cor(thetas[["bmi"]][1:(tau-1), 1], thetas[["bmi"]][2:tau, 1], use = "pairwise.complete.obs")
# chains <- thetas[[2]]

# compute PSRF, with Vehtari et al. (2019) modifications
psrf_max <- function(chains) {
  # original cf. Gelman and Rubin (1992) 
  rhat_original <- psrf_metric(chains)
  # adjusted cf. Vehtari et al. (2019) 
  rhat_bulk <- chains |> split_chains() |> z_scale() |> psrf_metric()
  rhat_tail <- chains |> fold_chains() |> split_chains() |> z_scale() |> psrf_metric()
  # return the maximum of the three
  rhat_max <- max(rhat_bulk, rhat_tail, rhat_original, na.rm = TRUE)
  return(rhat_max)
}
  
#   map_dfr(2:tau, function(it) {
#   # compute original r hat conform Gelman and Rubin (1992) 
#   rhat_original <- x[1:it,] %>% get.rhat()
#   # compute r hat in all ways described by Vehtari et al. (2019)
#   rhat_bulk <-
#     x[1:it,] %>% split_chains() %>% z_scale() %>% get.rhat()
#   rhat_tail <-
#     x[1:it,] %>% fold_sims() %>% split_chains() %>% z_scale() %>% get.rhat()
#   max(rhat_bulk, rhat_tail) %>% data.frame(r.hat.max = ., r.hat = rhat_original) #%>% set_names("r.hat")
# }) %>% rbind(NA, .) %>% cbind(iteration = 1:t, .)

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
    # split each chain to get 2m chains
    lower <- 1:floor(tau / 2)
    upper <- ceiling((tau / 2) + 1):t
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

# compute rhat
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

# psrf at last iteration to each variable
lapply(thetas, psrf_max)

# psrf at each iteration
calculate_psrf <- function(chains) {
  tau <- nrow(chains)
  psrf_it <- numeric(tau)
  for (it in seq_len(tau)) {
      psrf_it[it] <- psrf_max(chains[1:it, , drop = FALSE])
  }
  return(psrf_it)
}

# apply to all variables
lapply(thetas, calculate_psrf)

# compare with rstan version
convergence(imp)
