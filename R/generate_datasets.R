# functions to create incomplete datasets

# generate complete data
generate_complete <- function(n_obs, n_col, corr, betas) {
  # create variance-covariance matrix with moderate correlations
  vcov <- matrix(corr, nrow = n_col, ncol = n_col)
  diag(vcov) <- 1
  # create predictor space data
  X <- mvtnorm::rmvnorm(n = n_obs, sigma = vcov)
  # multiply each predictor observation by the corresponding beta
  Y <- X %*% betas
  # generate residual error for each observation
  e <- rnorm(n_obs, mean = 0, sd = 1)
  # combine predictors and outcome plus residual
  dat <- data.frame(Y = Y + e,
                    X)
  # output
  return(dat)
}

# generate multivariate missingness patterns patterns
create_patterns <- function(n_col) {
  pat_list <- vector(mode = "list", length = n_col + 1)
  pat_list <- lapply(pat_list, \(x) {x <- c(0, 1)})
  mis_pat <- expand.grid(pat_list) 
  names(mis_pat) <- c("Y", paste0("X", 1:n_col))
  # omit patterns with all/none missing
  mis_pat <- mis_pat[rowSums(mis_pat) > 0 & rowSums(mis_pat) < n_col, ]
  return(mis_pat)
}

# ampute the complete data
induce_missingness <- function(
    dat, mis_pat, mis_mech, mis_prop) {
  # create a list of amputed data objects 
  # for each of the mechanisms and proportions
  amps <- purrr::map(mis_mech, function(mm) {
    purrr::map(mis_prop, function(mp) {
      # ampute the data
      mice::ampute(dat, patterns = mis_pat, mech = mm, prop = mp) 
    }) 
  }) |> 
    # unlist just make sure the proportions 
    # are not nested within the meachnisms
    unlist(recursive = FALSE)
  # output
  return(amps)
}

# combine into one function
create_data <- function(sample_size,
                        correlations,
                        effects,
                        patterns,
                        mechanisms,
                        proportions) {
  # create a single complete dataset
  dat <- generate_complete(
    n_obs = sample_size,
    n_col = length(effects),
    corr = correlations,
    betas = effects
  )
  # ampute the data with different missingness
  amps <- induce_missingness(dat,
                             mis_pat = patterns,
                             mis_mech = mechanisms,
                             mis_prop = proportions)
  # output
  return(amps)
}
