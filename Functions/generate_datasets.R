# functions to create incomplete datasets

# generate complete data
generate_complete <- function(
    n_obs = 200,
    corr = 0.3,
    betas = c(-0.5,-0.1, 0.1, 0.5)
) {
  # create variance-covariance matrix with moderate correlations
  vcov <- matrix(0.3, nrow = 4, ncol = 4)
  diag(vcov) <- 1
  # create predictor space data
  X <- mvtnorm::rmvnorm(n = n_obs, sigma = vcov)
  # multiply each predictor observation by the corresponding beta
  Y <- X %*% betas
  # generate residual error for each observation
  e <- rnorm(n_obs)
  # combine predictors and outcome plus residual
  dat <- data.frame(Y = Y + e,
                    X)
  # output
  return(dat)
}

# generate multivariate missingness patterns patterns
create_patterns <- function() {
  mis_pat <-
    expand.grid(c(0, 1), c(0, 1), c(0, 1), c(0, 1), c(0, 1)) %>%
    setNames(., c("Y", "X1", "X2", "X3", "X4")) %>%
    .[rowSums(.) > 1 & rowSums(.) < 5, ]
  return(mis_pat)
}

# ampute the complete data
induce_missingness <- function(
    dat,
    mis_pat = NULL, 
    mis_mech = c("MCAR", "MAR"),
    mis_prop = c(0.1, 0.25, 0.5)
) {
  # create a list of amputed data objects 
  # for each of the mechanisms and proportions
  amps <- purrr::map(mis_mech, function(mm) {
    purrr::map(mis_prop, function(mp) {
      # ampute the data
      mice::ampute(dat, patterns = mis_pat, mech = mm, prop = mp) 
    }) 
  }) %>% 
    # unlist just make sure the proportions 
    # are not nested within the meachnisms
    unlist(recursive = FALSE)
  # output
  return(amps)
}

# combine into one function
create_data <- function(
    sample_size = 200, 
    correlations = 0.3,
    effects = c(-0.5,-0.1, 0.1, 0.5),
    patterns = NULL, 
    mechanisms = c("MCAR", "MAR"),
    proportions = c(0.1, 0.25, 0.5)
) {
  # create a single complete dataset
  dat <- generate_complete(n_obs = sample_size, corr = correlations, betas = effects)
  # ampute the data with different missingness
  amps <- induce_missingness(dat, mis_pat, mis_mech = mechanisms, mis_prop = proportions)
  # output
  return(amps)
}
