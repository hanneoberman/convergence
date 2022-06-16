########################
### SETUP SIMULATION ###
########################

# packages
library(dplyr)
library(mvtnorm)
library(mice)
library(miceadds)

# functions
miceadds::source.all("./functions")

# randomness
set.seed(11)

# parameters
n_sim <- 10
n_obs <- 200
corr <- 0.5
betas <- c(1, 1, -1, -1)
mis_pat <- create_patterns()
mis_mech = c("MCAR", "MAR")
mis_prop = c(0.25, 0.5, 0.75)
n_it <- 10

# #################################
# ### TEST LOWER LEVEL FUCTIONS ###
# #################################
# 
# # generate data
# dat <- generate_complete(n_obs, corr, betas)
# 
# # ampute data
# amps <- induce_missingness(dat, mis_pat, mis_mech = "MAR", mis_prop = 0.5)
# 
# ##################################
# ### TEST HIGHER LEVEL FUCTIONS ###
# ##################################
# 
# amps <- create_data()
# ests <- apply_methods(amps, betas, n_it)

################################
### COMBINE INTO ONE FUCTION ###
################################

simulate_once <- function(n_obs, betas, mis_pat, mis_mech, mis_prop) {
  # generate incomplete data
  amps <- create_data(
    sample_size = n_obs,
    effects = betas,
    patterns = mis_pat,
    mechanisms = mis_mech,
    proportions = mis_prop
  )
  # estimate regression coefficients
  ests <- apply_methods(amps, betas, n_it)
  # output
  return(ests)
}

# ################################
# ### TEST SIMULATION FUNCTION ###
# ################################
# 
# ests <- simulate_once(n_obs, betas, mis_mech, mis_prop)

######################
### RUN SIMULATION ###
######################

# repeat the simulation function n_sim times
results_raw <- replicate(
  n_sim, 
  simulate_once(n_obs, betas, mis_pat, mis_mech, mis_prop),
  simplify = FALSE
  )
# # save raw results
# saveRDS(results_raw, "./Results/raw.RDS")

########################
### EVALUATE RESULTS ###
########################

# calculate bias, coverage rate and CI width
performance <- evaluate_est(results_raw)
# saveRDS(performance, "./Results/performance.RDS")

# simulation results across all conditions
performance %>% 
  group_by(method, .it) %>% 
  summarise(across(c(bias, cov, ciw, ac, psrf), mean, na.rm = TRUE))

# simulation results split by condition
performance %>% 
  group_by(method, mech, prop, .it) %>% 
  summarise(across(c(bias, cov, ciw, ac, psrf), mean, na.rm = TRUE))

# simulation results split by condition and regression coefficient
performance %>% 
  group_by(method, mech, prop, .it, term) %>% 
  summarise(across(c(bias, cov, ciw, ac, psrf), mean, na.rm = TRUE))


