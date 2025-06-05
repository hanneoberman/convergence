########################
### SETUP SIMULATION ###
########################

# packages
library(dplyr)
library(mvtnorm)
library(mice)
library(miceadds)

# functions
miceadds::source.all("./R")

# randomness
set.seed(11)

# parameters
n_sim <- 20
n_obs <- 500
n_col <- 3
corr <- 0.5
beta <- 1
betas <- rep(beta, n_col)
mis_pat <- create_patterns(n_col)
mis_mech = c("MCAR") #, "MAR", "MNAR")
mis_prop = c(0.05, 0.25, 0.5, 0.75, 0.95)
n_it <- 50

# #################################
# ### TEST LOWER LEVEL FUCTIONS ###
# #################################
# 
# # generate data
# dat <- generate_complete(n_obs, n_col, corr, betas)
# 
# # ampute data
# amps <- induce_missingness(dat, mis_pat, mis_mech = "MCAR", mis_prop = 0.5)
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

simulate_once <- function(n_obs, corr, betas, mis_pat, mis_mech, mis_prop, n_it) {
  # generate incomplete data
  amps <- create_data(
    sample_size = n_obs,
    correlations = corr,
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
# ests <- simulate_once(n_obs, corr, betas, mis_pat, mis_mech, mis_prop, n_it)

######################
### RUN SIMULATION ###
######################

# # repeat the simulation function n_sim times
# results_raw <- replicate(
#   n_sim, 
#   simulate_once(n_obs, betas, mis_pat, mis_mech, mis_prop),
#   simplify = FALSE
#   )
# # save raw results
# saveRDS(results_raw, "./Results/raw.RDS")

####################
### RUN PARALLEL ###
####################

library(pbapply)

cl <- parallel::makeCluster(4)
parallel::clusterExport(
  cl,
  c(
    "generate_complete",
    "induce_missingness",
    "create_data",
    "apply_methods",
    "apply_full",
    "apply_CCA",
    "apply_MICE",
    "evaluate_est",
    "estimate_param",
    "create_patterns",
    "n_obs",
    "betas",
    "mis_pat",
    "mis_mech",
    "mis_prop",
    "simulate_once",
    "add_iteration",
    "n_it",
    "n_sim",
    "n_col",
    "corr",
    "beta",
    "betas",
    "mis_pat",
    "mis_mech",
    "mis_prop"
  )
)

out <- pbreplicate(n_sim, 
                   simulate_once(n_obs, corr, betas, mis_pat, mis_mech, mis_prop, n_it),
                   cl = cl, 
                   simplify = FALSE)

parallel::stopCluster(cl)

out <- purrr::map(1:length(out), ~{cbind(.sim = .x, out[[.x]])}) 

save(out, file = paste0("results/out", length(out), "sim", max(out[[1]]$.it), "it", format(Sys.time(), "%e%b%Y"), ".RData"))
save(out, file = "results/out.RData")
