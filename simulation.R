## simulation parameters ##

# setup
set.seed(123)
n_sim = 2

# data generation parameters
n_obs = 500 # c(50, 500, 5000) 
corr = 0.4 # c(0, 0.4, 0.8)
dat_par <- expand.grid(n_obs = n_obs, corr = corr)
# amputation parameters
mis_mech = c("MCAR", "MAR") # c("MCAR", "MAR", "MNAR")
mis_type = "RIGHT"  # c("LEFT", "RIGHT", "MID", "TAIL")
mis_prop = c(0.1, 0.25, 0.5)
amp_par <- expand.grid(dat_nr = 1:nrow(dat_par), mis_mech = mis_mech, mis_type = mis_type, mis_prop = mis_prop, stringsAsFactors = FALSE)
# imputation parameters
imp_meth = c("none", "norm") # c("none", "mean", "norm.predict", "norm") 
n_imp = 5 # c(1, 5, 10)
n_it = 7 # c(1, 5, 10)
imp_par <- expand.grid(amp_nr = 1:nrow(amp_par), imp_meth = imp_meth, n_imp = n_imp, n_it = n_it, stringsAsFactors = FALSE)

# simulation function
simulate_once <- function(...) {
  all_dat <- purrr::pmap(dat_par, ~{generation(n = ..1, r = ..2)})
  all_amp <- purrr::pmap(amp_par, ~{amputation(all_dat[[..1]], mech = ..2, type = ..3, prop = ..4)})
  all_imp <- purrr::pmap(imp_par, ~{imputation(all_amp[[..1]], meth = ..2, m = ..3, it = ..4)})
  out <- purrr::map_dfr(all_imp, ~evaluation(.x))
  return(out)
}

# test function
simulate_once()

# testrun simulation 
a <- replicate(n_sim, simulate_once(), simplify = FALSE)
