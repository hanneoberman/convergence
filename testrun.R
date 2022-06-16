# nonconv
library(ggplot2)

# generate data
dat <- generate_complete(n_obs, corr, betas)
# dat$X5 <- (2 * dat$X3) + dat$X4

# ampute data
amps <- induce_missingness(dat, mis_pat = NULL, mis_mech = "MAR", mis_prop = 0.5)

# impute data with MICE
imp <- mice(amps[[1]]$amp, maxit = 10)
plot(imp)
ggmice::plot_trace(imp)

# convergence
conv <- convergence(imp)
ggplot(conv, aes(x = .it, y = ac, color = vrb)) + geom_hline(yintercept = 0) + geom_line() + theme_classic()
ggplot(conv, aes(x = .it, y = psrf, color = vrb)) + geom_hline(yintercept = 1) + geom_line() + theme_classic()
