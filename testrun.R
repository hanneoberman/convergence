# setup via simulation.R file

# generate data
dat <- generate_complete(n_obs, n_col, corr, betas)

# ampute data
mis_pat <- create_patterns(n_col)
amps <- induce_missingness(dat, mis_pat = mis_pat, mis_mech = "MAR", mis_prop = 0.5)
ggmice::plot_pattern(amps[[1]]$amp)

# impute data with MICE correctly specified
meth <- make.method(amps[[1]]$amp)
meth[] <- "norm"
imp <- mice(amps[[1]]$amp, maxit = 10, meth = meth)
# plot(imp)
ggmice::plot_trace(imp)

# convergence
conv <- convergence(imp)
ggplot(conv, aes(x = .it, y = ac, color = vrb)) + geom_hline(yintercept = 0) + geom_line() + theme_classic()
ggplot(conv, aes(x = .it, y = psrf, color = vrb)) + geom_hline(yintercept = 1) + geom_line() + theme_classic()

# impute data with MICE incorrectly specified
dat <- generate_complete(n_obs, n_col, corr, betas, composite = TRUE)
# ampute data
mis_pat <- create_patterns(n_col)
amps <- induce_missingness(dat, mis_pat = mis_pat, mis_mech = "MAR", mis_prop = 0.5)
meth <- make.method(amps[[1]]$amp)
# meth["Z"] <- "~I(X1 / (X1 + X2))"
# meth["Z"] <- "~I(X1 / X2^2)"
meth[] <- "norm"
meth[n_col + 1] <- "~I(X1 + X2)"
imp <- mice(amps[[1]]$amp, maxit = 10, meth = meth)
# plot(imp2)
ggmice::plot_trace(imp)
# non-convergence
conv <- convergence(imp)
ggplot(conv, aes(x = .it, y = ac, color = vrb)) + geom_hline(yintercept = 0) + geom_line() + theme_classic()
ggplot(conv, aes(x = .it, y = psrf, color = vrb)) + geom_hline(yintercept = 1) + geom_line() + theme_classic()
