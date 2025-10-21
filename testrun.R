# setup via simulation.R file

# generate data
dat <- generate_complete(n_obs, n_col, corr, betas)
# dat$Z <- dat$X1 / (dat$X1 + dat$X2)
# dat$Z <- dat$X1 / dat$X2^2
dat$Z <- dat$X1 + dat$X2

# ampute data
mis_pat <- create_patterns(n_col + 1)
amps <- induce_missingness(dat, mis_pat = mis_pat, mis_mech = "MAR", mis_prop = 0.5)
ggmice::plot_pattern(amps[[1]]$amp)

# impute data with MICE correctly specified
meth <- make.method(amps[[1]]$amp[, 1:n_col])
meth[] <- "norm"
imp <- mice(amps[[1]]$amp[, 1:n_col], maxit = 10, meth = meth)
# plot(imp)
ggmice::plot_trace(imp)

# convergence
conv <- convergence(imp)
ggplot(conv, aes(x = .it, y = ac, color = vrb)) + geom_hline(yintercept = 0) + geom_line() + theme_classic()
ggplot(conv, aes(x = .it, y = psrf, color = vrb)) + geom_hline(yintercept = 1) + geom_line() + theme_classic()

# impute data with MICE incorrectly specified
meth <- make.method(amps[[1]]$amp)
# meth["Z"] <- "~I(X1 / (X1 + X2))"
# meth["Z"] <- "~I(X1 / X2^2)"
meth[] <- "norm"
meth["Z"] <- "~I(X1 + X2)"
imp2 <- mice(amps[[1]]$amp, maxit = 10, meth = meth)
# plot(imp2)
ggmice::plot_trace(imp2)
# non-convergence
conv2 <- convergence(imp2)
ggplot(conv2, aes(x = .it, y = ac, color = vrb)) + geom_hline(yintercept = 0) + geom_line() + theme_classic()
ggplot(conv2, aes(x = .it, y = psrf, color = vrb)) + geom_hline(yintercept = 1) + geom_line() + theme_classic()
