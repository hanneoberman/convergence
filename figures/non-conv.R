# reproduce example of pathological non-convergence traceplots
# by van Buuren (2018) and apply non-convergence diagnostics
# requires the packages 'mice', 'purrr', and 'dplyr'/'magrittr'
# and the non-convergence diagnostics function

library(mice, warn.conflicts = FALSE)

# non-conv
# impute missingness with mis-specified imputation model
meth <- make.method(boys) #define imputation model
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)" #mis-specify model
imp_nonconv <- mice( #impute missingness
  boys,
  meth = meth,
  maxit = 10,
  print = FALSE,
  seed = 60109
)


# impute missingness with correctly specified imputation model
pred <- make.predictorMatrix(boys) #mend imputation method
pred[c("hgt", "wgt"), "bmi"] <- 0 #remove dependency
imp_conv <- conv <- mice( #impute missingness
  boys,
  meth = meth,
  pred = pred,
  maxit = 10,
  print = FALSE,
  seed = 60109
)

plot_wgt <- function(imp) {
# extract parameters for plotting
mn <- imp$chainMean
sm <- sqrt(imp$chainVar)
vrb <-  "wgt" #names(imp$data)
p <- length(vrb)
m <- imp$m
it <- imp$iteration
long <- cbind(expand.grid(.it = seq_len(it), .m = seq_len(m)),
              data.frame(
                .ms = rep(c("mean", "sd"), each = m * it *
                            p),
                vrb = rep(vrb, each = m * it, times = 2),
                val = c(matrix(aperm(mn[vrb, , , drop = FALSE], c(
                  2, 3, 1
                )), nrow = m * it * p), matrix(aperm(sm[vrb, , , drop = FALSE], c(
                  2, 3, 1
                )), nrow = m * it * p))
              ))
ggplot2::ggplot(long[long$.ms == "mean", ],
                ggplot2::aes(
                  x = .data$.it,
                  y = .data$val,
                  color = as.factor(.data$.m)
                )) + ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::geom_hline(yintercept = -Inf) + 
  ggplot2::labs(x = "Iteration number", y = "Average imputed value", color = "") + 
  ggplot2::theme_classic() + 
  ggplot2::theme(legend.position = "none")
}

plot_wgt(imp_nonconv) + ggplot2::ggtitle("Non-convergence")
plot_wgt(imp_conv) + ggplot2::ggtitle("Typical convergence")

# # plot non-conv
# n_obs <- 50
# dat <- data.frame(X = sample(c(1:7), n_obs, replace = TRUE), Y = rnorm(n_obs))
# dat$Z <- dat$X * dat$Y^2
# pat <- expand.grid(c(0,1), c(0,1), c(0,1)) |> setNames(c("X", "Y", "Z"))
# amp <- mice::ampute(dat, prop = .9, patterns = pat, mech = "MAR")$amp
# ggmice::plot_pattern(amp)
# meth <- mice::make.method(amp)
# meth["Z"] <- "~I(X * Y^2)"
# pred <- mice::make.predictorMatrix(amp)
# # non-conv
# imp <- mice::mice(amp, m = 5, meth = meth, pred = pred, maxit = 20, print = FALSE) #, visitSequence = c("Z", "X", "Y")
# ggmice::plot_trace(imp)
# # conv
# pred[, "Z"] <- 0
# imp <- mice::mice(amp, m = 5, meth = meth, pred = pred, maxit = 20, print = FALSE) #, visitSequence = c("Z", "X", "Y")
# ggmice::plot_trace(imp)
