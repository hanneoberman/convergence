# plot results 
plot_results <- function(summarized_results, metric, hline, mis = "MAR") {
  # Check if the metric is valid
  if (!metric %in% c("bias", "cov", "ciw", "ac_mean", "ac_sd", "psrf_mean", "psrf_sd")) {
    stop("Invalid metric. Choose from 'bias', 'cov', 'ciw', 'ac_mean', 'ac_sd', 'psrf_mean', or 'psrf_sd'.")
  }
  metric_ll <- paste0(metric, "_ll")
  metric_ul <- paste0(metric, "_ul")
  y_lab <- switch(
    metric,
    bias = "Bias",
    cov = "Coverage rate",
    ciw = "CI width",
    ac_mean = "AC in chain means",
    ac_sd = "AC in chain SDs",
    psrf_mean = "PSRF in chain means",
    psrf_sd = "PSRF in chain SDs"
  )
  summarized_results |>
    filter(method == "MICE") |> 
    filter(mis %in% mech) |>
    ggplot(aes(x = .it, y = .data[[metric]])) +
    geom_hline(yintercept = hline, alpha = 0.1, linewidth = 1) + 
    geom_line(na.rm = TRUE) +
    geom_line(aes(y = .data[[metric_ll]]), alpha = 0.3, na.rm = TRUE) +
    geom_line(aes(y = .data[[metric_ul]]), alpha = 0.3, na.rm = TRUE) +
    facet_grid(term ~ prop + mech) +
    labs(x = "Iteration number", y = y_lab) +
    theme_classic()
}

plot_results2 <- function(summarized_results, metric, hline, mis = c("MCAR", "MAR", "MNAR")) {
  # Check if the metric is valid
  if (!metric %in% c("bias", "cov", "ciw", "ac_mean", "ac_sd", "psrf_mean", "psrf_sd")) {
    stop("Invalid metric. Choose from 'bias', 'cov', 'ciw', 'ac_mean', 'ac_sd', 'psrf_mean', or 'psrf_sd'.")
  }
  metric_ll <- paste0(metric, "_ll")
  metric_ul <- paste0(metric, "_ul")
  y_lab <- switch(
    metric,
    bias = "Bias",
    cov = "Coverage rate",
    ciw = "CI width",
    ac_mean = "AC in chain means",
    ac_sd = "AC in chain SDs",
    psrf_mean = "PSRF in chain means",
    psrf_sd = "PSRF in chain SDs"
  )
  summarized_results |>
    filter(method == "MICE", term == "X3", mech %in% mis) |> 
    ggplot(aes(x = .it, y = .data[[metric]])) +
    geom_hline(yintercept = hline, linewidth = 0.2, linetype = "dashed") + 
    geom_line(na.rm = TRUE) +
    geom_line(aes(y = .data[[metric_ll]]), alpha = 0.3, na.rm = TRUE) +
    geom_line(aes(y = .data[[metric_ul]]), alpha = 0.3, na.rm = TRUE) +
    facet_grid(. ~ prop + mech) +
    labs(x = "Iteration number", y = y_lab) +
    theme_classic()
}
