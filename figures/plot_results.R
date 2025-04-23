# plot results 
plot_results <- function(summarized_results, metric, hline, mis = "MAR") {
  # Check if the metric is valid
  if (!metric %in% c("bias", "cov", "ciw", "ac_mean", "ac_sd", "psrf_mean", "psrf_sd")) {
    stop("Invalid metric. Choose from 'bias', 'cov', 'ciw', 'ac_mean', 'ac_sd', 'psrf_mean', or 'psrf_sd'.")
  }
  metric_ll <- paste0(metric, "_ll")
  metric_ul <- paste0(metric, "_ul")
  summarized_results |>
    filter(method == "MICE") |> 
    filter(mis %in% mech) |>
    ggplot(aes(x = .it, y = .data[[metric]])) +
    geom_hline(yintercept = hline, alpha = 0.1) + 
    geom_line() +
    geom_line(aes(y = .data[[metric_ll]]), linetype = "dashed", alpha = 0.5) +
    geom_line(aes(y = .data[[metric_ul]]), linetype = "dashed", alpha = 0.5) +
    facet_grid(term ~ prop + mech) +
    theme_classic()
}
