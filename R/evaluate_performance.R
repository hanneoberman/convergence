# function(s) to evaluate the estimates
evaluate_est <- function(results) {
  performance <- purrr::map_dfr(results, ~{
    dplyr::mutate(.x,
      bias = truth - estimate,
      cov = conf.low <= truth & conf.high >= truth,
      ciw = conf.high - conf.low,
      .keep = "unused"
        )
  })
  return(performance)
}


summarize_res <- function(performance) {
  performance <- mutate(performance,
                        mech = factor(mech, levels = c("MCAR", "MAR", "MNAR"), ordered = TRUE),
                        prop = factor(prop, ordered = TRUE),
                        term = factor(term, levels = c("Y", "X1", "X2", "X3"), ordered = TRUE))
  results <- performance |> 
    group_by(method, mech, prop, .it, term) |> 
    summarise(across(c(bias, cov, ciw, ac_mean, psrf_mean, ac_sd, psrf_sd), \(x) mean(x, na.rm = TRUE)))
  results_ll <- performance |> 
    group_by(method, mech, prop, .it, term) |> 
    summarise(across(c(bias, cov, ciw, ac_mean, psrf_mean, ac_sd, psrf_sd), \(x) quantile(x, probs = c(0.025), na.rm = TRUE))) 
  results <- left_join(results, results_ll, by = c("method", "mech", "prop", ".it", "term"), suffix = c("", "_ll"))
  results_ul <- performance |> 
    group_by(method, mech, prop, .it, term) |> 
    summarise(across(c(bias, cov, ciw, ac_mean, psrf_mean, ac_sd, psrf_sd), \(x) quantile(x, probs = c(0.975), na.rm = TRUE))) 
  results <- left_join(results, results_ul, by = c("method", "mech", "prop", ".it", "term"), suffix = c("", "_ul"))
  cov_SE <- sqrt((results$cov * (1 - results$cov)) / max(performance$.sim))
  results$cov_ll <- results$cov - qnorm(0.975) * cov_SE
  results$cov_ul <- results$cov + qnorm(0.975) * cov_SE
  return(results)
}
# performance[performance$term == "b0", "term"] <- "Y"
