source("../R/generate_datasets.R")
# pat <- create_patterns(3)
# d <- dat[1:10,]
# is.na(d) <- pat == 0
# ggmice::plot_pattern(d)
plot_patterns <- function(n_col = 4) {
  pat <- create_patterns(n_col - 1)
  pat <- abs(pat - 1)
  pat <- rbind(pat, 0)
  pat[pat == 1] <- "missing"
  pat[pat == 0] <- "observed"
  rws <- nrow(pat)
  cls <- ncol(pat)
  vrb <- colnames(pat)
  # frq <- row.names(pat)[-rws]
  # na_row <- pat[-rws, cls]
  # na_col <- pat[rws, -cls]
  pat_clean <- cbind(.opacity = 1, pat)
  long <-
    as.data.frame(cbind(.y = 1:(rws), pat_clean)) %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(vrb),
      names_to = "x",
      values_to = ".where"
    ) %>%
    dplyr::mutate(
      .x = as.numeric(factor(
        .data$x, levels = vrb, ordered = TRUE
      )),
      .where = factor(
        .data$.where,
        levels = c("observed", "missing"),
        ordered = TRUE
      ),
      .opacity = as.numeric(.data$.opacity)
    )
  gg <-
    ggplot2::ggplot(
      long,
      ggplot2::aes(
        .data$.x,
        .data$.y,
        fill = .data$.where,
        alpha = 0.1 + .data$.opacity / 2
      )
    ) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c(
      "observed" = "#006CC2B3",
      "missing" = "#B61A51B3"
    )) +
    ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
    ggplot2::scale_x_continuous(breaks = 1:cls, labels = vrb, position = "top") +
    ggplot2::scale_y_continuous(breaks = NULL, labels = NULL) +
    ggmice:::theme_minimice() +
    labs(fill = "", x = "Variables", y = "Patterns")
  gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
  return(gg)
}
