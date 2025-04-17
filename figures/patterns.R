source("../R/generate_datasets.R")
pat <- create_patterns(3)
d <- dat[1:10,]
is.na(d) <- pat == 1
ggmice::plot_pattern(d)
