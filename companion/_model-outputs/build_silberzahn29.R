################################################################################
# Title:        Silberzahn et al. (2018) – the 29 teams' effect estimates
# Purpose:      Turn the published crowdsourcing results into the committed
#               copy of the workshop, from which the slides rebuild Figure 3 of
#               the paper from numbers rather than reproduce the image.
# Reads:        osf.io/gvm2z, component "Results: Data", file
#               "Crowdsourcing Effects in OR with Subgroups.csv" (public;
#               fetched over the network, nothing licensed is involved)
# Writes:       data/silberzahn29.csv
# Author:       Chris Moreh
# Last updated: 2026-07-31
################################################################################


src <- "https://osf.io/download/yprcx/"

# The OSF copy is CR-terminated, which read_csv reads as a single line.
raw <- src |>
  url() |>
  readLines(warn = FALSE) |>
  paste(collapse = "\n") |>
  stringr::str_replace_all("\r", "\n") |>
  I() |>
  readr::read_csv(show_col_types = FALSE)

# Two published forks: the response distribution assumed by the team, and its
# treatment of the non-independence of repeated players and referees. "misc"
# holds the two teams whose approach (Dirichlet-process clustering, Tobit)
# sits outside the three families.
sz <- raw |>
  dplyr::transmute(
    team          = Team,
    approach      = stringr::str_squish(Analytic.Approach),
    distribution  = dplyr::recode(Distribution, misc = "Other"),
    clustering    = Non_independence,
    or            = OR,
    or_lo         = OR_lo,
    or_hi         = OR_hi,
    significant   = or_lo > 1 | or_hi < 1
  ) |>
  datawizard::data_arrange(c("distribution", "or"))

# The summary printed in the paper, as a guard: a failure means that the
# source file has changed.
stopifnot(
  nrow(sz) == 29,
  round(median(sz$or), 3) == 1.310,
  round(min(sz$or), 3) == 0.888,
  round(max(sz$or), 3) == 2.931,
  sum(sz$significant) == 20
)

readr::write_csv(sz, "data/silberzahn29.csv")
