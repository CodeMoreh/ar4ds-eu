################################################################################
# Title:        03_spec_curve.R
# Purpose:      The curve of the claim: every specification of the menu fitted
#               once, classed by errors clustered by country, with the errors
#               clustered by country-round stored beside them as a check.
# Reads:        cases/trust/data/trust_ess.csv
# Writes:       cases/trust/data/spec_curve.csv
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
source("R/trust/_tools.R")

ess <- read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)

grid <- expand_grid(!!!SPEC_AXES)

fit_both <- function(outcome, controls, weights, items, fe) {
  by_country <- fit_spec(ess, outcome, controls, weights, items, fe)
  by_round <- fit_spec(ess, outcome, controls, weights, items, fe, cluster = "country_round")
  by_country |>
    select(-sample, -concern, -extra, -cluster) |>
    mutate(se_cr = by_round$se, t_cr = by_round$stat, df_cr = by_round$df, p_cr = by_round$p)
}

curve <- grid |>
  pmap(fit_both) |>
  bind_rows() |>
  mutate(spec_id = row_number(), .before = 1) |>
  mutate(
    supports = b > 0,
    sig = p < 0.05,
    is_reference = outcome == REFERENCE$outcome & controls == REFERENCE$controls &
      weights == REFERENCE$weights & items == REFERENCE$items & fe == REFERENCE$fe
  )

stopifnot(nrow(curve) == 192, sum(curve$is_reference) == 1)

write_csv(curve, "cases/trust/data/spec_curve.csv")
