################################################################################
# Title:        03_spec_curve.R
# Purpose:      The curve of the claim: every specification of the menu fitted
#               once, classed by design-based errors (strata and clusters of the
#               two surveys), with errors clustered by woman stored beside them
#               as a check.
# Reads:        cases/cohabitation/data/cohab_unions.csv
# Writes:       cases/cohabitation/data/spec_curve.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

library(tidyverse)
source("R/cohabitation/_tools.R")

unions <- read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                   col_types = cols(caseid = col_character(), stratum = col_character(), psu = col_character()))

grid <- expand_grid(!!!SPEC_AXES)

fit_both <- function(cohorts, horizon, adjust, weights, time, model) {
  by_design <- fit_spec(unions, cohorts, horizon, adjust, weights, time, model)
  by_woman <- fit_spec(unions, cohorts, horizon, adjust, weights, time, model, errors = "woman")
  by_design |>
    select(-comparison, -education, -short, -errors) |>
    mutate(se_woman = by_woman$se, t_woman = by_woman$stat, df_woman = by_woman$df, p_woman = by_woman$p)
}

curve <- grid |>
  pmap(fit_both) |>
  bind_rows() |>
  mutate(spec_id = row_number(), .before = 1) |>
  mutate(
    supports = b < 0,
    sig = p < 0.05,
    is_reference = cohorts == REFERENCE$cohorts & horizon == REFERENCE$horizon &
      adjust == REFERENCE$adjust & weights == REFERENCE$weights & time == REFERENCE$time &
      model == REFERENCE$model
  )

stopifnot(nrow(curve) == 144, sum(curve$is_reference) == 1)

write_csv(curve, "cases/cohabitation/data/spec_curve.csv")
