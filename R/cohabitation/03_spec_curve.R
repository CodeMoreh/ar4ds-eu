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

source("R/cohabitation/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" and
# the codes of the cases, strata and clusters as numbers
unions <- readr::read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                          col_types = readr::cols(caseid = readr::col_character(),
                                                  stratum = readr::col_character(),
                                                  psu = readr::col_character()))

grid <- tidyr::expand_grid(!!!SPEC_AXES)

# Both fits of one specification. dplyr rather than datawizard in the body,
# because datawizard evaluates its expressions in a fresh environment and so
# cannot see `by_woman`.
fit_both <- function(cohorts, horizon, adjust, weights, time, model) {
  by_design <- fit_spec(unions, cohorts, horizon, adjust, weights, time, model)
  by_woman <- fit_spec(unions, cohorts, horizon, adjust, weights, time, model, errors = "woman")
  by_design |>
    dplyr::select(-comparison, -education, -short, -errors) |>
    dplyr::mutate(se_woman = by_woman$se, t_woman = by_woman$stat, df_woman = by_woman$df, p_woman = by_woman$p)
}

curve <- grid |>
  purrr::pmap(fit_both) |>
  dplyr::bind_rows() |>
  datawizard::data_modify(
    spec_id = 1:n(),
    supports = b < 0,
    sig = p < 0.05,
    is_reference = cohorts == REFERENCE$cohorts & horizon == REFERENCE$horizon &
      adjust == REFERENCE$adjust & weights == REFERENCE$weights & time == REFERENCE$time &
      model == REFERENCE$model
  ) |>
  datawizard::data_relocate("spec_id")

stopifnot(nrow(curve) == 144, sum(curve$is_reference) == 1)

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(curve, "cases/cohabitation/data/spec_curve.csv")
