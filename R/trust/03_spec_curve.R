################################################################################
# Title:        03_spec_curve.R
# Purpose:      The curve of the claim: every specification of the menu fitted
#               once, classed by errors clustered by country, with the errors
#               clustered by country-round stored beside them as a check.
# Reads:        cases/trust/data/trust_ess.csv
# Writes:       cases/trust/data/spec_curve.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/trust/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" rather than NA
ess <- readr::read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)

grid <- tidyr::expand_grid(!!!SPEC_AXES)

# One specification under both clusterings, in the columns of the curve file;
# the interval of the conventional test is left out, as its t and df carry it
fit_both <- function(outcome, controls, weights, items, fe) {
  by_country <- fit_spec(ess, outcome, controls, weights, items, fe)
  by_round <- fit_spec(ess, outcome, controls, weights, items, fe, cluster = "country_round")
  by_country |>
    dplyr::select(-sample, -concern, -extra, -cluster, -ci_conv_low, -ci_conv_high) |>
    dplyr::mutate(se_cr = by_round$se, t_cr = by_round$stat, df_cr = by_round$df, p_cr = by_round$p)
}

curve <- grid |>
  purrr::pmap(fit_both) |>
  dplyr::bind_rows() |>
  dplyr::mutate(spec_id = dplyr::row_number(), .before = 1) |>
  datawizard::data_modify(
    supports = b > 0,
    sig = p < 0.05,
    is_reference = outcome == REFERENCE$outcome & controls == REFERENCE$controls &
      weights == REFERENCE$weights & items == REFERENCE$items & fe == REFERENCE$fe
  )

stopifnot(nrow(curve) == 192, sum(curve$is_reference) == 1)

# readr writes the bytes of the committed file; datawizard::data_write() does not
readr::write_csv(curve, "cases/trust/data/spec_curve.csv")
