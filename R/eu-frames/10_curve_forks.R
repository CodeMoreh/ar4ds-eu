##############################################################################
# Title:        10_curve_forks.R
# Purpose:      How much each decision on the curve moves the result, on both
#               curves. Two measures per axis that varies.
#               The share of the variance of the claim-aligned partial
#               correlation that the main effect of the axis carries:
#               non-partial eta-squared from a sequential main-effects fit,
#               with the remainder reported as the interactions between axes.
#               The series and the form of the predictor exist only for
#               unemployment, so the predictor enters first and the series and
#               the form after it; every other axis is crossed with every other
#               in a balanced design, so their shares do not depend on the
#               order.
#               The crossing rate: among the pairs of specifications that
#               differ only on the axis, the share whose class differs, the
#               class being "below zero and significant at 5% under CR2 errors
#               clustered by country with Satterthwaite df" or not. Axes with
#               three or more options contribute every pair of their options.
#               The series and the form pair only within unemployment; the
#               predictor pairs unemployment in the raw World Bank series with
#               growth in the World Bank series, the two that differ in nothing
#               but the measure of the economy. The rate under conventional
#               errors is kept beside it.
# Reads:        cases/eu-frames/data/spec_curve.csv,
#               cases/eu-frames/data/spec_curve_negative.csv
# Writes:       cases/eu-frames/data/spec_curve_forks.csv
# Author:       Chris Moreh
# Last updated: 2026-10-26
##############################################################################

source("R/eu-frames/_curve_tools.R")

curves <- list(
  positive = datawizard::data_read(out_path("spec_curve.csv"), verbose = FALSE),
  negative = datawizard::data_read(out_path("spec_curve_negative.csv"), verbose = FALSE)
)

varying <- c("outcome", "predictor", "measure", "pred_form", "copredictor", "estimator",
             "noop", "item_set", "cell_weight")

eta_shares <- function(d) {
  fml <- stats::as.formula(paste("r_ca ~", paste(varying, collapse = " + ")))
  e <- effectsize::eta_squared(stats::aov(fml, data = d), partial = FALSE, verbose = FALSE)
  tibble::tibble(axis = c(e$Parameter, "interactions"), share = 100 * c(e$Eta2, 1 - sum(e$Eta2)))
}

# every pair of specifications that agree on all axes but `a`
pairs_on <- function(d, a) {
  base <- d |>
    datawizard::data_modify(class_cr2 = supports & p_cr2 < 0.05,
                            class_conv = supports & p_conv < 0.05)
  if (a == "predictor") {
    base <- base |>
      datawizard::data_filter((predictor == "unemp" & measure == "wb_nat" & pred_form == "raw") |
                                predictor == "growth") |>
      datawizard::data_modify(measure = "paired", pred_form = "paired")
  }
  if (a %in% c("measure", "pred_form")) base <- datawizard::data_filter(base, predictor == "unemp")
  keys <- setdiff(axis_codes, a)
  slim <- dplyr::select(base, dplyr::all_of(keys), lev = dplyr::all_of(a), r_ca,
                        dplyr::starts_with("class_"))
  dplyr::inner_join(slim, slim, by = keys, suffix = c("_1", "_2"), relationship = "many-to-many") |>
    datawizard::data_filter(lev_1 < lev_2)
}

crossing <- function(d) {
  purrr::map(varying, \(a) {
    p <- pairs_on(d, a)
    tibble::tibble(axis = a, n_options = dplyr::n_distinct(c(p$lev_1, p$lev_2)), n_pairs = nrow(p),
                   median_abs_shift = stats::median(abs(p$r_ca_2 - p$r_ca_1)),
                   sign_changes = 100 * mean((p$r_ca_1 < 0) != (p$r_ca_2 < 0)),
                   crossing_rate = 100 * mean(p$class_cr2_1 != p$class_cr2_2),
                   crossing_rate_conv = 100 * mean(p$class_conv_1 != p$class_conv_2))
  }) |>
    purrr::list_rbind()
}

forks <- purrr::imap(curves, \(d, nm) {
  dplyr::full_join(eta_shares(d), crossing(d), by = "axis") |>
    dplyr::mutate(curve = nm, .before = 1)
}) |>
  purrr::list_rbind()

datawizard::data_write(forks, out_path("spec_curve_forks.csv"), verbose = FALSE)

stopifnot("the shares do not sum to 100" =
            all(abs(dplyr::summarise(forks, s = sum(share), .by = curve)$s - 100) < 1e-9))
forks |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) round(x, 1))) |>
  as.data.frame() |>
  print(row.names = FALSE)
