################################################################################
# Title:        check_parity.R
# Purpose:      Checks the estimator of R/housing/_tools.R against routes that
#               share none of its code: estimatr for the HC2 errors of the
#               linear model (Blair, Cooper, Coppock, Humphreys and Sonnet), and
#               margins for the average marginal effect of the logit and its
#               delta-method error (Leeper 2017), on every specification; and
#               the committed curve against a fresh fit.
# Reads:        cases/housing/data/housing_audit.csv, cases/housing/data/spec_curve.csv
# Writes:       nothing; stops on any disagreement
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/housing/_tools.R")

audit <- readr::read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)
curve <- readr::read_csv("cases/housing/data/spec_curve.csv", show_col_types = FALSE)

# The independent route for each specification. margins takes the discrete
# change of a factor, so the Hispanic name enters its logit as a factor.
reference_fit <- function(comparison, population, uncoded, model, adjust) {
  terms <- ADJUST_TERMS[[adjust]]
  frame <- spec_frame(audit, comparison, population, uncoded) |>
    dplyr::select("y", "hisp", dplyr::all_of(terms)) |>
    stats::na.omit()
  form <- stats::reformulate(c("hisp", terms), response = "y")
  if (model == "lpm") {
    fit <- estimatr::lm_robust(form, data = frame, se_type = "HC2")
    tibble::tibble(b_ref = 100 * stats::coef(fit)[["hisp"]], se_ref = 100 * fit$std.error[["hisp"]])
  } else {
    frame <- dplyr::mutate(frame, hisp = factor(hisp))
    fit <- stats::glm(form, family = stats::binomial(), data = frame)
    ame <- summary(margins::margins(fit, variables = "hisp", data = frame))  # r-style: keep – the independent route; marginaleffects is the estimator under test
    tibble::tibble(b_ref = 100 * ame$AME, se_ref = 100 * ame$SE)
  }
}

check <- curve |>
  dplyr::mutate(ref = purrr::pmap(list(comparison, population, uncoded, model, adjust), reference_fit)) |>
  tidyr::unnest(ref) |>
  dplyr::mutate(fresh = purrr::pmap(list(comparison, population, uncoded, model, adjust),
                                    \(...) fit_spec(audit, ...)[, c("b_pp", "stat")])) |>
  tidyr::unnest(fresh, names_sep = "_")

gaps <- check |>
  datawizard::data_summary(
    b_vs_reference = max(abs(b_pp - b_ref)),
    se_vs_reference = max(abs(se_pp - se_ref)),
    b_vs_fresh = max(abs(b_pp - fresh_b_pp)),
    stat_vs_fresh = max(abs(stat - fresh_stat))
  )
print(gaps)

stopifnot(gaps$b_vs_reference < 1e-6, gaps$se_vs_reference < 1e-3,
          gaps$b_vs_fresh < 1e-9, gaps$stat_vs_fresh < 1e-9)
cat("Parity holds on all", nrow(check), "specifications.\n")  # r-style: keep – a checker, whose output is its report
