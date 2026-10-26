################################################################################
# Title:        check_parity.R
# Purpose:      Checks the estimator of R/trust/_tools.R, which fits with fixest
#               and reads the fits with parameters, against a route that shares
#               none of its code: estimatr (Blair, Cooper, Coppock, Humphreys and
#               Sonnet) for the coefficient, the conventional errors and the
#               errors clustered by country on every specification of the curve
#               and on the off-curve fits; clubSandwich (Pustejovsky and Tipton
#               2018) against estimatr for the CR2 errors and Satterthwaite df of
#               the inference display; and the committed curve against a fresh fit.
# Reads:        cases/trust/data/trust_ess.csv, cases/trust/data/spec_curve.csv
# Writes:       nothing; stops on any disagreement
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/trust/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" rather than NA
ess <- readr::read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)
curve <- readr::read_csv("cases/trust/data/spec_curve.csv", show_col_types = FALSE,
                         col_types = readr::cols(stat_type = readr::col_character()))

# The independent route for one specification: estimatr with a dummy for
# every group that the estimator absorbs, and the round dummies as covariates.
# Its CR1 ("stata") counts every intercept in the small-sample factor
# (n - 1) / (n - K), where the default of fixest, which the curve uses, leaves
# out those nested in the clusters; so the clustered errors are compared under
# the convention of estimatr, which fixest takes as K.fixef = "full"
# (?fixest::ssc).
reference_fit <- function(outcome, controls, weights, items, fe, sample = "paper", concern = "index",
                          extra = "none") {
  frame <- spec_frame(ess, outcome, controls, weights, items, sample, concern, extra)
  rhs <- c("x", CONTROL_TERMS[[controls]], if (extra != "none") extra,
           if (fe == "country_and_round") "factor(essround)")
  dummies <- switch(fe, country_round = "factor(cr)", country_and_round = "factor(cntry)", pooled = NULL)
  form <- stats::reformulate(c(rhs, dummies), response = "y")
  conventional <- estimatr::lm_robust(form, data = frame, weights = w, se_type = "classical")
  clustered <- estimatr::lm_robust(form, data = frame, weights = w, clusters = cntry, se_type = "stata")
  # the fit of the estimator under the convention of estimatr
  full_k <- fixest::feols(stats::as.formula(paste("y ~", paste(rhs, collapse = " + "),
                                                  switch(fe, country_round = "| cr",
                                                         country_and_round = "| cntry", pooled = ""))),
                          data = frame, weights = ~w, vcov = "iid") |>
    summary(cluster = ~cntry, ssc = fixest::ssc(K.fixef = "full")) |>
    parameters::model_parameters(keep = "^x$")
  tibble::tibble(b_ref = conventional$coefficients[["x"]], se_conv_ref = conventional$std.error[["x"]],
                 se_cl_ref = clustered$std.error[["x"]], df_cl_ref = clustered$df[["x"]],
                 se_full_k = full_k$SE)
}

off_curve <- tibble::tribble(
  ~outcome,     ~controls, ~weights,          ~items,     ~fe,             ~sample,           ~concern, ~extra,
  "parliament", "paper",   "none",            "complete", "pooled",        "paper",           "index",  "none",
  "parliament", "paper",   "post_population", "complete", "country_round", "paper",           "index",  "none",
  "parliament", "paper",   "none",            "complete", "country_round", "all_respondents", "index",  "none",
  "parliament", "paper",   "none",            "complete", "country_round", "paper",           "place",  "gov_dissat"
)
fits <- curve |>
  datawizard::data_select(c("outcome", "controls", "weights", "items", "fe")) |>
  datawizard::data_modify(sample = "paper", concern = "index", extra = "none") |>
  dplyr::bind_rows(off_curve)
check <- fits |>
  dplyr::mutate(ours = purrr::pmap(fits, \(...) fit_spec(ess, ...)[, c("b", "se", "df", "se_conv")]),
                ref = purrr::pmap(fits, reference_fit)) |>
  tidyr::unnest(c(ours, ref))

# CR2 with Satterthwaite df, the few-cluster rule of the inference display:
# estimatr, which computes it for the display, against clubSandwich on one
# respondent in 20 of the reference specification, a sample small enough for
# the dense matrices of clubSandwich
cr2_frame <- spec_frame(ess) |>
  dplyr::slice(seq(1, dplyr::n(), by = 20))
cr2_estimatr <- estimatr::lm_robust(stats::reformulate(c("x", CONTROL_TERMS$paper), response = "y"),
                                    data = cr2_frame, fixed_effects = ~cr, clusters = cntry, se_type = "CR2")
cr2_club <- stats::lm(stats::reformulate(c("x", CONTROL_TERMS$paper, "factor(cr)"), response = "y"),
                      data = cr2_frame) |>
  clubSandwich::coef_test(vcov = "CR2", cluster = cr2_frame$cntry, test = "Satterthwaite", coefs = "x")

# The committed curve against a fresh fit of the estimator
check_fresh <- curve |>
  dplyr::mutate(fresh = purrr::pmap(list(outcome, controls, weights, items, fe),
                                    \(...) fit_spec(ess, ...)[, c("b", "stat", "r")])) |>
  tidyr::unnest(fresh, names_sep = "_")

gaps <- tibble::tibble(
  b_vs_estimatr = max(abs(check$b - check$b_ref)),
  se_conv_vs_estimatr = max(abs(check$se_conv / check$se_conv_ref - 1)),
  se_full_k_vs_estimatr = max(abs(check$se_full_k / check$se_cl_ref - 1)),
  df_vs_estimatr = max(abs(check$df - check$df_cl_ref)),
  cr2_se_vs_clubsandwich = abs(cr2_estimatr$std.error[["x"]] / cr2_club$SE - 1),
  cr2_df_vs_clubsandwich = abs(cr2_estimatr$df[["x"]] / cr2_club$df_Satt - 1),
  b_vs_fresh = max(abs(check_fresh$b - check_fresh$fresh_b)),
  stat_vs_fresh = max(abs(check_fresh$stat - check_fresh$fresh_stat))
)
print(as.data.frame(gaps))  # r-style: keep – a checker, whose output is its report

stopifnot(gaps$b_vs_estimatr < 1e-9, gaps$se_conv_vs_estimatr < 1e-8, gaps$se_full_k_vs_estimatr < 1e-8,
          gaps$df_vs_estimatr == 0, gaps$cr2_se_vs_clubsandwich < 1e-8, gaps$cr2_df_vs_clubsandwich < 1e-8,
          gaps$b_vs_fresh < 1e-9, gaps$stat_vs_fresh < 1e-9)
cat("Parity holds on", nrow(check), "fits against estimatr, and estimatr agrees with clubSandwich",  # r-style: keep – a checker, whose output is its report
    "on the CR2 errors and Satterthwaite df.\n")
