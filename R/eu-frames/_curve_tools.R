##############################################################################
# Title:        _curve_tools.R – data and fits shared by the curve stages
# Purpose:      One definition of what a specification is and of how it is
#               fitted, for the stages that build the curve of the claim, its
#               fork decomposition, the comparison with the options that are
#               not on the curve, and the displays. A specification is one
#               value on each of the thirteen axis codes that the exercise
#               helpers use (outcome, predictor, copredictor, estimator,
#               sample, weights, construction, noop, item_set, cell_weight,
#               measure, pred_form, family). Its cells are built the way the
#               helpers build them: the outcome from the measurement panel
#               under the chosen scoring rule, item set, no-opinion rule and
#               cell weighting; the predictor from the chosen published
#               series, which shortens the panel when the series is short; the
#               adjustment from the other economic measure in its World Bank
#               series.
#               Every linear fit reports the descriptive partial correlation
#               of the conventional fit, r = t / sqrt(t^2 + df) on the
#               conventional t and residual df, and three tests of the same
#               coefficient: conventional; clustered by country with the
#               small-sample adjustment of fixest (CR1), t on G - 1 df; and CR2
#               errors clustered by country with Satterthwaite df, the rule
#               that classes the curve.
#               The datawizard verbs evaluate their expressions outside the
#               calling function, so they cannot see its arguments (data_filter()
#               returns a broken frame and data_modify() an error when an
#               expression names row$...); the functions below that filter or
#               build columns from their arguments therefore use dplyr.
# Author:       Chris Moreh
# Last updated: 2026-10-26
# Sourced by:   R/eu-frames/09_spec_curve.R, R/eu-frames/10_curve_forks.R,
#               R/eu-frames/11_menu_deviations.R, R/eu-frames/12_displays.R,
#               R/eu-frames/13_reference_marks.R
##############################################################################

source("R/eu-frames/_paths.R")

axis_codes <- c("outcome", "predictor", "copredictor", "estimator", "sample", "weights",
                "construction", "noop", "item_set", "cell_weight", "measure", "pred_form", "family")
positive_scales <- c("mcosmo", "mutil", "mpos")
negative_scales <- c("mcomm", "mlib", "mneg")
unemp_series    <- c("wb_nat", "es_1574_act", "es_2064_pop", "es_2564_lfs")

# items in each scale, by item set: the rate is the count over these
k_items <- list(all = c(mcosmo = 5, mutil = 2, mcomm = 4, mlib = 2, mpos = 7, mneg = 6),
                un  = c(mcosmo = 5, mutil = 2, mcomm = 3, mlib = 2, mpos = 7, mneg = 5))

# the curve: one outcome set crossed with the options that the audit keeps
curve_menu <- function(outcomes) {
  dplyr::bind_rows(
    tidyr::expand_grid(predictor = "unemp", measure = unemp_series, pred_form = c("raw", "log")),
    tibble::tibble(predictor = "growth", measure = "wb_growth", pred_form = "raw")) |>
    tidyr::expand_grid(outcome = outcomes, copredictor = c("none", "other"),
                       estimator = c("fe_twoway", "fe_country"),
                       noop = c("zero", "drop"), item_set = c("all", "un"),
                       cell_weight = c("unweighted", "design")) |>
    datawizard::data_modify(sample = "all", weights = "none", construction = "share",
                            family = "gaussian") |>
    datawizard::data_select(select = axis_codes)
}

# claim alignment: negative scales are read reversed, and the claim expects
# the opposite sign for growth, so that r_ca < 0 is the direction of the claim
align_factor <- function(outcome, predictor) {
  dplyr::if_else(outcome %in% negative_scales, -1, 1) * dplyr::if_else(predictor == "growth", -1, 1)
}

curve_inputs <- function() {
  panel <- datawizard::data_read("cases/eu-frames/data/EUframes_cy.csv", verbose = FALSE)
  wts   <- datawizard::data_read(out_path("teney_panel_weights.csv"), verbose = FALSE)
  list(
    # the countries that the panel flags as recipients of financial assistance
    # in any year
    bailout = datawizard::data_filter(panel, bailout == 1)$cntry |> unique() |> sort(),
    meas = datawizard::data_read(out_path("teney_panel_measurement.csv"), verbose = FALSE) |>
      tidyr::pivot_wider(names_from = outcome, values_from = value),
    context = panel |>
      datawizard::data_select(select = c("cntry", "year", "bailout", "unemp", "growth")) |>
      datawizard::data_rename(select = c(wb_unemp = "unemp", wb_growth = "growth")) |>
      dplyr::left_join(datawizard::data_select(wts, select = c("cntry", "year", "pop_15plus")),
                       by = c("cntry", "year")),
    preds = datawizard::data_read(out_path("macro", "predictor_measures.csv"), verbose = FALSE)
  )
}

prep_sample <- function(d, s, bailout) {
  switch(s,
    all        = d,
    y2004_2008 = dplyr::filter(d, year <= 2008),
    y2009_2013 = dplyr::filter(d, year >= 2009),
    # !! because the cells carry a bailout column of their own, which would
    # otherwise mask the argument and let every country through
    no_bailout = dplyr::filter(d, !cntry %in% !!bailout),
    no_GR_ES   = dplyr::filter(d, !cntry %in% c("GR", "ES")))
}

# the cells of one specification, with y, x, copred and the regression weight w
spec_data <- function(row, inputs) {
  cons <- if (row$construction == "rate") "count" else row$construction
  k <- if (row$construction == "rate") k_items[[row$item_set]][[row$outcome]] else 1
  inputs$meas |>
    dplyr::filter(construction == cons, item_set == row$item_set, noop == row$noop,
                  cell_weight == row$cell_weight) |>
    dplyr::inner_join(inputs$context, by = c("cntry", "year")) |>
    dplyr::inner_join(inputs$preds |>
                        dplyr::filter(measure == row$measure) |>
                        dplyr::select(cntry, year, pred = value),
                      by = c("cntry", "year")) |>
    prep_sample(row$sample, inputs$bailout) |>
    dplyr::mutate(y = .data[[row$outcome]] / k,
                  x = if (row$pred_form == "log") log(pred) else pred,
                  copred = if (row$predictor == "unemp") wb_growth else wb_unemp,
                  w = switch(row$weights, none = 1, n_cell = n_cell, n_eff = n_eff_cell,
                             pop = as.numeric(pop_15plus))) |>
    # r-style: keep – dplyr::arrange() sorts in the C locale on every machine
    dplyr::arrange(cntry, year)
}

rhs_terms <- function(row) c("x", if (row$copredictor == "other") "copred")

# A linear fit (fixed effects or pooled) under the three kinds of inference.
# The conventional and CR1 tests come from fixest, whose clustered p-values use
# t on G - 1 df (its default t.df = "min"). CR2 is the bias-reduced
# linearisation of Bell and McCaffrey (2002) with the Satterthwaite df of
# Pustejovsky and Tipton (2018, Journal of Business & Economic Statistics),
# computed by clubSandwich on the same model fitted by lm() with the fixed
# effects as dummies; parameters::model_parameters(vcov = "CR2") gives the same
# standard error but tests it on the residual df, so clubSandwich is called
# directly. r is the conversion rule of Multi100 (standard_correlation_coefficient.R
# in marton-balazs-kovacs/multi100 on GitHub), r = t / sqrt(t^2 + df), which
# effectsize::t_to_r() computes identically.
fit_linear <- function(row, inputs, d = spec_data(row, inputs)) {
  rhs <- rhs_terms(row)
  fe  <- switch(row$estimator, fe_twoway = " | cntry + year", fe_country = " | cntry",
                pooled_cl = "")
  wv  <- if (row$weights == "none") NULL else d$w
  m   <- fixest::feols(stats::as.formula(paste0("y ~ ", paste(rhs, collapse = " + "), fe)),
                       data = d, weights = wv, vcov = "iid")
  conv <- fixest::coeftable(m)["x", ]
  cr1  <- fixest::coeftable(m, vcov = ~cntry)["x", ]
  df   <- fixest::degrees_freedom(m, type = "resid")
  dummies <- switch(row$estimator, fe_twoway = " + factor(cntry) + factor(year)",
                    fe_country = " + factor(cntry)", pooled_cl = "")
  m_lm <- stats::lm(stats::as.formula(paste("y ~", paste(rhs, collapse = " + "), dummies)),
                    data = d, weights = w)
  cr2 <- clubSandwich::coef_test(m_lm, vcov = "CR2", cluster = d$cntry,
                                 test = "Satterthwaite", coefs = "x")
  tibble::tibble(b = conv[["Estimate"]], se = conv[["Std. Error"]], t = conv[["t value"]], df = df,
                 r = effectsize::t_to_r(conv[["t value"]], df)$r, p_conv = conv[["Pr(>|t|)"]],
                 t_cr1 = cr1[["t value"]], p_cr1 = cr1[["Pr(>|t|)"]],
                 t_cr2 = cr2$tstat, df_cr2 = cr2$df_Satt, p_cr2 = cr2$p_Satt,
                 n_cells = nrow(d), n_countries = dplyr::n_distinct(d$cntry))
}
