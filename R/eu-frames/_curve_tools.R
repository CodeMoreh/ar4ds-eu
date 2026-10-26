##############################################################################
# Title:        _curve_tools.R – data and fits shared by the curve stages
# Purpose:      One definition of what a specification is and of how it is
#               fitted, for the stages that build the curve of the claim, its
#               fork decomposition, the comparison with the options that left
#               the curve, and the displays. A specification is one value on
#               each of the thirteen axis codes that the exercise helpers use
#               (outcome, predictor, copredictor, estimator, sample, weights,
#               construction, noop, item_set, cell_weight, measure, pred_form,
#               family). Its cells are built the way the helpers build them:
#               the outcome from the measurement panel under the chosen
#               scoring rule, item set, no-opinion rule and cell weighting;
#               the predictor from the chosen published series, which
#               shortens the panel when the series is short; the adjustment
#               from the other economic measure in its World Bank series.
#               Every linear fit reports the descriptive partial correlation
#               of the conventional fit, r = t / sqrt(t^2 + df) on the
#               conventional t and residual df, and four tests of the same
#               coefficient: conventional; clustered by country with the
#               small-sample adjustment of fixest (CR1), t on G - 1 df; CR2
#               with Satterthwaite df (clubSandwich); and the restricted wild
#               cluster bootstrap by country of _wild_bootstrap.R, with
#               Rademacher and with Webb signs.
# Author:       Chris Moreh
# Last updated: 2026-10-25
# Sourced by:   R/eu-frames/09_spec_curve.R, R/eu-frames/10_curve_forks.R,
#               R/eu-frames/11_menu_deviations.R, R/eu-frames/12_displays.R,
#               R/eu-frames/13_reference_marks.R
##############################################################################

source("R/eu-frames/_paths.R")
source("R/eu-frames/_wild_bootstrap.R")

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
  bind_rows(
    expand_grid(predictor = "unemp", measure = unemp_series, pred_form = c("raw", "log")),
    tibble(predictor = "growth", measure = "wb_growth", pred_form = "raw")) |>
    expand_grid(outcome = outcomes, copredictor = c("none", "other"),
                estimator = c("fe_twoway", "fe_country"),
                noop = c("zero", "drop"), item_set = c("all", "un"),
                cell_weight = c("unweighted", "design")) |>
    mutate(sample = "all", weights = "none", construction = "share", family = "gaussian") |>
    select(all_of(axis_codes))
}

# claim alignment: negative scales are read reversed, and the claim expects
# the opposite sign for growth, so that r_ca < 0 is the direction of the claim
align_factor <- function(outcome, predictor) {
  if_else(outcome %in% negative_scales, -1, 1) * if_else(predictor == "growth", -1, 1)
}

curve_inputs <- function() {
  panel <- datawizard::data_read("cases/eu-frames/data/EUframes_cy.csv", verbose = FALSE)
  wts   <- datawizard::data_read(out_path("teney_panel_weights.csv"), verbose = FALSE)
  list(
    # the countries that the panel flags as recipients of financial assistance
    # in any year
    bailout = sort(unique(panel$cntry[panel$bailout == 1])),
    meas = datawizard::data_read(out_path("teney_panel_measurement.csv"), verbose = FALSE) |>
      pivot_wider(names_from = outcome, values_from = value),
    context = panel |>
      select(cntry, year, bailout, wb_unemp = unemp, wb_growth = growth) |>
      left_join(wts |> select(cntry, year, pop_15plus), by = c("cntry", "year")),
    preds = datawizard::data_read(out_path("macro", "predictor_measures.csv"), verbose = FALSE)
  )
}

prep_sample <- function(d, s, bailout) {
  switch(s,
    all        = d,
    y2004_2008 = filter(d, year <= 2008),
    y2009_2013 = filter(d, year >= 2009),
    # !! because the cells carry a bailout column of their own, which would
    # otherwise mask the argument and let every country through
    no_bailout = filter(d, !cntry %in% !!bailout),
    no_GR_ES   = filter(d, !cntry %in% c("GR", "ES")))
}

# the cells of one specification, with y, x, copred and the regression weight w
spec_data <- function(row, inputs) {
  cons <- if (row$construction == "rate") "count" else row$construction
  k <- if (row$construction == "rate") k_items[[row$item_set]][[row$outcome]] else 1
  inputs$meas |>
    filter(construction == cons, item_set == row$item_set, noop == row$noop,
           cell_weight == row$cell_weight) |>
    inner_join(inputs$context, by = c("cntry", "year")) |>
    inner_join(inputs$preds |> filter(measure == row$measure) |> select(cntry, year, pred = value),
               by = c("cntry", "year")) |>
    prep_sample(row$sample, inputs$bailout) |>
    mutate(y = .data[[row$outcome]] / k,
           x = if (row$pred_form == "log") log(pred) else pred,
           copred = if (row$predictor == "unemp") wb_growth else wb_unemp,
           w = switch(row$weights, none = 1, n_cell = n_cell, n_eff = n_eff_cell,
                      pop = as.numeric(pop_15plus))) |>
    arrange(cntry, year)
}

rhs_terms <- function(row) c("x", if (row$copredictor == "other") "copred")

# a linear fit (fixed effects or pooled) under the five kinds of inference;
# with checks = FALSE the CR2 and Webb columns are left empty, which is all
# that a comparison under the bootstrap of the curve needs
fit_linear <- function(row, inputs, signs, d = spec_data(row, inputs), checks = TRUE) {
  rhs <- rhs_terms(row)
  fe  <- switch(row$estimator, fe_twoway = " | cntry + year", fe_country = " | cntry",
                pooled_cl = "")
  wv  <- if (row$weights == "none") NULL else d$w
  m   <- fixest::feols(as.formula(paste0("y ~ ", paste(rhs, collapse = " + "), fe)),
                       data = d, weights = wv, vcov = "iid")
  ct  <- fixest::coeftable(m)
  t   <- ct["x", "t value"]
  df  <- fixest::degrees_freedom(m, type = "resid")
  G   <- n_distinct(d$cntry)
  t_cr1 <- fixest::coeftable(m, vcov = ~cntry)["x", "t value"]
  dummies <- switch(row$estimator, fe_twoway = " + factor(cntry) + factor(year)",
                    fe_country = " + factor(cntry)", pooled_cl = "")
  m_lm <- lm(as.formula(paste("y ~", paste(rhs, collapse = " + "), dummies)), data = d, weights = w)
  cr2 <- if (checks) {
    clubSandwich::coef_test(m_lm, vcov = "CR2", cluster = d$cntry,
                            test = "Satterthwaite", coefs = "x")
  } else list(tstat = NA_real_, df_Satt = NA_real_, p_Satt = NA_real_)
  X <- model.matrix(m_lm)
  rad  <- wcr_test(d$y, X, d$cntry, signs$rademacher, "x", wv)
  webb <- if (checks) wcr_test(d$y, X, d$cntry, signs$webb, "x", wv) else c(p = NA_real_)
  tibble(b = ct["x", "Estimate"], se = ct["x", "Std. Error"], t = t, df = df,
         r = t / sqrt(t^2 + df), p_conv = 2 * pt(-abs(t), df),
         t_cr1 = t_cr1, p_cr1 = 2 * pt(-abs(t_cr1), G - 1),
         t_cr2 = cr2$tstat, df_cr2 = cr2$df_Satt, p_cr2 = cr2$p_Satt,
         p_wcr = rad[["p"]], p_wcr_webb = webb[["p"]],
         n_cells = nrow(d), n_countries = G)
}

# both sign matrices, drawn once
curve_signs <- function() list(rademacher = wcr_signs("rademacher"), webb = wcr_signs("webb"))
