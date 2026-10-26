##############################################################################
# Title:        11_menu_deviations.R
# Purpose:      What each option that is not on the curve would do to it, as
#               data for the map of the decisions. Each such option is swapped
#               into every curve specification to which it applies, refitted,
#               and set beside its twin on the curve: the count (the rate is
#               the count over a constant, which leaves t and r of a linear fit
#               unchanged, so it is checked, not refitted), the four
#               restricted samples, the three regression weights, pooled OLS,
#               random effects and the within-between model of Bell and Jones
#               (2015), the split rule and the two imputation engines (only
#               where the imputation tier holds them), the two series that
#               change the sample, the beta family, and the conventional and
#               CR1 versions of the test. Kept options are paired within the
#               curve, without refits.
#               Each option is tested under the rule that fits it, and the
#               rule is recorded: CR2 errors clustered by country with
#               Satterthwaite df, the rule of the curve, for every linear model,
#               pooled OLS included; conventional and Arellano errors (t on
#               G - 1 df) for random effects; Satterthwaite and CR2 for the
#               within-between model, whose year effects follow the twin; CR1 on
#               G - 1 df for the beta family and the pooled imputation results.
#               r is the descriptive one of the conventional fit (Satterthwaite
#               df for the within-between model, the z on the residual df for
#               the beta family).
#               Levels follow the menu: estimand, measurement, estimation and
#               inference (the test).
# Reads:        cases/eu-frames/data/spec_curve.csv,
#               cases/eu-frames/data/spec_curve_negative.csv,
#               cases/eu-frames/data/teney_panel_measurement.csv,
#               cases/eu-frames/data/teney_panel_weights.csv,
#               cases/eu-frames/data/macro/predictor_measures.csv,
#               cases/eu-frames/data/EUframes_cy.csv,
#               cases/eu-frames/data/spec_grid_mi.csv
# Writes:       cases/eu-frames/data/menu_deviations_rows.csv.gz (one row per
#               pair), cases/eu-frames/data/menu_deviations.csv (one row per
#               curve, axis, option and rule)
# Author:       Chris Moreh
# Last updated: 2026-10-26
##############################################################################

source("R/eu-frames/_curve_tools.R")

inputs <- curve_inputs()
curves <- list(
  positive = datawizard::data_read(out_path("spec_curve.csv"), verbose = FALSE),
  negative = datawizard::data_read(out_path("spec_curve_negative.csv"), verbose = FALSE)
)
mi <- datawizard::data_read(out_path("spec_grid_mi.csv"), verbose = FALSE)

level_of <- c(outcome = "estimand", predictor = "estimand", copredictor = "estimand",
              estimator = "estimand", sample = "estimand", weights = "estimand",
              construction = "estimand", noop = "measurement", item_set = "measurement",
              cell_weight = "measurement", measure = "measurement", pred_form = "estimation",
              family = "estimation", se = "inference")

empty_fit <- \(rule) tibble::tibble(rule = rule, r_dev = NA_real_, b_dev = NA_real_,
                                    t_dev = NA_real_, df_dev = NA_real_, p_dev = NA_real_,
                                    n_cells_dev = NA_integer_, n_countries_dev = NA_integer_)

## ---- the fitters, one per kind of model ------------------------------------------
## The t and p of a coefficient under a supplied variance come from
## lmtest::coeftest() (Zeileis and Hothorn 2002, R News), with the df stated.

fit_dev_linear <- function(row) {
  s <- fit_linear(row, inputs)
  tibble::tibble(rule = "cr2", r_dev = s$r, b_dev = s$b, t_dev = s$t_cr2, df_dev = s$df_cr2,
                 p_dev = s$p_cr2, n_cells_dev = s$n_cells, n_countries_dev = s$n_countries)
}

fit_dev_re <- function(row) {
  d <- spec_data(row, inputs)
  rhs <- paste(rhs_terms(row), collapse = " + ")
  m <- plm::plm(stats::as.formula(paste("y ~", rhs)), data = d, index = c("cntry", "year"),
                model = "random")
  df <- nrow(d) - length(stats::coef(m))
  G <- dplyr::n_distinct(d$cntry)
  # model-based errors, and the Arellano clustered variance of plm (CR1 by
  # country) tested on G - 1 df
  conv <- lmtest::coeftest(m, df = df)["x", ]
  clus <- lmtest::coeftest(m, vcov. = plm::vcovHC(m, method = "arellano", cluster = "group"),
                           df = G - 1)["x", ]
  tibble::tibble(rule = c("re_conventional", "re_cr1_g1"),
                 r_dev = effectsize::t_to_r(conv[["t value"]], df)$r, b_dev = conv[["Estimate"]],
                 t_dev = c(conv[["t value"]], clus[["t value"]]), df_dev = c(df, G - 1),
                 p_dev = c(conv[["Pr(>|t|)"]], clus[["Pr(>|t|)"]]),
                 n_cells_dev = nrow(d), n_countries_dev = G)
}

fit_dev_rewb <- function(row) {
  # the within-between split of Bell and Jones (2015): each predictor as the
  # deviation from its country mean (_within) and the country mean (_between)
  d <- spec_data(row, inputs) |>
    datawizard::demean(select = c("x", "copred"), by = "cntry", verbose = FALSE)
  rhs <- paste(c("x_within", "x_between",
                 if (row$copredictor == "other") c("copred_within", "copred_between"),
                 if (row$estimator == "rewb_year") "factor(year)", "(1 | cntry)"), collapse = " + ")
  m <- lmerTest::lmer(stats::as.formula(paste("y ~", rhs)), data = d, REML = TRUE)
  cf <- summary(m)$coefficients["x_within", ]
  cr2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = d$cntry, test = "Satterthwaite",
                                 coefs = "x_within")
  tibble::tibble(rule = c("rewb_satterthwaite", "rewb_cr2"),
                 r_dev = effectsize::t_to_r(cf[["t value"]], cf[["df"]])$r,
                 b_dev = cf[["Estimate"]], t_dev = c(cf[["t value"]], cr2$tstat),
                 df_dev = c(cf[["df"]], cr2$df_Satt), p_dev = c(cf[["Pr(>|t|)"]], cr2$p_Satt),
                 n_cells_dev = nrow(d), n_countries_dev = dplyr::n_distinct(d$cntry))
}

fit_dev_beta <- function(row) {
  d <- spec_data(row, inputs)
  dummies <- switch(row$estimator, fe_twoway = " + factor(cntry) + factor(year)",
                    fe_country = " + factor(cntry)")
  m <- betareg::betareg(stats::as.formula(paste("y ~", paste(rhs_terms(row), collapse = " + "),
                                                dummies)),
                        data = d, link = "logit")
  z <- summary(m)$coefficients$mean["x", "z value"]
  df <- stats::df.residual(m)
  G <- dplyr::n_distinct(d$cntry)
  # CR1 by country (sandwich::vcovCL; Zeileis, Koell and Graham 2020, Journal
  # of Statistical Software), tested on G - 1 df
  clus <- lmtest::coeftest(m, vcov. = sandwich::vcovCL(m, cluster = d$cntry), df = G - 1)["x", ]
  tibble::tibble(rule = "beta_cr1_g1", r_dev = effectsize::t_to_r(z, df)$r,
                 b_dev = clus[["Estimate"]], t_dev = clus[["t value"]], df_dev = G - 1,
                 p_dev = clus[["Pr(>|t|)"]], n_cells_dev = nrow(d), n_countries_dev = G)
}

fit_dev_mi <- function(row) {
  engine <- if (row$noop == "mi_mice") "mice_pmm" else "amelia_emb"
  hit <- mi |>
    dplyr::filter(engine == !!engine, outcome == row$outcome, copredictor == row$copredictor,
                  estimator == row$estimator, sample == "all", reg_weight == "none")
  conv <- dplyr::filter(hit, se == "conventional")
  clus <- dplyr::filter(hit, se == "clustered")
  if (nrow(conv) != 1 || nrow(clus) != 1 || is.na(conv$statistic)) return(empty_fit("mi_rubin_cr1_g1"))
  tibble::tibble(rule = "mi_rubin_cr1_g1", r_dev = conv$r, b_dev = conv$estimate,
                 t_dev = clus$statistic, df_dev = 26, p_dev = 2 * stats::pt(-abs(clus$statistic), 26),
                 n_cells_dev = conv$n, n_countries_dev = 27L)
}

safely_fit <- function(f, rules) {
  \(row) tryCatch(f(row), error = \(e) purrr::map(rules, empty_fit) |> purrr::list_rbind())
}

## ---- the options that are not on the curve ---------------------------------------------
## `applies` picks the twins on the curve; `swap` turns twin keys into the
## keys of the deviation; `fit` refits one deviation.

std_measure <- \(d) d$predictor == "unemp" & d$measure == "wb_nat" & d$pred_form == "raw" &
  d$item_set == "all" & d$cell_weight == "unweighted"
set_axis <- \(a, v) \(k) dplyr::mutate(k, !!a := v)

off_curve <- tibble::tribble(
  ~axis,          ~option,         ~applies,                     ~swap,                              ~fit,
  "construction", "count",         \(d) rep(TRUE, nrow(d)),       set_axis("construction", "count"),  safely_fit(fit_dev_linear, "cr2"),
  "sample",       "y2004_2008",    \(d) rep(TRUE, nrow(d)),       set_axis("sample", "y2004_2008"),   safely_fit(fit_dev_linear, "cr2"),
  "sample",       "y2009_2013",    \(d) rep(TRUE, nrow(d)),       set_axis("sample", "y2009_2013"),   safely_fit(fit_dev_linear, "cr2"),
  "sample",       "no_bailout",    \(d) rep(TRUE, nrow(d)),       set_axis("sample", "no_bailout"),   safely_fit(fit_dev_linear, "cr2"),
  "sample",       "no_GR_ES",      \(d) rep(TRUE, nrow(d)),       set_axis("sample", "no_GR_ES"),     safely_fit(fit_dev_linear, "cr2"),
  "weights",      "n_cell",        \(d) rep(TRUE, nrow(d)),       set_axis("weights", "n_cell"),      safely_fit(fit_dev_linear, "cr2"),
  "weights",      "n_eff",         \(d) rep(TRUE, nrow(d)),       set_axis("weights", "n_eff"),       safely_fit(fit_dev_linear, "cr2"),
  "weights",      "pop",           \(d) rep(TRUE, nrow(d)),       set_axis("weights", "pop"),         safely_fit(fit_dev_linear, "cr2"),
  "estimator",    "pooled_cl",     \(d) rep(TRUE, nrow(d)),       set_axis("estimator", "pooled_cl"), safely_fit(fit_dev_linear, "cr2"),
  "estimator",    "re",            \(d) rep(TRUE, nrow(d)),       set_axis("estimator", "re"),
    safely_fit(fit_dev_re, c("re_conventional", "re_cr1_g1")),
  "estimator",    "rewb",          \(d) rep(TRUE, nrow(d)),
    \(k) dplyr::mutate(k, estimator = dplyr::if_else(estimator == "fe_twoway", "rewb_year", "rewb")),
    safely_fit(fit_dev_rewb, c("rewb_satterthwaite", "rewb_cr2")),
  "noop",         "split",         \(d) rep(TRUE, nrow(d)),       set_axis("noop", "split"),          safely_fit(fit_dev_linear, "cr2"),
  "noop",         "mi_mice",       std_measure,                  set_axis("noop", "mi_mice"),        safely_fit(fit_dev_mi, "mi_rubin_cr1_g1"),
  "noop",         "mi_amelia",     std_measure,                  set_axis("noop", "mi_amelia"),      safely_fit(fit_dev_mi, "mi_rubin_cr1_g1"),
  "measure",      "es_1574_tips",  \(d) d$predictor == "unemp",  set_axis("measure", "es_1574_tips"), safely_fit(fit_dev_linear, "cr2"),
  "measure",      "es_growth",     \(d) d$predictor == "growth", set_axis("measure", "es_growth"),   safely_fit(fit_dev_linear, "cr2"),
  "family",       "beta",          \(d) rep(TRUE, nrow(d)),       set_axis("family", "beta"),         safely_fit(fit_dev_beta, "beta_cr1_g1")
)

twin_cols <- \(d) dplyr::select(d, dplyr::all_of(axis_codes), r_ca_twin = r_ca, p_twin = p_cr2,
                                p_twin_cr1 = p_cr1)

run_off_curve <- function(curve, def) {
  twins <- dplyr::filter(curve, def$applies(curve))
  keys  <- twins |> datawizard::data_select(select = axis_codes) |> def$swap()
  specs <- dplyr::distinct(keys)
  fits  <- purrr::map(seq_len(nrow(specs)), \(i) def$fit(specs[i, ]) |> dplyr::bind_cols(specs[i, ])) |>
    purrr::list_rbind()
  twin_cols(twins) |>
    dplyr::bind_cols(datawizard::data_addprefix(keys, ".dev_")) |>
    dplyr::inner_join(dplyr::rename_with(fits, \(n) paste0(".dev_", n), dplyr::all_of(axis_codes)),
                      by = paste0(".dev_", axis_codes), relationship = "many-to-many") |>
    dplyr::mutate(axis = def$axis, option = def$option,
                  r_ca_dev = r_dev * align_factor(outcome, predictor)) |>
    dplyr::select(-dplyr::starts_with(".dev_"), -r_dev)
}

## ---- the inference rules that are not on the curve, without refits ---------------

run_se <- function(curve) {
  rules <- tibble::tribble(~opt, ~t_col, ~p_col,
                           "conventional", "t", "p_conv",
                           "cr1_g1", "t_cr1", "p_cr1")
  purrr::pmap(rules, \(opt, t_col, p_col) {
    twin_cols(curve) |>
      dplyr::mutate(axis = "se", option = opt, rule = opt, r_ca_dev = r_ca_twin, b_dev = curve$b,
                    t_dev = curve[[t_col]],
                    df_dev = if (opt == "cr1_g1") curve$n_countries - 1 else curve$df,
                    p_dev = curve[[p_col]], n_cells_dev = curve$n_cells,
                    n_countries_dev = curve$n_countries)
  }) |>
    purrr::list_rbind()
}

## ---- the options kept on the curve: pairs within it --------------------------------

run_kept <- function(curve) {
  base <- curve |>
    dplyr::mutate(pair_measure = dplyr::if_else(
      predictor == "unemp" & measure == "wb_nat" & pred_form == "raw", "wb",
      dplyr::if_else(predictor == "growth", "wb", "other")))
  axes <- c("outcome", "predictor", "copredictor", "estimator", "noop", "item_set",
            "cell_weight", "measure", "pred_form")
  purrr::map(axes, \(a) {
    d <- base
    keys <- setdiff(axis_codes, a)
    if (a == "predictor") {
      d <- datawizard::data_filter(d, pair_measure == "wb")
      keys <- setdiff(keys, c("measure", "pred_form"))
    }
    if (a %in% c("measure", "pred_form")) d <- datawizard::data_filter(d, predictor == "unemp")
    left  <- twin_cols(d) |> dplyr::rename(.lev_twin = dplyr::all_of(a))
    right <- dplyr::select(d, dplyr::all_of(keys), .lev_dev = dplyr::all_of(a), r_ca_dev = r_ca,
                           b_dev = b, t_dev = t_cr2, df_dev = df_cr2, p_dev = p_cr2,
                           n_cells_dev = n_cells, n_countries_dev = n_countries)
    dplyr::inner_join(left, right, by = keys, relationship = "many-to-many") |>
      datawizard::data_filter(.lev_twin != .lev_dev) |>
      dplyr::mutate(!!a := .lev_twin, axis = a, option = .lev_dev, rule = "cr2") |>
      dplyr::select(-.lev_twin, -.lev_dev)
  }) |>
    purrr::list_rbind()
}

## ---- assemble ---------------------------------------------------------------------

rows <- purrr::imap(curves, \(curve, nm) {
  off <- purrr::map(seq_len(nrow(off_curve)),
                    \(i) run_off_curve(curve, off_curve[i, ] |> as.list() |> purrr::map(\(x) x[[1]]))) |>
    purrr::list_rbind()
  dplyr::bind_rows(off |> dplyr::mutate(on_curve = FALSE),
                   run_se(curve) |> dplyr::mutate(on_curve = FALSE),
                   run_kept(curve) |> dplyr::mutate(on_curve = TRUE)) |>
    dplyr::mutate(curve = nm, level = unname(level_of[axis]))
}) |>
  purrr::list_rbind() |>
  dplyr::select(curve, axis, option, rule, level, on_curve, dplyr::all_of(axis_codes), r_ca_twin,
                p_twin, p_twin_cr1, r_ca_dev, p_dev, b_dev, t_dev, df_dev, n_cells_dev,
                n_countries_dev)

# r-style: keep – datawizard::data_write() cannot write a gzip-compressed CSV
readr::write_csv(rows, out_path("menu_deviations_rows.csv.gz"))

## ---- the rate: the count over a constant ---------------------------------------------

rate_check <- purrr::imap(curves, \(curve, nm) {
  purrr::map(seq_len(nrow(curve)), \(i) {
    k_count <- curve[i, axis_codes] |> datawizard::data_modify(construction = "count")
    k_rate  <- curve[i, axis_codes] |> datawizard::data_modify(construction = "rate")
    fit_conv <- \(k) {
      d <- spec_data(k, inputs)
      fe <- if (k$estimator == "fe_twoway") " | cntry + year" else " | cntry"
      m <- fixest::feols(stats::as.formula(paste0("y ~ ", paste(rhs_terms(k), collapse = " + "), fe)),
                         data = d, vcov = "iid")
      t <- fixest::tstat(m)[["x"]]
      c(t = t, r = effectsize::t_to_r(t, fixest::degrees_freedom(m, type = "resid"))$r)
    }
    abs(fit_conv(k_count) - fit_conv(k_rate))
  }) |>
    purrr::reduce(pmax)
})
print(rate_check)
stopifnot("the rate does not reproduce the count" = all(unlist(rate_check) < 1e-9))

## ---- the summary table ------------------------------------------------------------------

summ <- rows |>
  dplyr::summarise(level = dplyr::first(level), on_curve = dplyr::first(on_curve), n_pairs = dplyr::n(),
                   below_zero = 100 * mean(r_ca_dev < 0, na.rm = TRUE),
                   below_zero_sig = 100 * mean(r_ca_dev < 0 & p_dev < 0.05, na.rm = TRUE),
                   median_r_ca = stats::median(r_ca_dev, na.rm = TRUE),
                   median_shift = stats::median(r_ca_dev - r_ca_twin, na.rm = TRUE),
                   sign_changes = 100 * mean((r_ca_dev < 0) != (r_ca_twin < 0), na.rm = TRUE),
                   class_changes = 100 * mean((r_ca_dev < 0 & p_dev < 0.05) !=
                                                (r_ca_twin < 0 & p_twin < 0.05), na.rm = TRUE),
                   n_failed = sum(is.na(r_ca_dev)),
                   .by = c(curve, axis, option, rule))

# options that the curve fixes, and the single growth series, have no pairs
# within the curve: their shares describe the curve specifications that carry
# them, and the pair columns are empty
fixed <- purrr::imap(curves, \(curve, nm) {
  tibble::tribble(~axis, ~option, ~keep,
                  "sample", "all", rep(TRUE, nrow(curve)),
                  "weights", "none", rep(TRUE, nrow(curve)),
                  "construction", "share", rep(TRUE, nrow(curve)),
                  "family", "gaussian", rep(TRUE, nrow(curve)),
                  "measure", "wb_growth", curve$measure == "wb_growth") |>
    purrr::pmap(\(axis, option, keep) {
      d <- curve[keep, ]
      tibble::tibble(curve = nm, axis = axis, option = option, rule = "cr2",
                     level = unname(level_of[axis]), on_curve = TRUE, n_pairs = 0L,
                     below_zero = 100 * mean(d$r_ca < 0),
                     below_zero_sig = 100 * mean(d$r_ca < 0 & d$sig),
                     median_r_ca = stats::median(d$r_ca), median_shift = NA_real_,
                     sign_changes = NA_real_, class_changes = NA_real_, n_failed = 0L)
    }) |>
    purrr::list_rbind()
}) |>
  purrr::list_rbind()

rate_rows <- summ |>
  datawizard::data_filter(axis == "construction" & option == "count") |>
  datawizard::data_modify(option = "rate", rule = "identical_to_count")

deviations <- dplyr::bind_rows(summ, rate_rows, fixed) |>
  # r-style: keep – dplyr::arrange() sorts in the C locale on every machine
  dplyr::arrange(curve, factor(level, c("estimand", "measurement", "estimation", "inference")),
                 axis, dplyr::desc(on_curve), option, rule)

datawizard::data_write(deviations, out_path("menu_deviations.csv"), verbose = FALSE)

deviations |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) round(x, 3))) |>
  as.data.frame() |>
  print(row.names = FALSE)
