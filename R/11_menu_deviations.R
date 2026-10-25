##############################################################################
# Title:        11_menu_deviations.R
# Purpose:      What each option that left the curve would have done to it,
#               as data for the map of the decisions. Each such option is
#               swapped into every curve specification to which it applies,
#               refitted, and set beside its twin on the curve: the count (the
#               rate is the count over a constant, which leaves t and r of a
#               linear fit unchanged, so it is checked, not refitted), the
#               four restricted samples, the three regression weights, pooled
#               OLS, random effects and the within-between model of Bell and
#               Jones (2015), the split rule and the two imputation engines
#               (only where the imputation tier holds them), the two series
#               that change the sample, the beta family, and the conventional,
#               CR1, CR2 and Webb versions of the test. Kept options are paired
#               within the curve, without refits.
#               Each option is tested under the rule that fits it, and the
#               rule is recorded: the wild cluster bootstrap by country for
#               every linear model, pooled OLS included; conventional and
#               Arellano errors (t on G - 1 df) for random effects;
#               Satterthwaite and CR2 for the within-between model, whose year
#               effects follow the twin; CR1 on G - 1 df for the beta family
#               and the pooled imputation results. r is the descriptive one of
#               the conventional fit (Satterthwaite df for the within-between
#               model, the z on the residual df for the beta family).
#               Levels follow the menu: estimand, measurement, estimation and
#               inference (the test).
# Reads:        data/spec_curve.csv, data/spec_curve_negative.csv,
#               data/teney_panel_measurement.csv, data/teney_panel_weights.csv,
#               data/macro/predictor_measures.csv, data/EUframes_cy.csv,
#               data/spec_grid_mi.csv
# Writes:       data/menu_deviations_rows.csv.gz (one row per pair),
#               data/menu_deviations.csv (one row per curve, axis, option and
#               rule)
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr)
  library(plm); library(lme4); library(lmerTest); library(betareg); library(sandwich)
})
source("R/_curve_tools.R")

inputs <- curve_inputs()
signs  <- curve_signs()
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

empty_fit <- \(rule) tibble(rule = rule, r_dev = NA_real_, b_dev = NA_real_, t_dev = NA_real_,
                           df_dev = NA_real_, p_dev = NA_real_, n_cells_dev = NA_integer_,
                           n_countries_dev = NA_integer_)

## ---- the fitters, one per kind of model ------------------------------------------

fit_dev_linear <- function(row) {
  s <- fit_linear(row, inputs, signs, checks = FALSE)
  tibble(rule = "wcr_rademacher", r_dev = s$r, b_dev = s$b, t_dev = s$t, df_dev = s$df,
         p_dev = s$p_wcr, n_cells_dev = s$n_cells, n_countries_dev = s$n_countries)
}

fit_dev_re <- function(row) {
  d <- spec_data(row, inputs)
  rhs <- paste(rhs_terms(row), collapse = " + ")
  m <- plm(as.formula(paste("y ~", rhs)), data = d, index = c("cntry", "year"), model = "random")
  b <- coef(m)[["x"]]
  df <- nrow(d) - length(coef(m))
  G <- n_distinct(d$cntry)
  t_c <- b / sqrt(vcov(m)["x", "x"])
  t_k <- b / sqrt(vcovHC(m, method = "arellano", cluster = "group")["x", "x"])
  tibble(rule = c("re_conventional", "re_cr1_g1"), r_dev = t_c / sqrt(t_c^2 + df), b_dev = b,
         t_dev = c(t_c, t_k), df_dev = c(df, G - 1),
         p_dev = c(2 * pt(-abs(t_c), df), 2 * pt(-abs(t_k), G - 1)),
         n_cells_dev = nrow(d), n_countries_dev = G)
}

fit_dev_rewb <- function(row) {
  d <- spec_data(row, inputs) |>
    mutate(x_mean = mean(x), x_dev = x - x_mean, c_mean = mean(copred), c_dev = copred - c_mean,
           .by = cntry)
  rhs <- paste(c("x_dev", "x_mean", if (row$copredictor == "other") c("c_dev", "c_mean"),
                 if (row$estimator == "rewb_year") "factor(year)", "(1 | cntry)"), collapse = " + ")
  m <- lmerTest::lmer(as.formula(paste("y ~", rhs)), data = d, REML = TRUE)
  cf <- summary(m)$coefficients
  t_s <- cf["x_dev", "t value"]; df_s <- cf["x_dev", "df"]
  cr2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = d$cntry, test = "Satterthwaite",
                                 coefs = "x_dev")
  tibble(rule = c("rewb_satterthwaite", "rewb_cr2"), r_dev = t_s / sqrt(t_s^2 + df_s),
         b_dev = cf["x_dev", "Estimate"], t_dev = c(t_s, cr2$tstat), df_dev = c(df_s, cr2$df_Satt),
         p_dev = c(cf["x_dev", "Pr(>|t|)"], cr2$p_Satt),
         n_cells_dev = nrow(d), n_countries_dev = n_distinct(d$cntry))
}

fit_dev_beta <- function(row) {
  d <- spec_data(row, inputs)
  dummies <- switch(row$estimator, fe_twoway = " + factor(cntry) + factor(year)",
                    fe_country = " + factor(cntry)")
  m <- betareg(as.formula(paste("y ~", paste(rhs_terms(row), collapse = " + "), dummies)),
               data = d, link = "logit")
  ct <- summary(m)$coefficients$mean
  z <- ct["x", "z value"]; df <- df.residual(m); G <- n_distinct(d$cntry)
  t_k <- ct["x", "Estimate"] / sqrt(vcovCL(m, cluster = d$cntry)["x", "x"])
  tibble(rule = "beta_cr1_g1", r_dev = z / sqrt(z^2 + df), b_dev = ct["x", "Estimate"],
         t_dev = t_k, df_dev = G - 1, p_dev = 2 * pt(-abs(t_k), G - 1),
         n_cells_dev = nrow(d), n_countries_dev = G)
}

fit_dev_mi <- function(row) {
  engine <- if (row$noop == "mi_mice") "mice_pmm" else "amelia_emb"
  hit <- mi |>
    filter(engine == !!engine, outcome == row$outcome, copredictor == row$copredictor,
           estimator == row$estimator, sample == "all", reg_weight == "none")
  conv <- filter(hit, se == "conventional"); clus <- filter(hit, se == "clustered")
  if (nrow(conv) != 1 || nrow(clus) != 1 || is.na(conv$statistic)) return(empty_fit("mi_rubin_cr1_g1"))
  tibble(rule = "mi_rubin_cr1_g1", r_dev = conv$r, b_dev = conv$estimate, t_dev = clus$statistic,
         df_dev = 26, p_dev = 2 * pt(-abs(clus$statistic), 26), n_cells_dev = conv$n,
         n_countries_dev = 27L)
}

safely_fit <- function(f, rules) {
  \(row) tryCatch(f(row), error = \(e) map(rules, empty_fit) |> list_rbind())
}

## ---- the options that left the curve ------------------------------------------------
## `applies` picks the twins on the curve; `swap` turns twin keys into the
## keys of the deviation; `fit` refits one deviation.

std_measure <- \(d) d$predictor == "unemp" & d$measure == "wb_nat" & d$pred_form == "raw" &
  d$item_set == "all" & d$cell_weight == "unweighted"
set_axis <- \(a, v) \(k) mutate(k, !!a := v)

off_curve <- tribble(
  ~axis,          ~option,         ~applies,                          ~swap,                              ~fit,
  "construction", "count",         \(d) rep(TRUE, nrow(d)),           set_axis("construction", "count"),  safely_fit(fit_dev_linear, "wcr_rademacher"),
  "sample",       "y2004_2008",    \(d) rep(TRUE, nrow(d)),           set_axis("sample", "y2004_2008"),   safely_fit(fit_dev_linear, "wcr_rademacher"),
  "sample",       "y2009_2013",    \(d) rep(TRUE, nrow(d)),           set_axis("sample", "y2009_2013"),   safely_fit(fit_dev_linear, "wcr_rademacher"),
  "sample",       "no_bailout",    \(d) rep(TRUE, nrow(d)),           set_axis("sample", "no_bailout"),   safely_fit(fit_dev_linear, "wcr_rademacher"),
  "sample",       "no_GR_ES",      \(d) rep(TRUE, nrow(d)),           set_axis("sample", "no_GR_ES"),     safely_fit(fit_dev_linear, "wcr_rademacher"),
  "weights",      "n_cell",        \(d) rep(TRUE, nrow(d)),           set_axis("weights", "n_cell"),      safely_fit(fit_dev_linear, "wcr_rademacher"),
  "weights",      "n_eff",         \(d) rep(TRUE, nrow(d)),           set_axis("weights", "n_eff"),       safely_fit(fit_dev_linear, "wcr_rademacher"),
  "weights",      "pop",           \(d) rep(TRUE, nrow(d)),           set_axis("weights", "pop"),         safely_fit(fit_dev_linear, "wcr_rademacher"),
  "estimator",    "pooled_cl",     \(d) rep(TRUE, nrow(d)),           set_axis("estimator", "pooled_cl"), safely_fit(fit_dev_linear, "wcr_rademacher"),
  "estimator",    "re",            \(d) rep(TRUE, nrow(d)),           set_axis("estimator", "re"),
    safely_fit(fit_dev_re, c("re_conventional", "re_cr1_g1")),
  "estimator",    "rewb",          \(d) rep(TRUE, nrow(d)),
    \(k) mutate(k, estimator = if_else(estimator == "fe_twoway", "rewb_year", "rewb")),
    safely_fit(fit_dev_rewb, c("rewb_satterthwaite", "rewb_cr2")),
  "noop",         "split",         \(d) rep(TRUE, nrow(d)),           set_axis("noop", "split"),          safely_fit(fit_dev_linear, "wcr_rademacher"),
  "noop",         "mi_mice",       std_measure,                       set_axis("noop", "mi_mice"),        safely_fit(fit_dev_mi, "mi_rubin_cr1_g1"),
  "noop",         "mi_amelia",     std_measure,                       set_axis("noop", "mi_amelia"),      safely_fit(fit_dev_mi, "mi_rubin_cr1_g1"),
  "measure",      "es_1574_tips",  \(d) d$predictor == "unemp",       set_axis("measure", "es_1574_tips"), safely_fit(fit_dev_linear, "wcr_rademacher"),
  "measure",      "es_growth",     \(d) d$predictor == "growth",      set_axis("measure", "es_growth"),   safely_fit(fit_dev_linear, "wcr_rademacher"),
  "family",       "beta",          \(d) rep(TRUE, nrow(d)),           set_axis("family", "beta"),         safely_fit(fit_dev_beta, "beta_cr1_g1")
)

twin_cols <- \(d) d |> select(all_of(axis_codes), r_ca_twin = r_ca, p_twin = p_wcr, p_twin_cr1 = p_cr1)

run_off_curve <- function(curve, def) {
  twins <- curve |> filter(def$applies(curve))
  keys  <- twins |> select(all_of(axis_codes)) |> def$swap()
  specs <- distinct(keys)
  fits  <- map(seq_len(nrow(specs)), \(i) def$fit(specs[i, ]) |> bind_cols(specs[i, ])) |>
    list_rbind()
  twin_cols(twins) |>
    bind_cols(keys |> rename_with(\(n) paste0(".dev_", n))) |>
    inner_join(fits |> rename_with(\(n) paste0(".dev_", n), all_of(axis_codes)),
               by = paste0(".dev_", axis_codes), relationship = "many-to-many") |>
    mutate(axis = def$axis, option = def$option,
           r_ca_dev = r_dev * align_factor(outcome, predictor)) |>
    select(-starts_with(".dev_"), -r_dev)
}

## ---- the inference rules, without refits -----------------------------------------

run_se <- function(curve) {
  rules <- tribble(~opt, ~t_col, ~df_col, ~p_col,
                   "conventional", "t", "df", "p_conv",
                   "cr1_g1", "t_cr1", NA, "p_cr1",
                   "cr2", "t_cr2", "df_cr2", "p_cr2",
                   "wcr_webb", NA, NA, "p_wcr_webb")
  pmap(rules, \(opt, t_col, df_col, p_col) {
    df_dev <- if (opt == "cr1_g1") curve$n_countries - 1
              else if (is.na(df_col)) NA_real_ else curve[[df_col]]
    twin_cols(curve) |>
      mutate(axis = "se", option = opt, rule = opt, r_ca_dev = r_ca_twin, b_dev = curve$b,
             t_dev = if (is.na(t_col)) NA_real_ else curve[[t_col]], df_dev = df_dev,
             p_dev = curve[[p_col]], n_cells_dev = curve$n_cells,
             n_countries_dev = curve$n_countries)
  }) |> list_rbind()
}

## ---- the options kept on the curve: pairs within it --------------------------------

run_kept <- function(curve) {
  base <- curve |>
    mutate(pair_measure = if_else(predictor == "unemp" & measure == "wb_nat" & pred_form == "raw",
                                  "wb", if_else(predictor == "growth", "wb", "other")))
  axes <- c("outcome", "predictor", "copredictor", "estimator", "noop", "item_set",
            "cell_weight", "measure", "pred_form")
  map(axes, \(a) {
    d <- base
    keys <- setdiff(axis_codes, a)
    if (a == "predictor") {
      d <- filter(d, pair_measure == "wb")
      keys <- setdiff(keys, c("measure", "pred_form"))
    }
    if (a %in% c("measure", "pred_form")) d <- filter(d, predictor == "unemp")
    left  <- twin_cols(d) |> rename(.lev_twin = all_of(a))
    right <- d |> select(all_of(keys), .lev_dev = all_of(a), r_ca_dev = r_ca, b_dev = b,
                         t_dev = t, df_dev = df, p_dev = p_wcr, n_cells_dev = n_cells,
                         n_countries_dev = n_countries)
    inner_join(left, right, by = keys, relationship = "many-to-many") |>
      filter(.lev_twin != .lev_dev) |>
      mutate(!!a := .lev_twin, axis = a, option = .lev_dev, rule = "wcr_rademacher") |>
      select(-.lev_twin, -.lev_dev)
  }) |> list_rbind()
}

## ---- assemble ---------------------------------------------------------------------

rows <- imap(curves, \(curve, nm) {
  off <- map(seq_len(nrow(off_curve)), \(i) run_off_curve(curve, off_curve[i, ] |>
                                                            as.list() |> map(\(x) x[[1]]))) |>
    list_rbind()
  bind_rows(off |> mutate(on_curve = FALSE), run_se(curve) |> mutate(on_curve = FALSE),
            run_kept(curve) |> mutate(on_curve = TRUE)) |>
    mutate(curve = nm, level = unname(level_of[axis]))
}) |>
  list_rbind() |>
  select(curve, axis, option, rule, level, on_curve, all_of(axis_codes), r_ca_twin, p_twin,
         p_twin_cr1, r_ca_dev, p_dev, b_dev, t_dev, df_dev, n_cells_dev, n_countries_dev)

readr::write_csv(rows, out_path("menu_deviations_rows.csv.gz"))

## ---- the rate: the count over a constant ---------------------------------------------

rate_check <- imap(curves, \(curve, nm) {
  map(seq_len(nrow(curve)), \(i) {
    k_count <- curve[i, axis_codes] |> mutate(construction = "count")
    k_rate  <- curve[i, axis_codes] |> mutate(construction = "rate")
    fit_conv <- \(k) {
      d <- spec_data(k, inputs)
      fe <- if (k$estimator == "fe_twoway") " | cntry + year" else " | cntry"
      m <- fixest::feols(as.formula(paste0("y ~ ", paste(rhs_terms(k), collapse = " + "), fe)),
                         data = d, vcov = "iid")
      t <- fixest::tstat(m)[["x"]]; df <- fixest::degrees_freedom(m, type = "resid")
      c(t = t, r = t / sqrt(t^2 + df))
    }
    abs(fit_conv(k_count) - fit_conv(k_rate))
  }) |> reduce(pmax)
})
print(rate_check)
stopifnot("the rate does not reproduce the count" = all(unlist(rate_check) < 1e-9))

## ---- the summary table ------------------------------------------------------------------

summ <- rows |>
  summarise(level = first(level), on_curve = first(on_curve), n_pairs = n(),
            below_zero = 100 * mean(r_ca_dev < 0, na.rm = TRUE),
            below_zero_sig = 100 * mean(r_ca_dev < 0 & p_dev < 0.05, na.rm = TRUE),
            median_r_ca = median(r_ca_dev, na.rm = TRUE),
            median_shift = median(r_ca_dev - r_ca_twin, na.rm = TRUE),
            sign_changes = 100 * mean((r_ca_dev < 0) != (r_ca_twin < 0), na.rm = TRUE),
            class_changes = 100 * mean((r_ca_dev < 0 & p_dev < 0.05) !=
                                         (r_ca_twin < 0 & p_twin < 0.05), na.rm = TRUE),
            n_failed = sum(is.na(r_ca_dev)),
            .by = c(curve, axis, option, rule))

# options that the curve fixes, and the single growth series, have no pairs
# within the curve: their shares describe the curve specifications that carry
# them, and the pair columns are empty
fixed <- imap(curves, \(curve, nm) {
  tribble(~axis, ~option, ~keep,
          "sample", "all", rep(TRUE, nrow(curve)),
          "weights", "none", rep(TRUE, nrow(curve)),
          "construction", "share", rep(TRUE, nrow(curve)),
          "family", "gaussian", rep(TRUE, nrow(curve)),
          "measure", "wb_growth", curve$measure == "wb_growth") |>
    pmap(\(axis, option, keep) {
      d <- curve[keep, ]
      tibble(curve = nm, axis = axis, option = option, rule = "wcr_rademacher",
             level = unname(level_of[axis]), on_curve = TRUE, n_pairs = 0L,
             below_zero = 100 * mean(d$r_ca < 0), below_zero_sig = 100 * mean(d$r_ca < 0 & d$sig),
             median_r_ca = median(d$r_ca), median_shift = NA_real_, sign_changes = NA_real_,
             class_changes = NA_real_, n_failed = 0L)
    }) |> list_rbind()
}) |> list_rbind()

rate_rows <- summ |>
  filter(axis == "construction", option == "count") |>
  mutate(option = "rate", rule = "identical_to_count")

deviations <- bind_rows(summ, rate_rows, fixed) |>
  arrange(curve, factor(level, c("estimand", "measurement", "estimation", "inference")),
          axis, desc(on_curve), option, rule)

datawizard::data_write(deviations, out_path("menu_deviations.csv"), verbose = FALSE)

deviations |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  as.data.frame() |>
  print(row.names = FALSE)
