##############################################################################
# Title:        12_displays.R
# Purpose:      The tables behind the displays that keep in the course what
#               the curve leaves out, at the standard measurement settings
#               (shares of all 13 items, zero rule, cells as interviewed,
#               World Bank series), each linear fit with the inference columns
#               of the curve. (a) The slope within countries, the slope across
#               the 27 country means, and the two slopes of the within-between
#               model (Bell and Jones 2015) with the Satterthwaite test of
#               their difference, by positive scale and predictor; and the 27
#               country means. (b) The 36 core specifications on the full
#               panel, on 2004-2008 and on 2009-2013. (c) The reference
#               specification and its country twin without each country in
#               turn, with the share of the variation in unemployment left
#               after the fixed effects that each country holds. (d) The slope
#               in the eight countries flagged for financial assistance and in
#               the other nineteen, and their difference in one interaction
#               model; with eight clusters the Rademacher bootstrap has only
#               256 sign patterns, so the Webb version is the one to read
#               there. (e) The reference cell and its country twin under
#               conventional, CR1 (G - 1 df), CR2 and bootstrap inference, with
#               the r that the conversion formula returns from each t and df
#               beside the descriptive r, which does not change.
# Reads:        data/EUframes_cy.csv, data/teney_panel_measurement.csv,
#               data/teney_panel_weights.csv, data/macro/predictor_measures.csv,
#               data/spec_curve.csv
# Writes:       data/display_between_within.csv, data/display_country_means.csv,
#               data/display_periods.csv, data/display_loo.csv,
#               data/display_bailout.csv, data/display_inference.csv
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(lme4); library(lmerTest)
})
source("R/_curve_tools.R")

inputs <- curve_inputs()
signs  <- curve_signs()
panel  <- datawizard::data_read("data/EUframes_cy.csv", verbose = FALSE)
curve  <- datawizard::data_read(out_path("spec_curve.csv"), verbose = FALSE)

standard <- curve_menu(positive_scales) |>
  filter(noop == "zero", item_set == "all", cell_weight == "unweighted",
         measure %in% c("wb_nat", "wb_growth"))
reference <- standard |>
  filter(outcome == "mcosmo", predictor == "unemp", pred_form == "raw", copredictor == "none")

with_class <- \(d) d |>
  mutate(r_ca = r * align_factor(outcome, predictor), supports = r_ca < 0, sig = p_wcr < 0.05)
fit_rows <- \(rows) map(seq_len(nrow(rows)), \(i) fit_linear(rows[i, ], inputs, signs)) |>
  list_rbind() |> bind_cols(rows) |> with_class()

## ---- (a) within and between ------------------------------------------------------

bw_rows <- standard |>
  filter(pred_form == "raw", copredictor == "none", estimator == "fe_country")

between_within <- map(seq_len(nrow(bw_rows)), \(i) {
  row <- bw_rows[i, ]
  d <- spec_data(row, inputs) |> mutate(x_mean = mean(x), x_dev = x - x_mean, .by = cntry)
  within <- fit_linear(row, inputs, signs, d = d)
  means <- d |> summarise(y = mean(y), x = mean(x), .by = cntry)
  bm <- summary(lm(y ~ x, data = means))$coefficients
  m <- lmerTest::lmer(y ~ x_dev + x_mean + (1 | cntry), data = d, REML = TRUE)
  cf <- summary(m)$coefficients
  dif <- lmerTest::contest(m, L = as.numeric(rownames(cf) == "x_dev") -
                             as.numeric(rownames(cf) == "x_mean"), joint = FALSE)
  tibble(outcome = row$outcome, predictor = row$predictor, measure = row$measure,
         b_within = within$b, se_within = within$se, r_within = within$r,
         p_within_wcr = within$p_wcr, p_within_cr2 = within$p_cr2,
         b_between = bm["x", "Estimate"], se_between = bm["x", "Std. Error"],
         t_between = bm["x", "t value"], df_between = nrow(means) - 2,
         p_between = bm["x", "Pr(>|t|)"],
         r_between = cor(means$x, means$y),
         b_rewb_within = cf["x_dev", "Estimate"], t_rewb_within = cf["x_dev", "t value"],
         df_rewb_within = cf["x_dev", "df"], p_rewb_within = cf["x_dev", "Pr(>|t|)"],
         b_rewb_between = cf["x_mean", "Estimate"], t_rewb_between = cf["x_mean", "t value"],
         df_rewb_between = cf["x_mean", "df"], p_rewb_between = cf["x_mean", "Pr(>|t|)"],
         b_difference = dif$Estimate, t_difference = dif[["t value"]], df_difference = dif$df,
         p_difference = dif[["Pr(>|t|)"]], n_cells = nrow(d), n_countries = nrow(means))
}) |> list_rbind()

country_means <- panel |>
  summarise(country = first(country), n_years = n(), bailout = as.integer(any(bailout == 1)),
            across(c(unemp, growth, mcosmo, mutil, mpos, mcomm, mlib, mneg), mean), .by = cntry) |>
  rename_with(\(n) paste0(n, "_mean"), c(unemp, growth, mcosmo, mutil, mpos, mcomm, mlib, mneg)) |>
  arrange(cntry)

## ---- (b) periods ---------------------------------------------------------------------

periods <- c("all", "y2004_2008", "y2009_2013") |>
  map(\(s) fit_rows(standard |> mutate(sample = s))) |>
  list_rbind() |>
  select(all_of(axis_codes), b, se, t, df, r, r_ca, p_conv, t_cr1, p_cr1, t_cr2, df_cr2, p_cr2,
         p_wcr, p_wcr_webb, supports, sig, n_cells, n_countries)

## ---- (c) leave one country out -------------------------------------------------------

loo_rows <- bind_rows(reference |> filter(estimator == "fe_twoway"),
                      reference |> filter(estimator == "fe_country"))

# the share of the variation in the predictor, net of the fixed effects of the
# design, that each country holds
variation_share <- function(row) {
  d <- spec_data(row, inputs)
  fe <- if (row$estimator == "fe_twoway") "x ~ 1 | cntry + year" else "x ~ 1 | cntry"
  xt <- resid(fixest::feols(as.formula(fe), data = d))
  tibble(left_out = d$cntry, xt2 = xt^2) |>
    summarise(v = sum(xt2), .by = left_out) |>
    transmute(left_out, variation_share = 100 * v / sum(v))
}

loo <- map(seq_len(nrow(loo_rows)), \(i) {
  row <- loo_rows[i, ]
  d_all <- spec_data(row, inputs)
  fits <- map(c("none", wcr_countries), \(cc) {
    fit_linear(row, inputs, signs, d = filter(d_all, cntry != cc)) |> mutate(left_out = cc)
  }) |> list_rbind()
  fits |>
    left_join(variation_share(row), by = "left_out") |>
    mutate(estimator = row$estimator, outcome = row$outcome, predictor = row$predictor,
           r_ca = r * align_factor(outcome, predictor))
}) |>
  list_rbind() |>
  select(estimator, left_out, outcome, predictor, b, se, t, df, r, r_ca, p_conv, p_cr2, p_wcr,
         p_wcr_webb, n_cells, n_countries, variation_share)

## ---- (d) bailout countries ---------------------------------------------------------

bail_rows <- standard |> filter(pred_form == "raw", copredictor == "none")

bailout <- map(seq_len(nrow(bail_rows)), \(i) {
  row <- bail_rows[i, ]
  d <- spec_data(row, inputs) |> mutate(bail = as.numeric(cntry %in% inputs$bailout), xb = x * bail)
  groups <- list(bailout = filter(d, bail == 1), other = filter(d, bail == 0)) |>
    imap(\(dd, g) fit_linear(row, inputs, signs, d = dd) |> mutate(group = g)) |>
    list_rbind()
  # the difference in one model: x for the other countries, xb for the gap
  dummies <- if (row$estimator == "fe_twoway") "+ factor(cntry) + factor(year)" else "+ factor(cntry)"
  m <- lm(as.formula(paste("y ~ x + xb", dummies)), data = d)
  ct <- summary(m)$coefficients
  cr2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = d$cntry, test = "Satterthwaite",
                                 coefs = "xb")
  X <- model.matrix(m)
  dif <- tibble(group = "difference", b = ct["xb", "Estimate"], se = ct["xb", "Std. Error"],
                t = ct["xb", "t value"], df = m$df.residual,
                r = ct["xb", "t value"] / sqrt(ct["xb", "t value"]^2 + m$df.residual),
                p_conv = ct["xb", "Pr(>|t|)"], t_cr2 = cr2$tstat, df_cr2 = cr2$df_Satt,
                p_cr2 = cr2$p_Satt,
                p_wcr = wcr_test(d$y, X, d$cntry, signs$rademacher, "xb")[["p"]],
                p_wcr_webb = wcr_test(d$y, X, d$cntry, signs$webb, "xb")[["p"]],
                n_cells = nrow(d), n_countries = n_distinct(d$cntry))
  bind_rows(groups, dif) |>
    mutate(outcome = row$outcome, predictor = row$predictor, estimator = row$estimator)
}) |>
  list_rbind() |>
  select(outcome, predictor, estimator, group, b, se, t, df, r, p_conv, t_cr2, df_cr2, p_cr2,
         p_wcr, p_wcr_webb, n_cells, n_countries)

## ---- (e) inference at the reference cell ---------------------------------------------

inference <- curve |>
  semi_join(reference, by = axis_codes) |>
  select(estimator, b, se, t, df, r, t_cr1, p_conv, p_cr1, t_cr2, df_cr2, p_cr2, p_wcr,
         p_wcr_webb, n_countries) |>
  pmap(\(estimator, b, se, t, df, r, t_cr1, p_conv, p_cr1, t_cr2, df_cr2, p_cr2, p_wcr,
         p_wcr_webb, n_countries) {
    # the vectors are built before the tibble, whose columns would otherwise
    # shadow the scalars of the same name
    se_v <- c(se, b / t_cr1, b / t_cr2, NA, NA)
    t_v  <- c(t, t_cr1, t_cr2, NA, NA)
    df_v <- c(df, n_countries - 1, df_cr2, NA, NA)
    p_v  <- c(p_conv, p_cr1, p_cr2, p_wcr, p_wcr_webb)
    tibble(estimator = estimator,
           inference = c("conventional", "cr1_g1", "cr2_satterthwaite", "wcr_rademacher",
                         "wcr_webb"),
           b = b, se = se_v, t = t_v, df = df_v, p = p_v, r_descriptive = r)
  }) |>
  list_rbind() |>
  mutate(r_formula = t / sqrt(t^2 + df))

walk2(list(between_within, country_means, periods, loo, bailout, inference),
      c("display_between_within.csv", "display_country_means.csv", "display_periods.csv",
        "display_loo.csv", "display_bailout.csv", "display_inference.csv"),
      \(x, f) datawizard::data_write(x, out_path(f), verbose = FALSE))

stopifnot("the periods table is not 36 specifications on three samples" = nrow(periods) == 108,
          "a country is missing from the leave-one-out table" = nrow(loo) == 56)

list(between_within, inference, bailout,
     loo |> filter(estimator == "fe_twoway") |> slice_max(variation_share, n = 4)) |>
  walk(\(x) print(as.data.frame(x |> mutate(across(where(is.double), \(v) signif(v, 4)))),
                  row.names = FALSE))
