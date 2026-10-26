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
#               model, each tested by CR2 with Satterthwaite df, which adapt
#               to the eight clusters of the first group. (e) The reference cell and its
#               country twin under conventional, CR1 (G - 1 df) and CR2
#               inference, with the r that the conversion formula returns from
#               each t and df beside the descriptive r, which does not change.
# Reads:        cases/eu-frames/data/EUframes_cy.csv,
#               cases/eu-frames/data/teney_panel_measurement.csv,
#               cases/eu-frames/data/teney_panel_weights.csv,
#               cases/eu-frames/data/macro/predictor_measures.csv,
#               cases/eu-frames/data/spec_curve.csv
# Writes:       cases/eu-frames/data/display_between_within.csv,
#               cases/eu-frames/data/display_country_means.csv,
#               cases/eu-frames/data/display_periods.csv,
#               cases/eu-frames/data/display_loo.csv,
#               cases/eu-frames/data/display_bailout.csv,
#               cases/eu-frames/data/display_inference.csv
# Author:       Chris Moreh
# Last updated: 2026-10-26
##############################################################################

source("R/eu-frames/_curve_tools.R")

inputs <- curve_inputs()
panel  <- datawizard::data_read("cases/eu-frames/data/EUframes_cy.csv", verbose = FALSE)
curve  <- datawizard::data_read(out_path("spec_curve.csv"), verbose = FALSE)

standard <- curve_menu(positive_scales) |>
  datawizard::data_filter(noop == "zero" & item_set == "all" & cell_weight == "unweighted" &
                            measure %in% c("wb_nat", "wb_growth"))
reference <- standard |>
  datawizard::data_filter(outcome == "mcosmo" & predictor == "unemp" & pred_form == "raw" &
                            copredictor == "none")

with_class <- \(d) d |>
  dplyr::mutate(r_ca = r * align_factor(outcome, predictor), supports = r_ca < 0, sig = p_cr2 < 0.05)
fit_rows <- \(rows) purrr::map(seq_len(nrow(rows)), \(i) fit_linear(rows[i, ], inputs)) |>
  purrr::list_rbind() |>
  dplyr::bind_cols(rows) |>
  with_class()

## ---- (a) within and between ------------------------------------------------------

bw_rows <- standard |>
  datawizard::data_filter(pred_form == "raw" & copredictor == "none" & estimator == "fe_country")

between_within <- purrr::map(seq_len(nrow(bw_rows)), \(i) {
  row <- bw_rows[i, ]
  # the within-between split of Bell and Jones (2015): x_within is the deviation
  # from the country mean, x_between the country mean
  d <- spec_data(row, inputs) |>
    datawizard::demean(select = "x", by = "cntry", verbose = FALSE)
  within <- fit_linear(row, inputs, d = d)
  # dplyr::summarise(), because a column named x would be taken for the first
  # argument of datawizard::data_summary()
  means <- dplyr::summarise(d, y = mean(y), x = mean(x), .by = cntry)
  bm <- summary(stats::lm(y ~ x, data = means))$coefficients
  m <- lmerTest::lmer(y ~ x_within + x_between + (1 | cntry), data = d, REML = TRUE)
  cf <- summary(m)$coefficients
  dif <- lmerTest::contest(m, L = as.numeric(rownames(cf) == "x_within") -
                             as.numeric(rownames(cf) == "x_between"), joint = FALSE)
  tibble::tibble(outcome = row$outcome, predictor = row$predictor, measure = row$measure,
                 b_within = within$b, se_within = within$se, r_within = within$r,
                 p_within_cr2 = within$p_cr2,
                 b_between = bm["x", "Estimate"], se_between = bm["x", "Std. Error"],
                 t_between = bm["x", "t value"], df_between = nrow(means) - 2,
                 p_between = bm["x", "Pr(>|t|)"],
                 r_between = stats::cor(means$x, means$y),
                 b_rewb_within = cf["x_within", "Estimate"], t_rewb_within = cf["x_within", "t value"],
                 df_rewb_within = cf["x_within", "df"], p_rewb_within = cf["x_within", "Pr(>|t|)"],
                 b_rewb_between = cf["x_between", "Estimate"],
                 t_rewb_between = cf["x_between", "t value"],
                 df_rewb_between = cf["x_between", "df"], p_rewb_between = cf["x_between", "Pr(>|t|)"],
                 b_difference = dif$Estimate, t_difference = dif[["t value"]], df_difference = dif$df,
                 p_difference = dif[["Pr(>|t|)"]], n_cells = nrow(d), n_countries = nrow(means))
}) |>
  purrr::list_rbind()

scale_cols <- c("unemp", "growth", "mcosmo", "mutil", "mpos", "mcomm", "mlib", "mneg")
country_means <- panel |>
  dplyr::summarise(country = dplyr::first(country), n_years = dplyr::n(),
                   bailout = as.integer(any(bailout == 1)),
                   dplyr::across(dplyr::all_of(scale_cols), mean), .by = cntry) |>
  dplyr::rename_with(\(n) paste0(n, "_mean"), dplyr::all_of(scale_cols)) |>
  # r-style: keep – dplyr::arrange() sorts in the C locale on every machine
  dplyr::arrange(cntry)

## ---- (b) periods ---------------------------------------------------------------------

periods <- c("all", "y2004_2008", "y2009_2013") |>
  purrr::map(\(s) fit_rows(dplyr::mutate(standard, sample = s))) |>
  purrr::list_rbind() |>
  datawizard::data_select(select = c(axis_codes, "b", "se", "t", "df", "r", "r_ca", "p_conv",
                                     "t_cr1", "p_cr1", "t_cr2", "df_cr2", "p_cr2", "supports",
                                     "sig", "n_cells", "n_countries"))

## ---- (c) leave one country out -------------------------------------------------------

loo_rows <- dplyr::bind_rows(datawizard::data_filter(reference, estimator == "fe_twoway"),
                             datawizard::data_filter(reference, estimator == "fe_country"))

# the share of the variation in the predictor, net of the fixed effects of the
# design, that each country holds
variation_share <- function(row) {
  d <- spec_data(row, inputs)
  fe <- if (row$estimator == "fe_twoway") "x ~ 1 | cntry + year" else "x ~ 1 | cntry"
  xt <- stats::resid(fixest::feols(stats::as.formula(fe), data = d))
  tibble::tibble(left_out = d$cntry, xt2 = xt^2) |>
    dplyr::summarise(v = sum(xt2), .by = left_out) |>
    dplyr::transmute(left_out, variation_share = 100 * v / sum(v))
}

loo <- purrr::map(seq_len(nrow(loo_rows)), \(i) {
  row <- loo_rows[i, ]
  d_all <- spec_data(row, inputs)
  # the 27 member states in the order of their codes, as the cells are sorted
  fits <- purrr::map(c("none", unique(d_all$cntry)), \(cc) {
    fit_linear(row, inputs, d = dplyr::filter(d_all, cntry != cc)) |> dplyr::mutate(left_out = cc)
  }) |>
    purrr::list_rbind()
  fits |>
    dplyr::left_join(variation_share(row), by = "left_out") |>
    dplyr::mutate(estimator = row$estimator, outcome = row$outcome, predictor = row$predictor,
                  r_ca = r * align_factor(outcome, predictor))
}) |>
  purrr::list_rbind() |>
  datawizard::data_select(select = c("estimator", "left_out", "outcome", "predictor", "b", "se",
                                     "t", "df", "r", "r_ca", "p_conv", "p_cr2", "n_cells",
                                     "n_countries", "variation_share"))

## ---- (d) bailout countries ---------------------------------------------------------

bail_rows <- datawizard::data_filter(standard, pred_form == "raw" & copredictor == "none")

bailout <- purrr::map(seq_len(nrow(bail_rows)), \(i) {
  row <- bail_rows[i, ]
  d <- spec_data(row, inputs) |>
    dplyr::mutate(bail = as.numeric(cntry %in% inputs$bailout), xb = x * bail)
  groups <- list(bailout = datawizard::data_filter(d, bail == 1),
                 other = datawizard::data_filter(d, bail == 0)) |>
    purrr::imap(\(dd, g) fit_linear(row, inputs, d = dd) |> dplyr::mutate(group = g)) |>
    purrr::list_rbind()
  # the difference in one model: x for the other countries, xb for the gap
  dummies <- if (row$estimator == "fe_twoway") "+ factor(cntry) + factor(year)" else "+ factor(cntry)"
  m <- stats::lm(stats::as.formula(paste("y ~ x + xb", dummies)), data = d)
  ct <- summary(m)$coefficients["xb", ]
  cr2 <- clubSandwich::coef_test(m, vcov = "CR2", cluster = d$cntry, test = "Satterthwaite",
                                 coefs = "xb")
  dif <- tibble::tibble(group = "difference", b = ct[["Estimate"]], se = ct[["Std. Error"]],
                        t = ct[["t value"]], df = stats::df.residual(m),
                        r = effectsize::t_to_r(ct[["t value"]], stats::df.residual(m))$r,
                        p_conv = ct[["Pr(>|t|)"]], t_cr2 = cr2$tstat, df_cr2 = cr2$df_Satt,
                        p_cr2 = cr2$p_Satt, n_cells = nrow(d), n_countries = dplyr::n_distinct(d$cntry))
  dplyr::bind_rows(groups, dif) |>
    dplyr::mutate(outcome = row$outcome, predictor = row$predictor, estimator = row$estimator)
}) |>
  purrr::list_rbind() |>
  datawizard::data_select(select = c("outcome", "predictor", "estimator", "group", "b", "se", "t",
                                     "df", "r", "p_conv", "t_cr2", "df_cr2", "p_cr2", "n_cells",
                                     "n_countries"))

## ---- (e) inference at the reference cell ---------------------------------------------

inference <- curve |>
  dplyr::semi_join(reference, by = axis_codes) |>
  datawizard::data_select(select = c("estimator", "b", "se", "t", "df", "r", "t_cr1", "p_conv",
                                     "p_cr1", "t_cr2", "df_cr2", "p_cr2", "n_countries")) |>
  purrr::pmap(\(estimator, b, se, t, df, r, t_cr1, p_conv, p_cr1, t_cr2, df_cr2, p_cr2,
                n_countries) {
    # the vectors are built before the tibble, whose columns would otherwise
    # shadow the scalars of the same name; a clustered standard error is the
    # estimate over its t
    se_v <- c(se, b / t_cr1, b / t_cr2)
    t_v  <- c(t, t_cr1, t_cr2)
    df_v <- c(df, n_countries - 1, df_cr2)
    p_v  <- c(p_conv, p_cr1, p_cr2)
    tibble::tibble(estimator = estimator,
                   inference = c("conventional", "cr1_g1", "cr2_satterthwaite"),
                   b = b, se = se_v, t = t_v, df = df_v, p = p_v, r_descriptive = r)
  }) |>
  purrr::list_rbind() |>
  dplyr::mutate(r_formula = effectsize::t_to_r(t, df)$r)

purrr::walk2(list(between_within, country_means, periods, loo, bailout, inference),
             c("display_between_within.csv", "display_country_means.csv", "display_periods.csv",
               "display_loo.csv", "display_bailout.csv", "display_inference.csv"),
             \(x, f) datawizard::data_write(x, out_path(f), verbose = FALSE))

stopifnot("the periods table is not 36 specifications on three samples" = nrow(periods) == 108,
          "a country is missing from the leave-one-out table" = nrow(loo) == 56)

list(between_within, inference, bailout,
     loo |> datawizard::data_filter(estimator == "fe_twoway") |>
       dplyr::slice_max(variation_share, n = 4)) |>
  purrr::walk(\(x) print(as.data.frame(dplyr::mutate(x, dplyr::across(dplyr::where(is.double),
                                                                       \(v) signif(v, 4)))),
                         row.names = FALSE))
