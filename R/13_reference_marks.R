##############################################################################
# Title:        13_reference_marks.R
# Purpose:      The six reference results of the multiverse chart, each placed
#               on the degrees of freedom of its own inference. Where the
#               software of an analyst counted the units that carry the
#               evidence, the Multi100 record stands (KQXUE, C6HJR, KEVF1,
#               018OL). The two results reported without df are converted on
#               the Satterthwaite df of a refit of their own design on the
#               rebuilt respondents (maximum likelihood, as Stata's mixed):
#               the original study (Table 3, model 1: the cosmopolitan share
#               of respondents who mention an item, in country-waves in
#               countries, with sex, age, education, occupation, community
#               type, growth, unemployment and the interactions of the two
#               with education), and PRL47 (model B8: the same share, the wave
#               standing in for the quarter, unemployment of the 25-64
#               population, the individual controls of the analyst, growth and
#               a crisis indicator from 2009). The notes list what the files
#               cannot supply. r_233 converts each statistic on 233 df, a
#               device of the conversion lesson, not a placement.
#               p_cluster_check is, where the design can be fitted on the
#               cells, the p-value of the course refit of that design under
#               the rule of the curve. Only aggregate results are written.
# Reads:        _template/data_private/eb_respondents.rds through
#               companion/_model-outputs/_load_respondents.R (GESIS-derived,
#               local only), data/analysts5.csv, data/spec_curve.csv,
#               data/teney_panel_measurement.csv, data/teney_panel_weights.csv,
#               data/macro/predictor_measures.csv, data/EUframes_cy.csv
# Writes:       companion/_model-outputs/reference_refits.csv,
#               data/reference_marks.csv
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tidyr); library(lme4); library(lmerTest)
})
source("R/_curve_tools.R")
source("companion/_model-outputs/_load_respondents.R")

inputs <- curve_inputs()
signs  <- curve_signs()
curve  <- datawizard::data_read(out_path("spec_curve.csv"), verbose = FALSE)
an     <- datawizard::data_read("data/analysts5.csv", verbose = FALSE)
r_of   <- \(t, df) t / sqrt(t^2 + df)

## ---- the respondents of the two designs -------------------------------------------

lfs <- inputs$preds |> filter(measure == "es_2564_lfs") |> select(cntry, year, u_lfs = value)

resp <- ind |>
  filter(!is.na(edu3), !is.na(ses), !is.na(urban), !is.na(age), !is.na(female)) |>
  left_join(lfs, by = c("cntry", "year")) |>
  mutate(mage = age - 48, munemp = unemp - 8.76, mgrowth = growth - 1.41,
         age_cw = age - mean(age), .by = eb) |>
  mutate(crisis_d = as.numeric(year >= 2009),
         ses = factor(ses, levels = c("manual", "self_employed", "manager", "white_collar",
                                      "house_person", "unemployed", "retired", "student")))

oa_data  <- \(d) d |> mutate(edu3 = factor(edu3, levels = c("low", "mid", "high")),
                             urban = factor(urban, levels = c("large_town", "rural", "small_town")))
prl_data <- \(d) d |> mutate(edu3 = factor(edu3, levels = c("mid", "low", "high")),
                             urban = factor(urban, levels = c("small_town", "rural", "large_town")))

refits <- tribble(
  ~refit_id,                 ~design, ~rule,  ~formula,
  "OA_published",            "OA",    "drop", cosmo ~ female + mage + edu3 + ses + urban + mgrowth + munemp +
                                                  mgrowth:edu3 + munemp:edu3 + (1 | cntry) + (1 | cyw),
  "OA_zero_rule",            "OA",    "zero", cosmo ~ female + mage + edu3 + ses + urban + mgrowth + munemp +
                                                  mgrowth:edu3 + munemp:edu3 + (1 | cntry) + (1 | cyw),
  # without the interactions the slope is the average over the education bands,
  # and its df show how far the interactions lift those of the published model
  "OA_no_interactions",      "OA",    "drop", cosmo ~ female + mage + edu3 + ses + urban + mgrowth + munemp +
                                                  (1 | cntry) + (1 | cyw),
  "PRL47_B8_fixed_slopes",   "PRL47", "drop", cosmo ~ u_lfs + female + age_cw + edu3 + ses + urban + growth +
                                                  crisis_d + (1 | cntry) + (1 | cyw),
  "PRL47_individual_only",   "PRL47", "drop", cosmo ~ u_lfs + female + age_cw + edu3 + ses + urban +
                                                  (1 | cntry) + (1 | cyw),
  "PRL47_zero_rule",         "PRL47", "zero", cosmo ~ u_lfs + female + age_cw + edu3 + ses + urban + growth +
                                                  crisis_d + (1 | cntry) + (1 | cyw)
)

fit_refit <- function(refit_id, design, rule, formula) {
  d <- resp |> filter(rule == "zero" | k > 0)
  d <- if (design == "OA") oa_data(d) else prl_data(filter(d, !is.na(u_lfs)))
  term <- if (design == "OA") "munemp" else "u_lfs"
  m <- lmerTest::lmer(formula, data = d, REML = FALSE,
                      control = lmerControl(calc.derivs = FALSE))
  cf <- summary(m)$coefficients
  vc <- as.data.frame(VarCorr(m))
  tibble(refit_id, design, noop = rule, term,
         b = cf[term, "Estimate"], se = cf[term, "Std. Error"], t = cf[term, "t value"],
         df_satterthwaite = cf[term, "df"], p = cf[term, "Pr(>|t|)"],
         r_on_df = r_of(cf[term, "t value"], cf[term, "df"]),
         sd_country = vc$sdcor[vc$grp == "cntry"], sd_country_wave = vc$sdcor[vc$grp == "cyw"],
         sd_residual = vc$sdcor[vc$grp == "Residual"],
         respondents = nobs(m), country_waves = n_distinct(d$cyw), countries = n_distinct(d$cntry))
}

refit_res <- pmap(refits, fit_refit) |> list_rbind()
datawizard::data_write(refit_res, "companion/_model-outputs/reference_refits.csv", verbose = FALSE)

## ---- the cluster checks on the cells -------------------------------------------------

curve_p <- \(...) curve |> filter(...) |> pull(p_wcr)
std <- \(d) d |> filter(outcome == "mcosmo", predictor == "unemp", pred_form == "raw",
                        copredictor == "none", item_set == "all", cell_weight == "unweighted")
p_c6hjr <- curve |> filter(is_reference) |> pull(p_wcr)
p_kqxue <- curve |> std() |> filter(measure == "es_2064_pop", estimator == "fe_country",
                                    noop == "drop") |> pull(p_wcr)
p_kqxue_panel <- curve |> std() |> filter(measure == "wb_nat", estimator == "fe_country",
                                          noop == "zero") |> pull(p_wcr)

# KEVF1 on its own cells: the count of the five cosmopolitan items, the
# tipsun20 series without the Swedish cells of 2005-2008, which Eurostat added
# after KEVF1 merged the table; country fixed effects stand in for the random
# intercept of the country, since the bootstrap of the curve needs a linear fit
kevf1_row <- curve_menu("mcosmo") |>
  filter(predictor == "unemp", measure == "wb_nat", pred_form == "raw", copredictor == "none",
         noop == "zero", item_set == "all", cell_weight == "unweighted",
         estimator == "fe_country") |>
  mutate(measure = "es_1574_tips", construction = "count")
kevf1_cells <- spec_data(kevf1_row, inputs) |> filter(!(cntry == "SE" & year <= 2008))
kevf1_fit <- fit_linear(kevf1_row, inputs, signs, d = kevf1_cells)

## ---- the six marks ----------------------------------------------------------------------

oa  <- refit_res |> filter(refit_id == "OA_published")
prl <- refit_res |> filter(refit_id == "PRL47_B8_fixed_slopes")
rec <- \(id, col) an[[col]][an$analyst_id == id]
p_reported <- c(KQXUE = "0.000", C6HJR = "0.00015", KEVF1 = ".0000000277", PRL47 = "0.000",
                `018OL` = "0.228")

marks <- tribble(
  ~id,     ~label,                                   ~unit,                                             ~design,
  "OA",    "Original study (Table 3, model 1)",      "respondents (N = 335,566 in Table 3)",            "respondents within country-waves within countries, individual controls, GDP growth, education interactions",
  "KQXUE", "KQXUE",                                  "country-year cells (270)",                        "country fixed effects, cosmopolitan share, Eurostat unemployment of the 20-64 population",
  "C6HJR", "C6HJR",                                  "country-year cells (270)",                        "country and year fixed effects, cosmopolitan share, World Bank unemployment",
  "KEVF1", "KEVF1",                                  "respondents",                                     "random intercepts for years within countries, count of the five cosmopolitan items, Eurostat tipsun20",
  "PRL47", "PRL47 (model B8)",                       "respondents",                                     "respondents within country-quarters within countries, individual and macro controls, random slopes",
  "018OL", "018OL",                                  "country-wave cells (420)",                        "crossed random intercepts for country, year and wave, log of the mean of the five cosmopolitan items"
) |>
  mutate(
    unit = case_when(
      id == "KEVF1" ~ sprintf("respondents (%s)", format(rec("KEVF1", "n"), big.mark = ",")),
      id == "PRL47" ~ sprintf("respondents (%s)", format(rec("PRL47", "n"), big.mark = ",")),
      .default = unit),
    statistic_type = c("t", rec("KQXUE", "statistic_type"), rec("C6HJR", "statistic_type"),
                       rec("KEVF1", "statistic_type"), rec("PRL47", "statistic_type"),
                       rec("018OL", "statistic_type")),
    statistic_recorded = c(-4.03, rec("KQXUE", "statistic"), rec("C6HJR", "statistic"),
                           rec("KEVF1", "statistic"), rec("PRL47", "statistic"),
                           rec("018OL", "statistic")),
    df_recorded = c(NA, rec("KQXUE", "df1"), rec("C6HJR", "df1"), rec("KEVF1", "df1"),
                    rec("PRL47", "df1"), rec("018OL", "df1")),
    r_recorded = c(NA, rec("KQXUE", "r"), rec("C6HJR", "r"), rec("KEVF1", "r"), rec("PRL47", "r"),
                   rec("018OL", "r")),
    df_placement = c(oa$df_satterthwaite, df_recorded[2:4], prl$df_satterthwaite, df_recorded[6]),
    df_basis = c("Satterthwaite df of the course refit of the design",
                 "residual df as recorded", "residual df as recorded",
                 "Satterthwaite df as recorded", "Satterthwaite df of the course refit of the design",
                 "Satterthwaite df as recorded"),
    r_placement = r_of(statistic_recorded, df_placement),
    r_233 = r_of(statistic_recorded, 233),
    b_natural = c(-0.00340, NA, curve$b[curve$is_reference], -0.0295, NA, NA),
    se_natural = c(0.00340 / 4.03, NA, curve$se[curve$is_reference], 0.0295 / 5.899, NA, NA),
    b_unit = c("cosmopolitan share (0-1) per point of unemployment, low-education band",
               NA, "cosmopolitan share (0-1) per point of unemployment",
               "count of five items (0-5) per point of unemployment", NA, NA),
    p_recorded = c("< 0.001", p_reported[c("KQXUE", "C6HJR", "KEVF1", "PRL47", "018OL")]),
    p_cluster_check = c(NA, p_kqxue, p_c6hjr, kevf1_fit$p_wcr, NA, NA),
    note = c(
      sprintf("The paper prints b = -0.00340 and t = -4.03 with no df; Multi100 recorded no effect size for the original study. Placed on the %.1f Satterthwaite df of the course refit (b = %.5f, t = %.2f on %s respondents); on the respondent count the same t would give r = %.4f. se_natural is b / t.",
              oa$df_satterthwaite, oa$b, oa$t, format(oa$respondents, big.mark = ","), r_of(-4.03, 335566)),
      sprintf("Recorded t uses conventional errors. The course refit on the curve (own series es_2064_pop, drop rule, country fixed effects) has p_wcr = %.4f; at the standard measurement settings p_wcr = %.4f. No slope is in the Multi100 record.",
              p_kqxue, p_kqxue_panel),
      "Recorded t = -3.804 is the corrected build; the form reported -3.853. b and se are those of the reproduced specification, whose bootstrap p is p_cluster_check.",
      sprintf("The recorded value is a cell-level quantity. p_cluster_check is the country fixed-effects analogue on the %d cells of the count that the series covers, in %d countries, b = %.4f, r = %.3f, with the bootstrap of the curve.",
              kevf1_fit$n_cells, kevf1_fit$n_countries, kevf1_fit$b, kevf1_fit$r),
      sprintf("Stata reported z = -3.594 without df; Multi100 converted on N. Placed on the %.1f Satterthwaite df of the course refit (b = %.5f, t = %.2f on %s respondents), which uses annual series and fixed slopes, without GDP per capita, the change in unemployment and the Candidate Countries wave.",
              prl$df_satterthwaite, prl$b, prl$t, format(prl$respondents, big.mark = ",")),
      "The estimate was reported as -0.00 on the log scale; the analyst ticked 'opposite direction' for a negative estimate. Country-wave cells share annual unemployment values, so the 396.9 df overstate the independent comparisons."
    )
  ) |>
  select(id, label, unit, design, statistic_type, statistic_recorded, df_recorded, r_recorded,
         df_placement, df_basis, r_placement, r_233, b_natural, se_natural, b_unit, p_recorded,
         p_cluster_check, note)

datawizard::data_write(marks, out_path("reference_marks.csv"), verbose = FALSE)

print(as.data.frame(refit_res |> mutate(across(where(is.double), \(x) signif(x, 4)))), row.names = FALSE)
print(as.data.frame(marks |> select(id, statistic_recorded, df_placement, r_placement, r_233,
                                    p_cluster_check)), row.names = FALSE)
