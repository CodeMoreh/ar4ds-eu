###############################################################################
# Title:        OA's published model, main effects only, at person level
# Purpose:      Fit the specification the simulated twin has to reproduce, and
#               write out the parameters the synthesis needs. The published
#               models carry cross-level interactions between the macro
#               predictors and education; a synthesis that preserves within-cell
#               covariance and cell means cannot reproduce those, so the target
#               here is the same specification with the interactions dropped.
#               That is the model the twin reproduces and the one its numbers
#               may be quoted from.
#
#               Also measures the compositional gradient: how much a cell's
#               share of unemployed respondents rises with its published
#               unemployment rate. The simulation module has had to assume that
#               quantity; here it is estimated.
#
# Reads:        the respondent file, through _load_respondents.R (licensed, local only)
# Writes:       companion/_model-outputs/anchor_person_fixed.csv
#               companion/_model-outputs/anchor_person_random.csv
#               companion/_model-outputs/prevalence_gradient.csv
#               All three are coefficient tables - aggregate summaries of the
#               kind a published appendix carries. No respondent record leaves
#               this script.
# Author:       Chris Moreh
# Last updated: 2026-07-30
###############################################################################
source("companion/_model-outputs/_load_respondents.R")

# Reference categories are the published ones: manual workers for occupation,
# large town for urbanisation, lowest band for education. Getting these wrong
# does not change the macro slopes but makes every contrast unreadable against
# the paper.
ind <- ind |>
  datawizard::data_modify(cyw   = paste(cntry, eb, sep = "_"),
         edu3  = factor(edu3, levels = c("low", "mid", "high")),
         ses   = factor(ses, levels = c("manual", "self_employed", "manager",
                                        "white_collar", "house_person",
                                        "unemployed", "retired", "student")),
         urban = factor(urban, levels = c("large_town", "rural", "small_town")),
         mage  = age - mean(age, na.rm = TRUE),
         munemp  = unemp  - mean(unemp,  na.rm = TRUE),
         mgrowth = growth - mean(growth, na.rm = TRUE))

fit_one <- function(y) {
  f <- reformulate(c("female", "mage", "edu3", "ses", "urban",
                     "mgrowth", "munemp", "(1 | cntry)", "(1 | cyw)"),
                   response = y)
  m <- lme4::lmer(f, data = ind, REML = FALSE,
            control = lme4::lmerControl(calc.derivs = FALSE))
  fx <- summary(m)$coefficients
  fixed <- tibble::tibble(outcome = y, term = rownames(fx),
                  estimate = fx[, "Estimate"], se = fx[, "Std. Error"],
                  t = fx[, "t value"])
  vc <- as.data.frame(lme4::VarCorr(m))  # r-style: keep – the VarCorr method lists the variance components as rows
  rand <- tibble::tibble(outcome = y, component = c(vc$grp, "residual_sd"),
                 sd = c(vc$sdcor, sigma(m)))
  list(fixed = fixed, random = rand[!duplicated(rand$component), ])
}

res <- purrr::map(c("cosmo", "util", "comm", "lib"), fit_one)

fixed <- purrr::map(res, "fixed") |> purrr::list_rbind()
random <- purrr::map(res, "random") |> purrr::list_rbind()

readr::write_csv(fixed  |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 6))),
          "companion/_model-outputs/anchor_person_fixed.csv")
readr::write_csv(random |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 6))),
          "companion/_model-outputs/anchor_person_random.csv")

# The macro slopes of the main-effects specification, and the unemployed
# against manual workers
fixed |>
  datawizard::data_filter(term %in% c("munemp", "mgrowth", "sesunemployed")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 4))) |>
  print()

# --- the compositional gradient --------------------------------------------
# How much a country-year's share of unemployed respondents rises with its
# published unemployment rate. This is the quantity the module's
# composition-versus-context arithmetic has had to assume.
cells <- ind |>
  dplyr::summarise(share = mean(unemployed, na.rm = TRUE),
            unemp = dplyr::first(unemp), .by = c(cntry, year))

grad_pooled <- lm(share ~ unemp, data = cells)
grad_within <- lm(share ~ unemp + factor(cntry) + factor(year), data = cells)

gradient <- tibble::tibble(
  specification = c("pooled across country-years",
                    "within country and year (two-way fixed effects)"),
  gradient = c(coef(grad_pooled)[["unemp"]], coef(grad_within)[["unemp"]]),
  se = c(sqrt(diag(vcov(grad_pooled)))[["unemp"]],
         sqrt(diag(vcov(grad_within)))[["unemp"]]),
  mean_share = mean(cells$share),
  mean_rate  = mean(cells$unemp))

readr::write_csv(gradient |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 6))),
          "companion/_model-outputs/prevalence_gradient.csv")

# The compositional gradient (unemployed share per point of the rate), and the
# ratio of respondent unemployment to the published rate
gradient |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 4))) |>
  print()
print(signif(mean(cells$share) * 100 / mean(cells$unemp), 3))
