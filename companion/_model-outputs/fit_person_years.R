###############################################################################
# Title:        Year effects in the published design, on the real respondents
# Purpose:      Fit the published specification of the EU-frames case without
#               its cross-level interactions, with respondents in country-years
#               in countries (the 270 cells of the panel as the middle level,
#               where the paper has its country-waves), the zero rule and
#               maximum likelihood, and the same specification with the
#               common pattern of the years added: once as a random intercept
#               for each year, the structure that Schmidt-Catran and
#               Fairbrother (2016) recommend for comparative longitudinal
#               survey data, and once as a dummy for each year. The
#               multilevel module and its deck set the unemployment slopes
#               beside the two-way fixed-effects slope of the panel.
# Reads:        the respondent file, through _load_respondents.R (licensed,
#               local only; EB_RESPONDENTS gives its path)
# Writes:       companion/_model-outputs/person_year_effects.csv
#               The slopes of unemployment and growth with their standard
#               errors, Satterthwaite degrees of freedom and t, the standard
#               deviations of the random intercepts and the respondent count:
#               aggregate, and committed.
# Author:       Chris Moreh
# Last updated: 2026-11-07
###############################################################################
source("companion/_model-outputs/_load_respondents.R")

# The reference categories of the paper (manual workers, large towns, the
# lowest education band), and age and the two national rates centred on their
# means over the respondents
person <- ind |>
  dplyr::mutate(
    edu3    = factor(edu3, levels = c("low", "mid", "high")),
    ses     = factor(ses, levels = c("manual", "self_employed", "manager", "white_collar",
                                     "house_person", "unemployed", "retired", "student")),
    urban   = factor(urban, levels = c("large_town", "rural", "small_town")),
    mage    = age - mean(age, na.rm = TRUE),
    munemp  = unemp - mean(unemp, na.rm = TRUE),
    mgrowth = growth - mean(growth, na.rm = TRUE)
  )

published_terms <- c("female", "mage", "edu3", "ses", "urban", "mgrowth", "munemp")
designs <- list(
  published   = c("(1 | cntry)", "(1 | cy)"),
  year_random = c("(1 | cntry)", "(1 | year)", "(1 | cy)"),
  year_dummy  = c("factor(year)", "(1 | cntry)", "(1 | cy)")
)

fits <- tidyr::expand_grid(outcome = c("cosmo", "lib"), design = names(designs)) |>
  purrr::pmap(\(outcome, design) {
    m <- lmerTest::lmer(reformulate(c(published_terms, designs[[design]]), response = outcome),
                        data = person, REML = FALSE,
                        control = lme4::lmerControl(calc.derivs = FALSE))
    fx <- summary(m)$coefficients[c("munemp", "mgrowth"), ]
    vc <- as.data.frame(lme4::VarCorr(m))  # r-style: keep – the VarCorr method lists the variance components as rows
    dplyr::bind_rows(
      tibble::tibble(outcome = outcome, design = design, kind = "fixed", term = rownames(fx),
                     estimate = fx[, "Estimate"], se = fx[, "Std. Error"],
                     df = fx[, "df"], t = fx[, "t value"]),
      tibble::tibble(outcome = outcome, design = design, kind = "sd", term = vc$grp,
                     estimate = vc$sdcor),
      tibble::tibble(outcome = outcome, design = design, kind = "n", term = "respondents",
                     estimate = stats::nobs(m))
    )
  }) |>
  purrr::list_rbind()

fits |>
  dplyr::mutate(dplyr::across(c("estimate", "se", "df", "t"), \(x) signif(x, 6))) |>
  readr::write_csv("companion/_model-outputs/person_year_effects.csv")
