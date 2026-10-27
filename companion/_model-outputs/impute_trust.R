################################################################################
# Title:        Multiple imputation of the immigration-and-trust case
# Purpose:      The off-curve option "imputation" of the decision on missing
#               answers, which the missing-answers module teaches: the missing
#               answers of the respondents of the paper filled in ten times by
#               mice (predictive mean matching, van Buuren and
#               Groothuis-Oudshoorn 2011, Journal of Statistical Software), the
#               reference specification of the curve fitted on each completed
#               file with errors clustered by country, and the ten results
#               pooled by Rubin's rules with the degrees of freedom of Barnard
#               and Rubin (1999, Biometrika) on 15 complete-data df, the df of
#               the clustered test.
# Reads:        cases/trust/data/trust_ess.csv, R/trust/_tools.R (the analysed
#               rows of a specification and the controls of the paper)
# Writes:       companion/_model-outputs/trust_imputation.csv
# Author:       Claude for CM
# Last updated: 2026-10-27
################################################################################

source("R/trust/_tools.R")

ess <- readr::read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)

# The respondents of the paper who answered the item on parliament. The outcome
# is not imputed; everything that the reference specification uses besides it
# is, from everything else in the file and from the country and the round.
imputed_vars <- c("imm_econ", "imm_culture", "imm_place", "unhappy", "life_dissat", "meet_rarely",
                  "distrust_people", "unfair_people", "unhelpful_people", "econ_dissat",
                  "income_difficulty", "health_dissat", "educ_dissat", "left_right",
                  "hh_income_z", "age", "female", "education")
base <- ess |>
  datawizard::data_filter(citizen == 1, minority == 0, !is.na(distrust_parl))
to_impute <- base |>
  datawizard::data_select(select = c(imputed_vars, "distrust_parl", "winner", "far_right_vote")) |>
  dplyr::mutate(cntry = factor(base$cntry), essround = factor(base$essround))

# Ten imputations, ten iterations each. Household income is missing for every
# respondent of France and Ireland in round 1, and the feeling about household
# income for every respondent of France in rounds 1 and 2, so their values
# there come wholly from the other country-rounds through the imputation model.
imp <- mice::mice(to_impute, m = 10, maxit = 10, method = "pmm", seed = 20261027, printFlag = FALSE)

# Each completed file goes back into the columns that the estimator reads; the
# rows of mice::complete() keep the order of the data that were imputed
fits <- purrr::map(seq_len(imp$m), \(i) {
  completed <- base |>
    datawizard::data_select(exclude = imputed_vars) |>
    dplyr::bind_cols(mice::complete(imp, i) |> datawizard::data_select(select = imputed_vars))
  fixest::feols(stats::as.formula(paste("y ~", paste(c("x", CONTROL_TERMS$paper), collapse = " + "), "| cr")),
                data = spec_frame(completed), cluster = ~cntry)
})

pooled <- mice::pool(mice::as.mira(fits), dfcom = 15) |>
  summary(conf.int = TRUE) |>
  datawizard::data_filter(term == "x")
fmi <- mice::pool(mice::as.mira(fits), dfcom = 15)$pooled |>
  datawizard::data_filter(term == "x")

each <- tibble::tibble(
  row = paste0("imputation_", seq_along(fits)),
  b = purrr::map_dbl(fits, \(f) stats::coef(f)[["x"]]),
  se = purrr::map_dbl(fits, \(f) sqrt(stats::vcov(f)["x", "x"])),
  n = purrr::map_int(fits, stats::nobs)
)

trust_imputation <- dplyr::bind_rows(
  each,
  tibble::tibble(row = "pooled", b = pooled$estimate, se = pooled$std.error, t = pooled$statistic,
                 df = pooled$df, p = pooled$p.value, ci_low = pooled$`2.5 %`, ci_high = pooled$`97.5 %`,
                 fmi = fmi$fmi, n = each$n[1])
) |>
  dplyr::mutate(m = imp$m, iterations = imp$iteration, respondents = nrow(base),
                dplyr::across(dplyr::where(is.double), \(x) signif(x, 6)))

# readr writes the bytes of the committed files; datawizard::data_write() does not
readr::write_csv(trust_imputation, "companion/_model-outputs/trust_imputation.csv", na = "")
