################################################################################
# Title:        Multilevel fits of the immigration-and-trust case
# Purpose:      The respondent-level models that the statistics module and the
#               individual-level module discuss, fitted once on the ESS extract
#               of the course, because lme4 needs from seconds to minutes for
#               each of them on 70,000 respondents: the empty model of three
#               levels (the variance at each level, against Table 2 of the
#               paper), the model of the paper with its country-level variables
#               (the degrees of freedom of each coefficient by the level of its
#               predictor), random intercepts for the countries alone and for
#               countries and country-rounds, a random slope of concern by
#               country, the within-between model in its two forms (a slope
#               within and a slope between countries, or the slope within and
#               the contextual effect), and the random-effects fit of plm with
#               its quasi-demeaning weight for each country.
# Reads:        cases/trust/data/trust_ess.csv, cases/trust/data/trust_macro.csv,
#               R/trust/_tools.R (the analysed rows of the reference specification)
# Writes:       companion/_model-outputs/trust_multilevel.csv
# Author:       Claude for CM
# Last updated: 2026-10-27
################################################################################

source("R/trust/_tools.R")

ess <- readr::read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)
macro <- readr::read_csv("cases/trust/data/trust_macro.csv", show_col_types = FALSE) |>
  datawizard::data_select(select = c("cntry", "essround", "far_right_share", "social_protection",
                                     "wgi", "gdp_index", "unemployment", "long_term_imm"))

# The respondents of the reference specification of the curve: distrust of
# parliament, the controls of the paper, every item answered (70,326 in 57
# country-rounds). y is distrust, x concern about immigration, cr the
# country-round. The country means of concern and the departures from them
# carry the suffixes _between and _within (datawizard::demean()).
ref_frame <- spec_frame(ess) |>
  dplyr::left_join(macro, by = c("cntry", "essround")) |>
  datawizard::demean(select = "x", by = "cntry")

controls <- CONTROL_TERMS$paper
country_round_vars <- c("far_right_share", "social_protection")
country_vars <- c("long_term_imm", "wgi", "gdp_index", "unemployment")

# The empty model of Table 2 of the paper: every respondent of the paper who
# answered the item on parliament, with no predictor at all
empty_frame <- ess |>
  datawizard::data_filter(citizen == 1, minority == 0, !is.na(distrust_parl)) |>
  datawizard::data_modify(cr = paste0(cntry, essround))

specs <- list(
  empty = list(data = empty_frame,
               formula = distrust_parl ~ 1 + (1 | cntry) + (1 | cr)),
  paper = list(data = ref_frame,
               formula = stats::reformulate(c("x", controls, country_round_vars, "factor(essround)",
                                              country_vars, "(1 | cntry)", "(1 | cr)"), response = "y")),
  ri_country = list(data = ref_frame,
                    formula = stats::reformulate(c("x", controls, "factor(essround)", "(1 | cntry)"),
                                                 response = "y")),
  ri_country_round = list(data = ref_frame,
                          formula = stats::reformulate(c("x", controls, "factor(essround)", "(1 | cntry)",
                                                         "(1 | cr)"), response = "y")),
  random_slope = list(data = ref_frame,
                      formula = stats::reformulate(c("x", controls, "factor(essround)", "(1 + x | cntry)",
                                                     "(1 | cr)"), response = "y")),
  within_between = list(data = ref_frame,
                        formula = stats::reformulate(c("x_within", "x_between", controls, "factor(essround)",
                                                       "(1 | cntry)", "(1 | cr)"), response = "y")),
  # the same model with concern as answered beside its country mean, so that
  # the coefficient of the mean is the contextual effect, the between slope
  # minus the within slope, with its own test (Mundlak 1978, Econometrica)
  contextual = list(data = ref_frame,
                    formula = stats::reformulate(c("x", "x_between", controls, "factor(essround)",
                                                   "(1 | cntry)", "(1 | cr)"), response = "y"))
)

# lmerTest fits by REML; parameters reads every fixed coefficient with its t on
# Satterthwaite df (Kuznetsova, Brockhoff and Christensen 2017, Journal of
# Statistical Software) and every variance component as a standard deviation.
# lme4 results are kept to six significant digits, the precision that is
# stable from one run to the next. purrr::exec() writes the formula and the
# data into the stored call, which lmerTest evaluates again to build the
# deviance function behind the Satterthwaite df; a call that names s$data fails.
fits <- purrr::map(specs, \(s) purrr::exec(lmerTest::lmer, formula = s$formula, data = s$data))

lmer_rows <- purrr::imap(fits, \(fit, label) {
  groups <- lme4::ngrps(fit)
  parameters::model_parameters(fit, effects = "all", ci_method = "satterthwaite") |>
    as.data.frame() |>  # r-style: keep – drops the print method of parameters before the columns are chosen
    dplyr::transmute(model = label, kind = dplyr::if_else(Effects == "fixed", "fixed", "random"),
                     term = dplyr::if_else(Effects == "fixed", Parameter, paste0(Group, ": ", Parameter)),
                     estimate = signif(Coefficient, 6), se = signif(SE, 6), t = signif(t, 6),
                     df = signif(df_error, 6), p = signif(p, 6),
                     n = stats::nobs(fit), countries = groups[["cntry"]],
                     country_rounds = if ("cr" %in% names(groups)) groups[["cr"]] else NA_integer_)
}) |>
  purrr::list_rbind()

# The random-effects estimator of the statistics module (generalised least
# squares) with countries as the groups and
# the respondents of a country as its observations, an unbalanced panel in the
# terms of plm (Croissant and Millo 2008, Journal of Statistical Software); the
# row number within a country stands in for the time index, which a model with
# individual effects alone does not use. The variance components are those of
# Wallace and Hussain (1969, Econometrica), from the residuals of pooled OLS:
# the default of plm (Swamy and Arora) needs a regression on the group means
# with more groups than coefficients, and 16 countries are fewer than the 20
# coefficients. theta, the share of the country mean that the estimator
# subtracts, differs by country with its number of respondents.
plm_frame <- ref_frame |>
  dplyr::mutate(row_in_country = dplyr::row_number(), .by = cntry)
plm_fit <- plm::plm(stats::reformulate(c("x", controls, "factor(essround)"), response = "y"),
                    data = plm_frame, index = c("cntry", "row_in_country"),
                    model = "random", effect = "individual", random.method = "walhus")
plm_theta <- plm::ercomp(plm_fit)$theta
# parameters reads the GLS coefficient with a z statistic, written to the t column
plm_coef <- parameters::model_parameters(plm_fit, keep = "^x$")
plm_rows <- tibble::tibble(
  model = "re_plm", kind = c("fixed", "theta", "theta"), term = c("x", "theta_min", "theta_max"),
  estimate = signif(c(plm_coef$Coefficient, min(plm_theta), max(plm_theta)), 6),
  se = c(signif(plm_coef$SE, 6), NA, NA), t = c(signif(plm_coef$z, 6), NA, NA),
  df = c(signif(plm_coef$df_error, 6), NA, NA), p = c(signif(plm_coef$p, 6), NA, NA),
  n = stats::nobs(plm_fit), countries = dplyr::n_distinct(plm_frame$cntry), country_rounds = NA_integer_
)

trust_multilevel <- dplyr::bind_rows(lmer_rows, plm_rows)

# The model of the paper here is the refit that display_reproduction.csv holds
# as course_repro_60 (0.175341), and the random intercepts for countries and
# country-rounds are the off-curve option of display_options.csv (0.17534)
reproduction <- readr::read_csv("cases/trust/data/display_reproduction.csv", show_col_types = FALSE)
options_d <- readr::read_csv("cases/trust/data/display_options.csv", show_col_types = FALSE)
paper_x <- trust_multilevel$estimate[trust_multilevel$model == "paper" & trust_multilevel$term == "x"]
ri_x <- trust_multilevel$estimate[trust_multilevel$model == "ri_country_round" & trust_multilevel$term == "x"]
stopifnot(abs(paper_x - reproduction$b[reproduction$id == "course_repro_60"]) < 1e-5,
          abs(ri_x - options_d$b[options_d$option == "random_intercepts"]) < 1e-5)

# readr writes the bytes of the committed files; datawizard::data_write() does not
readr::write_csv(trust_multilevel, "companion/_model-outputs/trust_multilevel.csv", na = "")
