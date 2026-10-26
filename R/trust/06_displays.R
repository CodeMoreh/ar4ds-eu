################################################################################
# Title:        06_displays.R
# Purpose:      The teaching displays beside the curve: the options off the curve
#               fitted once at the reference settings, the inference of the
#               reference specification under several rules, the estimate with
#               each country left out, the reliability of the two scales, and the
#               country-round means behind the between-country picture.
# Reads:        cases/trust/data/trust_ess.csv
# Writes:       cases/trust/data/display_options.csv, display_inference.csv,
#               display_loo.csv, display_reliability.csv, display_country_means.csv
#               (all in cases/trust/data/)
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/trust/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" rather than NA
ess <- readr::read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)
reference <- purrr::exec(fit_spec, ess, !!!REFERENCE)
# the reference specification with one or more of its settings changed
fit_at <- \(...) purrr::exec(fit_spec, ess, !!!utils::modifyList(REFERENCE, list(...)))

# Options off the curve that a linear model can fit, each changed alone from
# the reference specification
option_rows <- tibble::tribble(
  ~axis,      ~option,            ~args,
  "sample",   "all_respondents",  list(sample = "all_respondents"),
  "concern",  "economy",          list(concern = "economy"),
  "concern",  "culture",          list(concern = "culture"),
  "concern",  "place",            list(concern = "place"),
  "controls", "paper_gov",        list(extra = "gov_dissat"),
  "controls", "paper_no_unhappy", list(controls = "paper_no_unhappy"),
  "weights",  "post",             list(weights = "post"),
  "weights",  "post_population",  list(weights = "post_population"),
  "fe",       "pooled",           list(fe = "pooled")
)
# fitted outside dplyr::mutate(), which would read the !!! of purrr::exec() as its own
option_fits <- purrr::map(option_rows$args, \(a) purrr::exec(fit_at, !!!a))
option_rows <- option_rows |>
  datawizard::data_select(exclude = "args") |>
  dplyr::mutate(fit = option_fits) |>
  tidyr::unnest(fit, names_sep = "_")

# Voters only: the vote controls of the paper (cabinet, far right) dropped to
# missing for everyone who did not vote, as the first attempt of the SCORE
# source-data reproduction 65wm6 coded them
voters_only <- purrr::exec(fit_spec, datawizard::data_filter(ess, voted %in% 1), !!!REFERENCE) |>
  datawizard::data_addprefix("fit_") |>
  dplyr::mutate(axis = "votes", option = "nonvoters_missing", .before = 1)

# Random intercepts for country and country-round in place of fixed effects,
# with round dummies and the same controls (no country-level variables), fitted
# by lmerTest and read with parameters: t on Satterthwaite df (Kuznetsova,
# Brockhoff and Christensen 2017, Journal of Statistical Software). lme4
# results are kept to six significant digits, the precision that is stable
# from run to run.
ri_frame <- spec_frame(ess)
ri_fit <- lmerTest::lmer(
  stats::reformulate(c("x", CONTROL_TERMS$paper, "factor(essround)", "(1 | cntry)", "(1 | cr)"), response = "y"),
  data = ri_frame)
ri_coef <- parameters::model_parameters(ri_fit, effects = "fixed", ci_method = "satterthwaite", keep = "^x$")
random_intercepts <- tibble::tibble(
  axis = "fe", option = "random_intercepts", fit_b = signif(ri_coef$Coefficient, 6),
  fit_se = signif(ri_coef$SE, 6), fit_stat_type = "t", fit_stat = signif(ri_coef$t, 6),
  fit_df = signif(ri_coef$df_error, 6), fit_p = signif(ri_coef$p, 6), fit_n = stats::nobs(ri_fit),
  fit_n_country_rounds = lme4::ngrps(ri_fit)[["cr"]],
  fit_ci_low = signif(ri_coef$CI_low, 6), fit_ci_high = signif(ri_coef$CI_high, 6)
)

display_options <- dplyr::bind_rows(option_rows, voters_only, random_intercepts) |>
  dplyr::select(axis, option, b = fit_b, se = fit_se, stat = fit_stat, df = fit_df, p = fit_p,
                t_conv = fit_t_conv, r = fit_r, b_std = fit_b_std, n = fit_n,
                n_country_rounds = fit_n_country_rounds, ci_low = fit_ci_low, ci_high = fit_ci_high) |>
  datawizard::data_modify(b_reference = reference$b, shift = b - reference$b)

# readr writes the bytes of the committed files; datawizard::data_write() does not
readr::write_csv(display_options, "cases/trust/data/display_options.csv", na = "")

# Inference of the reference specification under several rules. The CR2 rule
# corrects the clustered errors for the leverage of each cluster (Bell and
# McCaffrey 2002, Survey Methodology) and tests the coefficient on Satterthwaite
# degrees of freedom, which fall as a few countries come to carry the variation
# (Pustejovsky and Tipton 2018, Journal of Business & Economic Statistics). It
# comes from estimatr (Blair, Cooper, Coppock, Humphreys and Sonnet), which
# absorbs the intercepts of the country-rounds; clubSandwich gives the same
# errors and df (R/trust/check_parity.R) but holds a dense matrix for every
# respondent pair within each country, which 70,000 respondents in 16 countries
# put out of reach. The slowest step of the pipeline, about 50 minutes.
# Its results are read from the fit itself, because parameters reports the
# residual df of an estimatr fit rather than its Satterthwaite df.
ref_frame <- spec_frame(ess)
cr2_fit <- estimatr::lm_robust(stats::reformulate(c("x", CONTROL_TERMS$paper), response = "y"),
                               data = ref_frame, fixed_effects = ~cr, clusters = cntry, se_type = "CR2")
cr2 <- list(b = cr2_fit$coefficients[["x"]], SE = cr2_fit$std.error[["x"]], t = cr2_fit$statistic[["x"]],
            df_error = cr2_fit$df[["x"]], p = cr2_fit$p.value[["x"]], CI_low = cr2_fit$conf.low[["x"]],
            CI_high = cr2_fit$conf.high[["x"]])
stopifnot(abs(cr2$b - reference$b) < 1e-10)
by_round <- fit_at(cluster = "country_round")

# Each rule with its 95% interval, as its package computes it
ri <- random_intercepts
display_inference <- tibble::tribble(
  ~rule,                                   ~se,               ~stat,            ~df,               ~p,               ~ci_low,               ~ci_high,
  "conventional (model-based) errors",     reference$se_conv, reference$t_conv, reference$df_conv, reference$p_conv, reference$ci_conv_low, reference$ci_conv_high,
  "clustered by country-round (CR1)",      by_round$se,       by_round$stat,    by_round$df,       by_round$p,       by_round$ci_low,       by_round$ci_high,
  "clustered by country (CR1), the curve", reference$se,      reference$stat,   reference$df,      reference$p,      reference$ci_low,      reference$ci_high,
  "CR2 by country with Satterthwaite df",  cr2$SE,            cr2$t,            cr2$df_error,      cr2$p,            cr2$CI_low,            cr2$CI_high,
  "random intercepts (multilevel model)",  ri$fit_se,         ri$fit_stat,      ri$fit_df,         ri$fit_p,         ri$fit_ci_low,         ri$fit_ci_high
) |>
  dplyr::mutate(b = c(rep(reference$b, 4), ri$fit_b), .after = rule) |>
  datawizard::data_modify(note = c(
    "Assumes independent respondents.",
    "57 clusters, t on 56 df.",
    "16 clusters, t on 15 df: the rule that classes every specification of the curve.",
    "16 clusters, errors corrected for the leverage of each country and t on Satterthwaite df, which suit few clusters; from estimatr, which agrees with clubSandwich.",
    "lme4 with Satterthwaite df; the random intercepts replace the fixed effects."
  ))

readr::write_csv(display_inference, "cases/trust/data/display_inference.csv", na = "")

# The reference estimate with each country left out in turn, with its 95%
# interval on 14 df. dplyr inside the lambda, because datawizard cannot see
# its argument.
display_loo <- unique(ess$cntry) |>
  purrr::map(\(country) purrr::exec(fit_spec, dplyr::filter(ess, cntry != country), !!!REFERENCE) |>
               dplyr::mutate(left_out = country, .before = 1)) |>
  dplyr::bind_rows() |>
  datawizard::data_select(c("left_out", "b", "se", "stat", "df", "p", "n", "n_country_rounds",
                            "ci_low", "ci_high")) |>
  datawizard::data_modify(b_reference = reference$b)

readr::write_csv(display_loo, "cases/trust/data/display_loo.csv")

# Cronbach's alpha and the mean inter-item correlation of the two scales, from
# performance (Luedecke, Ben-Shachar, Patil, Waggoner and Makowski 2021,
# Journal of Open Source Software), on the respondents of the paper who
# answered every item, by country and pooled ("all")
scales <- list(concern = CONCERN_ITEMS$index, trust = OUTCOME_ITEMS$index)
paper_sample <- datawizard::data_filter(ess, citizen %in% 1, minority %in% 0)
display_reliability <- purrr::imap(scales, \(items, scale) {
  complete <- tidyr::drop_na(paper_sample, dplyr::all_of(items))
  dplyr::bind_rows(complete, dplyr::mutate(complete, cntry = "all")) |>
    tidyr::nest(data = -cntry) |>
    dplyr::mutate(stats = purrr::map(data, \(d) tibble::tibble(
      alpha = performance::cronbachs_alpha(d[, items]),
      mean_r = performance::item_intercor(d[, items]),
      n = nrow(d)
    ))) |>
    dplyr::select(cntry, stats) |>
    tidyr::unnest(stats) |>
    dplyr::mutate(scale = scale, .before = 1)
}) |>
  dplyr::bind_rows()

readr::write_csv(display_reliability, "cases/trust/data/display_reliability.csv")

# Country-round means of concern and of distrust in parliament (design-weighted,
# paper sample, complete answers): the between-country picture that fixed
# effects set aside
display_country_means <- paper_sample |>
  dplyr::mutate(concern = scale_mean(dplyr::pick(dplyr::everything()), CONCERN_ITEMS$index, "complete")) |>
  datawizard::data_filter(!is.na(concern), !is.na(distrust_parl)) |>
  datawizard::data_summary(concern = stats::weighted.mean(concern, dweight),
                           distrust_parl = stats::weighted.mean(distrust_parl, dweight),
                           n = n(), by = c("cntry", "essround")) |>
  datawizard::data_arrange(c("cntry", "essround"))

readr::write_csv(display_country_means, "cases/trust/data/display_country_means.csv")
