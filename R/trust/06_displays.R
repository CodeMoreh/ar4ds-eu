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

library(tidyverse)
library(lmerTest)
source("R/trust/_tools.R")

ess <- read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)
reference <- do.call(fit_spec, c(list(ess), REFERENCE))
fit_at <- \(...) do.call(fit_spec, modifyList(c(list(ess = ess), REFERENCE), list(...)))

# Options off the curve that a linear model can fit, each changed alone from
# the reference specification
option_rows <- tribble(
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
) |>
  mutate(fit = map(args, \(a) do.call(fit_at, a))) |>
  select(-args) |>
  unnest(fit, names_sep = "_")

# Voters only: the vote controls of the paper (cabinet, far right) dropped to
# missing for everyone who did not vote, as the first attempt of the SCORE
# source-data reproduction 65wm6 coded them
voters_only <- do.call(fit_spec, c(list(filter(ess, voted %in% 1)), REFERENCE)) |>
  rename_with(\(x) paste0("fit_", x)) |>
  mutate(axis = "votes", option = "nonvoters_missing", .before = 1)

# Random intercepts for country and country-round in place of fixed effects,
# with round dummies and the same controls (no country-level variables)
ri_frame <- spec_frame(ess)
ri_fit <- lmerTest::lmer(
  as.formula(paste("y ~", paste(c("x", CONTROL_TERMS$paper), collapse = " + "),
                   "+ factor(essround) + (1 | cntry) + (1 | cr)")),
  data = ri_frame)
# lme4 results kept to six significant digits, the precision that is stable
# from run to run
ri_coef <- signif(coef(summary(ri_fit))["x", ], 6)
random_intercepts <- tibble(
  axis = "fe", option = "random_intercepts", fit_b = ri_coef[["Estimate"]], fit_se = ri_coef[["Std. Error"]],
  fit_stat_type = "t", fit_stat = ri_coef[["t value"]], fit_df = ri_coef[["df"]],
  fit_p = ri_coef[["Pr(>|t|)"]], fit_n = nobs(ri_fit), fit_n_country_rounds = lme4::ngrps(ri_fit)[["cr"]]
)

display_options <- bind_rows(option_rows, voters_only, random_intercepts) |>
  select(axis, option, b = fit_b, se = fit_se, stat = fit_stat, df = fit_df, p = fit_p,
         t_conv = fit_t_conv, r = fit_r, b_std = fit_b_std, n = fit_n, n_country_rounds = fit_n_country_rounds) |>
  mutate(b_reference = reference$b, shift = b - reference$b)

write_csv(display_options, "cases/trust/data/display_options.csv", na = "")

# Inference of the reference specification under several rules. The wild
# cluster bootstrap uses Webb weights, which suit 16 clusters better than
# Rademacher weights (only 2^16 sign patterns), with the null imposed.
# boottest() does not parse the absorbed fixed effects of a fixest fit here,
# so the bootstrap runs on lm() with country-round dummies (the same estimate).
ref_frame <- as.data.frame(spec_frame(ess))
ref_lm <- lm(as.formula(paste("y ~", paste(c("x", CONTROL_TERMS$paper), collapse = " + "), "+ factor(cr)")),
             data = ref_frame)
set.seed(20261025)
dqrng::dqset.seed(20261025)
wild <- fwildclusterboot::boottest(ref_lm, param = "x", clustid = "cntry", B = 9999,
                                   type = "webb", impose_null = TRUE)
by_round <- fit_at(cluster = "country_round")

display_inference <- tribble(
  ~rule,                                   ~se,               ~stat,               ~df,                ~p,
  "conventional (model-based) errors",     reference$se_conv, reference$t_conv,    reference$df_conv,  reference$p_conv,
  "clustered by country-round (CR1)",      by_round$se,       by_round$stat,       by_round$df,        by_round$p,
  "clustered by country (CR1), the curve", reference$se,      reference$stat,      reference$df,       reference$p,
  "wild cluster bootstrap by country",     NA,                wild$t_stat,         NA,                 wild$p_val,
  "random intercepts (multilevel model)",  ri_coef[["Std. Error"]], ri_coef[["t value"]], ri_coef[["df"]], ri_coef[["Pr(>|t|)"]]
) |>
  mutate(b = c(rep(reference$b, 4), ri_coef[["Estimate"]]), .after = rule) |>
  mutate(note = c("Assumes independent respondents.",
                  "57 clusters, t on 56 df.",
                  "16 clusters, t on 15 df: the rule that classes every specification of the curve.",
                  "Webb weights, 9,999 draws, null imposed; the p-value cannot go below about 1/10,000.",
                  "lme4 with Satterthwaite df; the random intercepts replace the fixed effects."))

write_csv(display_inference, "cases/trust/data/display_inference.csv", na = "")

# The reference estimate with each country left out in turn
display_loo <- unique(ess$cntry) |>
  map(\(country) do.call(fit_spec, c(list(filter(ess, cntry != country)), REFERENCE)) |>
        mutate(left_out = country, .before = 1)) |>
  bind_rows() |>
  select(left_out, b, se, stat, df, p, n, n_country_rounds) |>
  mutate(b_reference = reference$b)

write_csv(display_loo, "cases/trust/data/display_loo.csv")

# Cronbach's alpha and the mean inter-item correlation of the two scales, on the
# respondents of the paper who answered every item, by country and pooled
alpha_of <- function(m) {
  k <- ncol(m)
  r <- cor(m)
  tibble(alpha = k / (k - 1) * (1 - sum(diag(cov(m))) / var(rowSums(m))),
         mean_r = mean(r[upper.tri(r)]), n = nrow(m))
}
scales <- list(concern = CONCERN_ITEMS$index, trust = OUTCOME_ITEMS$index)
paper_sample <- filter(ess, citizen %in% 1, minority %in% 0)
display_reliability <- imap(scales, \(items, scale) {
  complete <- drop_na(paper_sample, all_of(items))
  by_country <- complete |>
    nest(data = -cntry) |>
    mutate(stats = map(data, \(d) alpha_of(as.matrix(d[, items])))) |>
    select(cntry, stats) |>
    unnest(stats)
  bind_rows(by_country, mutate(alpha_of(as.matrix(complete[, items])), cntry = "all")) |>
    mutate(scale = scale, .before = 1)
}) |>
  bind_rows()

write_csv(display_reliability, "cases/trust/data/display_reliability.csv")

# Country-round means of concern and of distrust in parliament (design-weighted,
# paper sample, complete answers): the between-country picture that fixed
# effects set aside
display_country_means <- paper_sample |>
  mutate(concern = scale_mean(pick(everything()), CONCERN_ITEMS$index, "complete")) |>
  filter(!is.na(concern), !is.na(distrust_parl)) |>
  summarise(concern = weighted.mean(concern, dweight), distrust_parl = weighted.mean(distrust_parl, dweight),
            n = n(), .by = c(cntry, essround)) |>
  arrange(cntry, essround)

write_csv(display_country_means, "cases/trust/data/display_country_means.csv")
