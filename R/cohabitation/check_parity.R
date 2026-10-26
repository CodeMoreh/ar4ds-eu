################################################################################
# Title:        check_parity.R
# Purpose:      Checks the hand-written estimator of R/cohabitation/_tools.R
#               against established packages: the multinomial fits against nnet
#               (coefficients, model-based errors) and svyVGAM (design-based
#               errors), the logit, complementary log-log and linear fits
#               against survey::svyglm (coefficients, design-based errors and
#               degrees of freedom, the analysed unions as a subset of the full
#               design), the life table against the Aalen-Johansen estimator of
#               survival, and the committed curve against a fresh fit.
# Reads:        cases/cohabitation/data/cohab_unions.csv,
#               cases/cohabitation/data/spec_curve.csv,
#               cases/cohabitation/data/display_lifetable.csv
# Writes:       nothing; stops on any disagreement
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

library(tidyverse)
library(nnet)
library(survey)
library(svyVGAM)
library(survival)
source("R/cohabitation/_tools.R")

unions <- read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                   col_types = cols(caseid = col_character(), stratum = col_character(), psu = col_character()))
curve <- read_csv("cases/cohabitation/data/spec_curve.csv", show_col_types = FALSE,
                  col_types = cols(stat_type = col_character()))
lifetable <- read_csv("cases/cohabitation/data/display_lifetable.csv", show_col_types = FALSE)

# The analysed person-months of a specification with its model columns, and one
# row for every union of the same surveys that the specification leaves out, so
# that the survey design keeps every stratum and cluster (a domain analysis)
design_frame <- function(cohorts, horizon, adjust, weights, time, comparison = "1995_2006") {
  frame <- spec_frame(unions, cohorts, horizon, weights, comparison)
  x <- spec_matrix(frame, adjust, time)
  analysed <- as_tibble(x[, -1, drop = FALSE]) |>
    mutate(y = frame$y, w = frame$w, stratum = frame$stratum, psu = frame$psu, keep = TRUE)
  pair <- COMPARISONS[[comparison]]
  analysed_unions <- distinct(frame, cycle, caseid)
  others <- unions |>
    filter(cycle %in% c(pair$earlier, pair$later)) |>
    anti_join(analysed_unions, by = c("cycle", "caseid")) |>
    transmute(w = 1, stratum, psu, keep = FALSE)
  bind_rows(analysed, others) |>
    mutate(across(all_of(colnames(x)[-1]), \(v) replace_na(v, 0)), y = replace_na(y, "intact"))
}

svy_fit <- function(cohorts, horizon, adjust, weights, time, model, comparison = "1995_2006") {
  d <- design_frame(cohorts, horizon, adjust, weights, time, comparison)
  terms <- setdiff(names(d), c("y", "w", "stratum", "psu", "keep"))
  des <- svydesign(ids = ~psu, strata = ~stratum, weights = ~w, data = d, nest = TRUE) |>
    subset(keep)  # r-style: keep – the subset method of a survey design keeps its full design
  form <- as.formula(paste("marry ~", paste(sprintf("`%s`", terms), collapse = " + ")))
  des <- update(des, marry = as.numeric(y == "marriage"))
  fam <- switch(model, logit = quasibinomial(), cloglog = quasibinomial(link = "cloglog"), lpm = gaussian())
  fit <- svyglm(form, design = des, family = fam)
  tibble(b_ref = coef(fit)[["later"]], se_ref = SE(fit)[["later"]], df_ref = fit$df.residual)
}

svy_mlogit <- function(cohorts, horizon, adjust, weights, time) {
  d <- design_frame(cohorts, horizon, adjust, weights, time) |>
    mutate(y = factor(y, levels = c("marriage", "separation", "intact")))
  terms <- setdiff(names(d), c("y", "w", "stratum", "psu", "keep"))
  des <- svydesign(ids = ~psu, strata = ~stratum, weights = ~w, data = d, nest = TRUE) |>
    subset(keep)  # r-style: keep – the subset method of a survey design keeps its full design
  form <- as.formula(paste("y ~", paste(sprintf("`%s`", terms), collapse = " + ")))
  fit <- svy_vglm(form, design = des, family = VGAM::multinomial(refLevel = "intact"))
  co <- coef(fit)
  at <- which(names(co) == "later:1")
  tibble(b_ref = co[[at]], se_ref = sqrt(diag(vcov(fit)))[[at]])
}

# nnet computes its Hessian numerically, which loses about 0.2% of the error of
# `later` when age squared (up to about 1,800) sits unscaled beside 0/1 columns;
# on columns divided by their largest value, as fit_spec() fits them, it agrees
nnet_mlogit <- function(cohorts, horizon, adjust, weights, time) {
  frame <- spec_frame(unions, cohorts, horizon, weights)
  x <- spec_matrix(frame, adjust, time)
  x <- sweep(x, 2, apply(abs(x), 2, max), `/`)  # r-style: keep – a model matrix
  d <- as_tibble(x[, -1, drop = FALSE]) |>
    mutate(y = factor(frame$y, levels = c("intact", "marriage", "separation")))
  form <- as.formula(paste("y ~", paste(sprintf("`%s`", colnames(x)[-1]), collapse = " + ")))
  fit <- multinom(form, data = d, weights = frame$w, trace = FALSE, maxit = 2000, reltol = 1e-14,
                  abstol = 1e-14)
  tibble(b_nnet = coef(fit)["marriage", "later"], se_nnet = sqrt(diag(vcov(fit)))[["marriage:later"]])
}

# Every fourth specification of the curve against survey (logit, cloglog) or
# nnet (mlogit, coefficients and model-based errors); every twelfth mlogit
# against svyVGAM
checked <- curve |>
  filter(spec_id %% 4 == 1)
binary <- checked |>
  filter(model != "mlogit") |>
  mutate(ref = pmap(list(cohorts, horizon, adjust, weights, time, model), svy_fit)) |>
  unnest(ref)
multinomial <- checked |>
  filter(model == "mlogit") |>
  mutate(ref = pmap(list(cohorts, horizon, adjust, weights, time), nnet_mlogit)) |>
  unnest(ref)
multinomial_design <- curve |>
  filter(model == "mlogit", spec_id %% 12 == 1) |>
  mutate(ref = pmap(list(cohorts, horizon, adjust, weights, time), svy_mlogit)) |>
  unnest(ref)

# Off-curve fits that use other parts of the estimator: the linear check, a
# comparison of other cycles, and the interaction model of the paper
off_curve <- tribble(
  ~cohorts, ~horizon,     ~adjust,       ~weights,   ~time,    ~model,  ~comparison,
  "paper",  "five_years", "composition", "weighted", "linear", "lpm",   "1995_2006",
  "paper",  "five_years", "composition", "weighted", "linear", "logit", "2002_2011",
  "paper",  "five_years", "race_period", "weighted", "linear", "logit", "1995_2006"
) |>
  mutate(ours = pmap(pick(everything()), \(...) fit_spec(unions, ...)[, c("b", "se", "df")]),
         ref = pmap(pick(cohorts:comparison), svy_fit)) |>
  unnest(c(ours, ref))

# The life table of all unions against the Aalen-Johansen estimator: a union
# that married or separated has its event at its last month, an intact one is
# censored there, and a union is at risk in every month up to its last
aj <- spec_unions(unions) |>
  mutate(time = duration + 1, state = factor(outcome, levels = c("intact", "marriage", "separation")),
         cohort = if_else(later == 1, "2005-2010", "1990-1995"))
lifetable_all <- filter(lifetable, group == "all")
aj_fit <- survfit(Surv(time, state) ~ cohort, data = aj, weights = weight)
aj_married <- summary(aj_fit, times = c(12, 24, 36, 48))
aj_table <- tibble(cohort = str_remove(as.character(aj_married$strata), "cohort="), month = aj_married$time,
                   married_aj = aj_married$pstate[, which(aj_fit$states == "marriage")],
                   separated_aj = aj_married$pstate[, which(aj_fit$states == "separation")]) |>
  left_join(lifetable_all, by = c("cohort", "month"))

# The committed curve against a fresh fit of the estimator
fresh <- curve |>
  filter(spec_id %% 6 == 1) |>
  mutate(new = pmap(list(cohorts, horizon, adjust, weights, time, model),
                    \(...) fit_spec(unions, ...)[, c("b", "stat", "r")])) |>
  unnest(new, names_sep = "_")

gaps <- tibble(
  b_vs_svyglm = max(abs(c(binary$b - binary$b_ref, off_curve$b - off_curve$b_ref))),
  se_vs_svyglm = max(abs(c(binary$se / binary$se_ref, off_curve$se / off_curve$se_ref) - 1)),
  df_vs_svyglm = max(abs(c(binary$df - binary$df_ref, off_curve$df - off_curve$df_ref))),
  b_vs_nnet = max(abs(multinomial$b - multinomial$b_nnet)),
  se_model_vs_nnet = max(abs(multinomial$se_model / multinomial$se_nnet - 1)),
  b_vs_svyvgam = max(abs(multinomial_design$b - multinomial_design$b_ref)),
  se_vs_svyvgam = max(abs(multinomial_design$se / multinomial_design$se_ref - 1)),
  lifetable_vs_aj = max(abs(c(aj_table$married - aj_table$married_aj, aj_table$separated - aj_table$separated_aj))),
  b_vs_fresh = max(abs(fresh$b - fresh$new_b)),
  stat_vs_fresh = max(abs(fresh$stat - fresh$new_stat))
)
print(as.data.frame(gaps))

# The complementary log-log errors differ from svyglm in the sixth digit at
# most, because the two fits stop at slightly different points of convergence
stopifnot(gaps$b_vs_svyglm < 1e-6, gaps$se_vs_svyglm < 1e-5, gaps$df_vs_svyglm == 0,
          gaps$b_vs_nnet < 1e-4, gaps$se_model_vs_nnet < 1e-4,
          gaps$b_vs_svyvgam < 1e-4, gaps$se_vs_svyvgam < 1e-3,
          gaps$lifetable_vs_aj < 1e-9, gaps$b_vs_fresh < 1e-9, gaps$stat_vs_fresh < 1e-9)
cat("Parity holds on", nrow(binary) + nrow(off_curve), "fits against svyglm,", nrow(multinomial),  # r-style: keep – a checker, whose output is its report
    "against nnet,", nrow(multinomial_design), "against svyVGAM, the life table against survfit and",
    nrow(fresh), "rows of the curve against a fresh fit.\n")
