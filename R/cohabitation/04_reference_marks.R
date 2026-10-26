################################################################################
# Title:        04_reference_marks.R
# Purpose:      The reference marks of the curve and the reproduction of the
#               paper: Models 1 and 2 of Table 2 refitted on the rebuilt unions;
#               the five Multi100 analyses refitted from their public scripts on
#               the person-month file that SCORE built and Multi100 gave them,
#               checked against their recorded values, and run again on the same
#               file with its 1995 cohort rebuilt; the SCORE replication refitted
#               on that file; and the reference specification on later cycles.
# Reads:        cases/cohabitation/data/cohab_unions.csv,
#               data-raw/cohabitation/multi100/multi100_kuo_extract.csv, and
#               cases/cohabitation/data/score_person_months.csv, the
#               person-month file of SCORE (Estimation Data - Kuo (JWzJ).csv,
#               osf.io/b6p3j), fetched from the OSF only if it is absent
# Writes:       cases/cohabitation/data/analysts5.csv,
#               cases/cohabitation/data/reference_marks.csv,
#               cases/cohabitation/data/display_reproduction.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

library(tidyverse)
library(nnet)
source("R/cohabitation/_tools.R")

unions <- read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                   col_types = cols(caseid = col_character(), stratum = col_character(), psu = col_character()))

# The file that SCORE built for its replication (component jkzdq of osf.io/u8x9s)
# and that Multi100 linked as the original materials of the five analysts. Its
# report says that all materials of the project may be shared publicly, and it
# is committed under a course name with its provenance in its codebook, so that
# the workspace and the browser lab run the five analyses offline. The OSF copy
# is fetched only when the committed file is missing; either must match the MD5
# sum of the copy that the analysts received.
SCORE_URL <- "https://osf.io/download/b6p3j/"
SCORE_MD5 <- "2b656d3814d273f4a93e29bc4f0139ef"
score_path <- "cases/cohabitation/data/score_person_months.csv"
if (!file.exists(score_path)) download.file(SCORE_URL, score_path, mode = "wb", quiet = TRUE)
stopifnot(unname(tools::md5sum(score_path)) == SCORE_MD5)
score <- read_csv(score_path, show_col_types = FALSE)

# The same file with its 1995 cohort rebuilt: the 929 unions of the paper in
# the conventions of the SCORE file (an intact union contributes as many months
# as it lasted, a union that ended one more month, with the outcome in the last;
# AGE is the age at the start rounded up; education with the associate degree
# as college). The four other cycles are left as SCORE built them.
rebuilt_1995 <- unions |>
  spec_unions() |>
  filter(cycle == "1995") |>
  mutate(n_months = duration + (outcome != "intact")) |>
  uncount(n_months, .id = "MONTHNUM") |>
  transmute(
    CASEID = as.numeric(caseid), MONTHNUM,
    OUTCOME = if_else(MONTHNUM == duration + 1, c(intact = 1, marriage = 2, separation = 3)[outcome], 1),
    LESSHIGH = as.integer(educ_score == "less_hs"), HIGH = as.integer(educ_score == "hs"),
    SOMECOLLEGE = as.integer(educ_score == "some_college"), COLLEGEPLUS = as.integer(educ_score == "college"),
    P95 = 1, P10 = 0, P13 = 0, P15 = 0,
    ONH = as.integer(race == "other"), HSP = as.integer(race == "hispanic"), AA = as.integer(race == "black"),
    NHW = as.integer(race == "white"), COH1DUR = duration, AGE = ceiling(age_start), P02 = 0
  )
repaired <- bind_rows(rebuilt_1995, filter(score, P95 == 0))

last_rows <- \(d) d |> group_by(CASEID) |> slice_max(MONTHNUM, n = 1) |> ungroup()

# 73FDT (R, nnet): the last month of every woman, a multinomial model of the
# outcome on five period dummies (all five beside the intercept, as in the
# script) with the duration and age; then a regression of the five marriage
# coefficients on the middle year of each period, whose F is the result.
analysis_73fdt <- function(d) {
  women <- last_rows(d)
  fit <- multinom(OUTCOME ~ P95 + P02 + P10 + P13 + P15 + COH1DUR + AGE, data = women, trace = FALSE)
  est <- tibble(year = c(1995, 2002, 2008, 2012, 2014),
                estimate = coef(fit)["2", c("P95", "P02", "P10", "P13", "P15")])
  slope <- summary(lm(estimate ~ year, data = est))
  tibble(stat_type = "F", stat = slope$fstatistic[["value"]], df1 = 1, df2 = 3, n = nrow(women),
         estimate = coef(slope)["year", "Estimate"], p = coef(slope)["year", "Pr(>|t|)"])
}

# N8ZYW (jamovi): the last month of the women of 1995 and 2006-2010, a logit of
# marriage on the later period, two education dummies, two race dummies, the
# total duration and age
analysis_n8zyw <- function(d) {
  women <- last_rows(filter(d, P95 == 1 | P10 == 1))
  fit <- glm(I(OUTCOME == 2) ~ HIGH + COLLEGEPLUS + P10 + HSP + AA + COH1DUR + AGE,
             family = binomial(), data = women)
  co <- coef(summary(fit))["P10", ]
  tibble(stat_type = "z", stat = co[["z value"]], df1 = NA, df2 = NA, n = nrow(women),
         estimate = co[["Estimate"]], p = co[["Pr(>|z|)"]])
}

# 456AP (R, VGAM): every person-month of 2002 and 2013-2015, a multinomial model
# with staying together as reference, on the later period, education, age and
# race, without duration; nnet gives the same maximum-likelihood fit as vglm
analysis_456ap <- function(d) {
  months <- d |>
    filter(P02 == 1 | P15 == 1) |>
    mutate(outcome = factor(OUTCOME, levels = 1:3, labels = c("intact", "marry", "break")))
  fit <- multinom(outcome ~ P15 + LESSHIGH + HIGH + SOMECOLLEGE + AGE + ONH + HSP + AA, data = months,
                  trace = FALSE, maxit = 500)
  est <- coef(fit)["marry", "P15"]
  se <- sqrt(diag(vcov(fit)))[["marry:P15"]]
  tibble(stat_type = "z", stat = est / se, df1 = NA, df2 = NA, n = nrow(months),
         estimate = est, p = 2 * pnorm(-abs(est / se)))
}

# 5X3KG (R): the share of person-month rows that record a marriage in 1995
# against 2006-2010, compared by prop.test() with the continuity correction
analysis_5x3kg <- function(d) {
  rows <- filter(d, P95 == 1 | P10 == 1)
  test <- prop.test(x = c(sum(rows$P95 == 1 & rows$OUTCOME == 2), sum(rows$P10 == 1 & rows$OUTCOME == 2)),
                    n = c(sum(rows$P95), sum(rows$P10)), correct = TRUE)
  tibble(stat_type = "chi2", stat = test$statistic[["X-squared"]], df1 = 1, df2 = NA, n = nrow(rows),
         estimate = unname(test$estimate[2] - test$estimate[1]), p = test$p.value)
}

# JDA15 (R): the last month of every woman of the five cycles, a logit of
# marriage (against separation or an intact union) on education interacted with
# race, age, the total duration and a linear term for years since 1995
analysis_jda15 <- function(d) {
  women <- last_rows(d) |>
    mutate(
      education = factor(case_when(LESSHIGH == 1 ~ "lesshigh", HIGH == 1 ~ "high",
                                   SOMECOLLEGE == 1 ~ "somecollege", COLLEGEPLUS == 1 ~ "collegeplus"),
                         levels = c("lesshigh", "high", "somecollege", "collegeplus")),
      race = factor(case_when(AA == 1 ~ "black", NHW == 1 ~ "white", HSP == 1 ~ "hispanic", ONH == 1 ~ "other"),
                    levels = c("black", "hispanic", "other", "white")),
      year = case_when(P95 == 1 ~ 1995, P02 == 1 ~ 2002, P10 == 1 ~ 2010, P13 == 1 ~ 2013, P15 == 1 ~ 2015) - 1995
    )
  fit <- glm(I(OUTCOME == 2) ~ education * race + AGE + COH1DUR + year, family = binomial(), data = women)
  co <- coef(summary(fit))["year", ]
  tibble(stat_type = "z", stat = co[["z value"]], df1 = NA, df2 = NA, n = nrow(women),
         estimate = co[["Estimate"]], p = co[["Pr(>|z|)"]])
}

ANALYSES <- list(`73FDT` = analysis_73fdt, N8ZYW = analysis_n8zyw, `456AP` = analysis_456ap,
                 `5X3KG` = analysis_5x3kg, JDA15 = analysis_jda15)

m100 <- read_csv("data-raw/cohabitation/multi100/multi100_kuo_extract.csv", show_col_types = FALSE) |>
  transmute(
    id = analyst_id, discipline = primary_discipline, hours = total_hours,
    task1 = task1_categorisation_plotting, direction = direction_of_result, software = task2_software,
    statistic_type = reanalysis_type_of_statistic, statistic_recorded = reanalysis_statistic_report,
    df1_recorded = reanalysis_degrees_of_freedom_1, df2_recorded = reanalysis_degrees_of_freedom_2,
    n_recorded = reanalysis_model_sample_size, r_recorded = reanalysis_correlation_coef,
    d_recorded = reanalysis_cohens_d
  )

r_of <- \(res) r_multi100(res$stat, res$stat_type, res$df2, res$n)

refits <- imap(ANALYSES, \(analysis, id) {
  on_score <- analysis(score)
  on_repaired <- analysis(repaired)
  tibble(id = id,
         stat_type_refit = on_score$stat_type, stat_refit = on_score$stat, n_refit = on_score$n,
         estimate_refit = on_score$estimate, p_refit = on_score$p, r_refit = r_of(on_score),
         stat_repaired = on_repaired$stat, n_repaired = on_repaired$n,
         estimate_repaired = on_repaired$estimate, p_repaired = on_repaired$p, r_repaired = r_of(on_repaired))
}) |>
  bind_rows()

# Choices of each analysis, from the Task 2 reports and the scripts on the OSF
choices <- tribble(
  ~id,     ~comparison,                      ~unit,                           ~outcome_model,                                        ~controls,
  "73FDT", "five cycles, 1995 to 2013-2015", "one row per woman (last month)", "multinomial logit, then a regression of five coefficients on year", "total duration, age",
  "N8ZYW", "1995 against 2006-2010",         "one row per woman (last month)", "logit of marriage against no marriage",                "two education and two race dummies, total duration, age",
  "456AP", "2002 against 2013-2015",         "person-months",                  "multinomial logit, staying together as reference",     "education, race, age; no duration",
  "5X3KG", "1995 against 2006-2010",         "person-month rows",              "two-sample test of proportions (prop.test)",           "none",
  "JDA15", "five cycles, years since 1995",  "one row per woman (last month)", "logit of marriage against no marriage",                "education x race, age, total duration"
)

analysts <- m100 |>
  left_join(choices, by = "id") |>
  left_join(refits, by = "id") |>
  mutate(matches_record = case_when(
    is.na(statistic_recorded) ~ NA,
    .default = abs(abs(stat_refit) - abs(statistic_recorded)) < 0.006 & n_refit == n_recorded
  ))

write_csv(analysts, "cases/cohabitation/data/analysts5.csv", na = "")

# The SCORE replication (report osf.io/s2hez, script Phase I Kuo (JWzJ).R): the
# person-months of 2002 against those of 2011-2013 and 2013-2015 pooled,
# unweighted multinomial model with education x period; the claim tested is the
# some-college x period interaction. The same script on 2002 against 2006-2010
# is what the report calls its reproduction.
score_interaction <- function(d, later_cycles) {
  months <- d |>
    filter(P02 == 1 | if_any(all_of(later_cycles), \(v) v == 1)) |>
    mutate(period = as.integer(if_any(all_of(later_cycles), \(v) v == 1)),
           outcome = relevel(factor(OUTCOME, levels = 1:3, labels = c("Stay Intact", "Marry", "Break Up")),
                             ref = "Stay Intact"),
           AGE2 = AGE^2, LESSHIGH_PERIOD = period * LESSHIGH, HIGH_PERIOD = period * HIGH,
           SOMECOLLEGE_PERIOD = period * SOMECOLLEGE)
  fit <- multinom(outcome ~ period + AA + HSP + ONH + LESSHIGH + HIGH + SOMECOLLEGE + LESSHIGH_PERIOD +
                    HIGH_PERIOD + SOMECOLLEGE_PERIOD + COH1DUR + AGE + AGE2, data = months,
                  trace = FALSE, maxit = 1000)
  est <- coef(fit)["Marry", "SOMECOLLEGE_PERIOD"]
  se <- sqrt(diag(vcov(fit)))[["Marry:SOMECOLLEGE_PERIOD"]]
  tibble(b = est, se = se, stat = est / se, n = nrow(months), n_women = n_distinct(months$CASEID))
}
score_replication <- score_interaction(score, c("P13", "P15"))
score_reproduction <- score_interaction(score, "P10")

# Models 1 and 2 of Table 2 on the rebuilt unions: every coefficient of both
# equations with its design-based error, beside the published values
model_table <- function(adjust) {
  m <- fit_model(unions, adjust = adjust)
  tibble(equation = rep(colnames(m$coef), each = nrow(m$coef)), term = rep(rownames(m$coef), ncol(m$coef)),
         b = as.vector(m$coef), se = sqrt(diag(m$vcov)))
}

published <- tribble(
  ~model, ~term,                ~marriage_b, ~marriage_se, ~separation_b, ~separation_se,
  "1",    "later",              -0.137,      0.18,         -0.343,        0.26,
  "1",    "black",              -0.427,      0.15,          0.363,        0.12,
  "1",    "hispanic",           -0.325,      0.18,         -0.201,        0.15,
  "1",    "other",              -0.466,      0.34,          0.265,        0.21,
  "1",    "less_hs",             0.096,      0.26,         -0.220,        0.24,
  "1",    "hs",                  0.316,      0.17,         -0.088,        0.21,
  "1",    "some_college",        0.202,      0.18,          0.236,        0.22,
  "1",    "later_less_hs",      -0.963,      0.34,          0.564,        0.32,
  "1",    "later_hs",           -1.017,      0.30,          0.432,        0.29,
  "1",    "later_some_college", -0.725,      0.27,          0.168,        0.31,
  "1",    "(Intercept)",        -5.815,      1.15,         -1.660,        1.06,
  "2",    "later",              -0.683,      0.14,          0.071,        0.14,
  "2",    "black",              -0.546,      0.18,          0.517,        0.13,
  "2",    "hispanic",           -0.182,      0.20,         -0.252,        0.19,
  "2",    "other",              -0.058,      0.30,          0.683,        0.33,
  "2",    "less_hs",            -0.222,      0.21,          0.061,        0.20,
  "2",    "hs",                 -0.005,      0.14,          0.109,        0.17,
  "2",    "some_college",       -0.073,      0.14,          0.304,        0.16,
  "2",    "later_black",         0.297,      0.31,         -0.336,        0.22,
  "2",    "later_hispanic",     -0.450,      0.35,          0.073,        0.28,
  "2",    "later_other",        -0.912,      0.64,         -0.671,        0.46,
  "2",    "(Intercept)",        -5.820,      1.12,         -1.943,        1.05
) |>
  pivot_longer(-c(model, term), names_to = c("equation", ".value"), names_sep = "_") |>
  rename(b_published = b, se_published = se)

reproduction <- bind_rows(model_table("educ_period") |> mutate(model = "1"),
                          model_table("race_period") |> mutate(model = "2")) |>
  left_join(published, by = c("model", "equation", "term")) |>
  relocate(model, equation, term)

write_csv(reproduction, "cases/cohabitation/data/display_reproduction.csv", na = "")

# The marks. The published coefficient is that of Model 2, the change for
# non-Hispanic white women in a model with race-ethnicity x period; Multi100
# recorded it as z = -4.8786 on 54,291 person-months.
oa_refit <- fit_spec(unions, adjust = "race_period")
reference <- fit_spec(unions)
later_cycles <- c("2002_2011", "1995_2011") |>
  map(\(cmp) fit_spec(unions, comparison = cmp)) |>
  bind_rows()

marks <- bind_rows(
  tibble(mark = "OA", label = "OAs 2016", source = "Table 2, Model 2 (race-ethnicity x period), published",
         b = -0.683, se = 0.14, stat_type = "z", stat = -0.683 / 0.14, df = NA, n = 54291, on_chart = TRUE),
  tibble(mark = "OA_refit", label = "OA refit", source = "Model 2 refitted on the rebuilt unions, design-based errors",
         b = oa_refit$b, se = oa_refit$se, stat_type = "t", stat = oa_refit$stat, df = oa_refit$df,
         n = oa_refit$n, z_model = oa_refit$z_model, on_chart = FALSE),
  tibble(mark = "reference", label = "Reference", source = "The reference specification of the curve",
         b = reference$b, se = reference$se, stat_type = "t", stat = reference$stat, df = reference$df,
         n = reference$n, z_model = reference$z_model, on_chart = FALSE),
  analysts |>
    transmute(mark = id, label = id,
              source = paste0("Multi100 analyst, refitted on the SCORE file: ", outcome_model, "; ", comparison),
              b = estimate_refit, se = NA, stat_type = stat_type_refit, stat = stat_refit,
              df = if_else(stat_type_refit == "F", 3, NA), n = n_refit,
              on_chart = id == "456AP"),
  tibble(mark = "SCORE_replication", label = "SCORE replication",
         source = "Some college x period, 2002 against 2011-2015, SCORE file, unweighted (refit of osf.io/u8x9s)",
         b = score_replication$b, se = score_replication$se, stat_type = "z", stat = score_replication$stat,
         df = NA, n = score_replication$n, on_chart = FALSE),
  tibble(mark = "SCORE_reproduction", label = "SCORE reproduction",
         source = "Some college x period, 2002 against 2006-2010, SCORE file, unweighted (refit of osf.io/u8x9s)",
         b = score_reproduction$b, se = score_reproduction$se, stat_type = "z", stat = score_reproduction$stat,
         df = NA, n = score_reproduction$n, on_chart = FALSE),
  later_cycles |>
    transmute(mark = paste0("course_", comparison), label = paste("Course,", str_replace(comparison, "_", " vs ")),
              source = "The reference specification on other NSFG cycles (rebuilt unions)",
              b, se, stat_type = "t", stat, df, n, z_model, on_chart = FALSE)
) |>
  # r by the Multi100 rule: from the recorded or refitted statistic, and for the
  # course fits from the conventional z on the person-months, as on the curve
  mutate(p = case_when(stat_type == "t" ~ 2 * pt(-abs(stat), df),
                       stat_type == "z" ~ 2 * pnorm(-abs(stat)),
                       stat_type == "chi2" ~ pchisq(stat, 1, lower.tail = FALSE)),
         r = if_else(is.na(z_model), r_multi100(stat, stat_type, df, n), r_multi100(z_model, "z", NA, n)))

# The p-value of the F of 73FDT comes from its own regression
marks <- marks |>
  mutate(p = if_else(mark == "73FDT", analysts$p_refit[analysts$id == "73FDT"], p))

write_csv(marks, "cases/cohabitation/data/reference_marks.csv", na = "")
