################################################################################
# Title:        04_reference_marks.R
# Purpose:      The reference marks of the curve: the published result, the two
#               SCORE reproductions from source data, the five Multi100 analyses
#               refitted from their public scripts on the file that Multi100
#               gave the analysts and checked against their recorded values, and
#               the two SCORE replications on later ESS rounds. Also refits the
#               model of the SCORE reproduction (osf.io/jp7tr) on the course
#               extract, the reproduction target of the case.
# Reads:        cases/trust/data/trust_ess.csv, cases/trust/data/trust_macro.csv,
#               data-raw/trust/multi100/multi100_mclaren_extract.csv, and the
#               Multi100 analysis file (McLaren_SRD_final_data.csv, osf.io/e48gd),
#               fetched once into data-raw/trust/cache/
# Writes:       cases/trust/data/analysts5.csv, cases/trust/data/reference_marks.csv,
#               cases/trust/data/display_reproduction.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

library(tidyverse)
library(lmerTest)
source("R/trust/_tools.R")

ess <- read_csv("cases/trust/data/trust_ess.csv", show_col_types = FALSE)
macro <- read_csv("cases/trust/data/trust_macro.csv", show_col_types = FALSE)

# The file that Multi100 gave the five analysts: the dataset that the SCORE
# source-data reproduction 65wm6 built from ESS rounds 1-4 (osf.io/e48gd,
# component 294hy). It holds person-level ESS data under an unstated licence,
# so it stays in the gitignored cache and only results leave this script.
SRD_URL <- "https://osf.io/download/yehpm/"
SRD_MD5 <- "afc3762de082e3249601973a0986fff3"
srd_path <- "data-raw/trust/cache/McLaren_SRD_final_data.csv"
dir.create(dirname(srd_path), showWarnings = FALSE, recursive = TRUE)
if (!file.exists(srd_path)) download.file(SRD_URL, srd_path, mode = "wb", quiet = TRUE)
stopifnot(unname(tools::md5sum(srd_path)) == SRD_MD5)
srd <- read_csv(srd_path, show_col_types = FALSE, col_select = -1)

m100 <- read_csv("data-raw/trust/multi100/multi100_mclaren_extract.csv", show_col_types = FALSE) |>
  transmute(
    id = analyst_id, discipline = primary_discipline, hours = total_hours,
    task1 = if_else(str_starts(task1_categorisation, "The results show evidence for"),
                    "evidence for the claim", task1_categorisation),
    direction = direction_of_result, software = task2_software,
    statistic_type = reanalysis_type_of_statistic, statistic_recorded = reanalysis_statistic_report,
    df1_recorded = reanalysis_degrees_of_freedom_1, df2_recorded = reanalysis_degrees_of_freedom_2,
    n_recorded = reanalysis_model_sample_size, r_recorded = reanalysis_correlation_coef,
    d_recorded = reanalysis_cohens_d
  )

# lme4 fits can differ from run to run in the eighth significant digit, so
# their results are kept to six, which keeps the written files stable.
coef_row <- \(fit, term) signif(coef(summary(fit))[term, ], 6)

# N8P2J (R, lmerTest): the full model of Table 3 on the Multi100 file, with
# country-round and country random intercepts, REML; t with Satterthwaite df
fit_n8p2j <- lmerTest::lmer(
  trstprl_r ~ im_conc + happy_r + stflife_r + sclmeet_r + distrust + stfeco_r + hincfel +
    stfhlth_r + stfedu_r + winner + far_right + lrscale + hhinc_std + agea + edu + female +
    far_right_share + spr_estat + factor(round) + long_im_his + wgi + GDP + unemp +
    (1 | country_round) + (1 | cntry), data = srd)

# 6U3LL (R, lmerTest): the same model with the education variable of the ESS
# (edulvla, "other" coded 55 left in) and without unemployment
fit_6u3ll <- lmerTest::lmer(
  trstprl_r ~ im_conc + happy_r + stflife_r + sclmeet_r + distrust + stfeco_r + hincfel +
    stfhlth_r + stfedu_r + winner + far_right + lrscale + hhinc_std + agea + edulvla + female +
    far_right_share + spr_estat + factor(round) + long_im_his + wgi + GDP +
    (1 | country_round) + (1 | cntry), data = srd)

# LT39B (Stata, mixed, ML): the single item "worse or better place to live" as
# the measure of concern, raw satisfaction items, satisfaction with the
# government, country dummies and random intercepts for country and country-round
lt39b_data <- srd |>
  mutate(unhappy = 10 - happy) |>
  drop_na(trstprl_r, imwbcnt_r, unhappy, stflife, sclmeet, distrust, hincfel, stfhlth, stfedu,
          stfgov, lrscale, agea, female, edulvla, hinctnt, winner, far_right, wgi, GDP, unemp, round)
fit_lt39b <- lme4::lmer(
  trstprl_r ~ imwbcnt_r + unhappy + stflife + factor(sclmeet) + distrust + hincfel + stfhlth +
    stfedu + stfgov + lrscale + agea + female + factor(edulvla) + hinctnt + winner + far_right +
    wgi + GDP + unemp + factor(round) + factor(cntry_num) + (1 | cntry) + (1 | country_round),
  data = lt39b_data, REML = FALSE)

# 456AD (Stata, mixed, ML): the three items averaged over the answered ones
# (alpha, gen()), a shorter set of controls with satisfaction with the
# government, country dummies and random intercepts; the sample of the
# analyst's regression with far-right share and long-term immigration as well
ad_data <- srd |>
  mutate(immcon = scale_mean(pick(everything()), c("imbgeco_r", "imueclt_r", "imwbcnt_r"), "available")) |>
  drop_na(trstprl_r, immcon, happy_r, stflife_r, sclmeet_r, distrust, stfhlth_r, stfedu_r, stfgov_r,
          lrscale, agea, female, edulvla, far_right_share, spr_estat, long_im_his, wgi, GDP, unemp, round)
fit_456ad <- lme4::lmer(
  trstprl_r ~ immcon + happy_r + stflife_r + factor(sclmeet_r) + distrust + stfhlth_r + stfedu_r +
    stfgov_r + lrscale + agea + female + factor(edulvla) + spr_estat + wgi + GDP + unemp +
    factor(round) + factor(cntry_num) + (1 | cntry) + (1 | country_round),
  data = ad_data, REML = FALSE)

# FQ1G7 (jamovi): trust in parliament, not reversed, with a country random
# intercept and the controls listed in the report. The report also names "year",
# which the file does not hold; this refit matches the recorded sample of 77,384
# and comes within 0.1% of the recorded F, but not exactly.
fq1g7_data <- drop_na(srd, trstprl, im_conc, happy, agea, female, edu, hinctnt, distrust, round,
                      far_right_share, GDP, unemp)
fit_fq1g7 <- lmerTest::lmer(
  trstprl ~ im_conc + happy + agea + female + edu + hinctnt + distrust + factor(round) +
    far_right_share + GDP + unemp + (1 | cntry), data = fq1g7_data)
f_fq1g7 <- signif(anova(fit_fq1g7)["im_conc", ], 6)

refits <- tribble(
  ~id,     ~outcome,                        ~concern,                 ~method,
  "N8P2J", "distrust in parliament",        "index (any item)",       "three-level model of Table 3, REML, lmerTest",
  "FQ1G7", "trust in parliament",           "index (any item)",       "model with a country random intercept and eleven controls, jamovi",
  "LT39B", "distrust in parliament",        "single item: worse or better place", "Table 3 controls (some raw) with satisfaction with government, country dummies and random intercepts, ML, Stata",
  "456AD", "distrust in parliament",        "index (any item)",       "shorter controls with satisfaction with government, country dummies and random intercepts, ML, Stata",
  "6U3LL", "distrust in parliament",        "index (any item)",       "Table 3 without unemployment, REML, lmerTest"
) |>
  mutate(
    b_refit = c(coef_row(fit_n8p2j, "im_conc")[["Estimate"]], -coef_row(fit_fq1g7, "im_conc")[["Estimate"]],
                coef_row(fit_lt39b, "imwbcnt_r")[["Estimate"]], coef_row(fit_456ad, "immcon")[["Estimate"]],
                coef_row(fit_6u3ll, "im_conc")[["Estimate"]]),
    se_refit = c(coef_row(fit_n8p2j, "im_conc")[["Std. Error"]], coef_row(fit_fq1g7, "im_conc")[["Std. Error"]],
                 coef_row(fit_lt39b, "imwbcnt_r")[["Std. Error"]], coef_row(fit_456ad, "immcon")[["Std. Error"]],
                 coef_row(fit_6u3ll, "im_conc")[["Std. Error"]]),
    statistic_refit = c(coef_row(fit_n8p2j, "im_conc")[["t value"]], f_fq1g7$`F value`,
                        coef_row(fit_lt39b, "imwbcnt_r")[["t value"]], coef_row(fit_456ad, "immcon")[["t value"]],
                        coef_row(fit_6u3ll, "im_conc")[["t value"]]),
    df_refit = c(coef_row(fit_n8p2j, "im_conc")[["df"]], f_fq1g7$DenDF, NA,
                 NA, coef_row(fit_6u3ll, "im_conc")[["df"]]),
    n_refit = c(nobs(fit_n8p2j), nobs(fit_fq1g7), nobs(fit_lt39b), nobs(fit_456ad), nobs(fit_6u3ll))
  )

# A refit matches when it gives the recorded statistic to the decimals that
# Multi100 recorded, on the recorded number of respondents.
decimals <- \(x) nchar(str_remove(format(x, scientific = FALSE, drop0trailing = TRUE), "^[^.]*\\.?"))

analysts <- m100 |>
  left_join(refits, by = "id") |>
  mutate(
    matches_record = round(statistic_refit, map_int(statistic_recorded, decimals)) == statistic_recorded &
      n_refit == n_recorded,
    r_refit = r_multi100(statistic_refit, statistic_type, df_refit, n_refit)
  )

stopifnot(nrow(analysts) == 5,
          sum(analysts$matches_record) == 4,
          !analysts$matches_record[analysts$id == "FQ1G7"])

write_csv(analysts, "cases/trust/data/analysts5.csv", na = "")

# The reproduction target: the model of the SCORE reproduction from source data
# (osf.io/jp7tr, focal analysis: b = 0.1780, SE = 0.00428, t = 41.583,
# N = 69,522 in 56 country-rounds) refitted on the course extract. That
# reproduction used the four integrated round files, which lack the Italian
# round 2, so the first refit leaves it out; the second keeps it.
repro_frame <- ess |>
  filter(citizen == 1, minority == 0) |>
  mutate(imm_concern = scale_mean(pick(everything()), CONCERN_ITEMS$index, "complete"),
         soc_distrust = scale_mean(pick(everything()), c("distrust_people", "unfair_people", "unhelpful_people"),
                                   "complete"),
         cntry_round = paste0(cntry, essround)) |>
  left_join(macro, by = c("cntry", "essround"))

fit_repro <- function(data) {
  lmerTest::lmer(
    distrust_parl ~ imm_concern + unhappy + life_dissat + meet_rarely + soc_distrust + econ_dissat +
      income_difficulty + health_dissat + educ_dissat + winner + far_right_vote + left_right +
      hh_income_z + age + education + female + far_right_share + social_protection +
      factor(essround) + long_term_imm + wgi + gdp_index + unemployment +
      (1 | cntry) + (1 | cntry_round), data = data)
}
repro_no_it2 <- fit_repro(filter(repro_frame, !(cntry == "IT" & essround == 2)))
repro_all <- fit_repro(repro_frame)

repro_row <- function(fit, label, note) {
  cf <- coef_row(fit, "imm_concern")
  groups <- lme4::ngrps(fit)
  tibble(id = label, b = cf[["Estimate"]], se = cf[["Std. Error"]], statistic_type = "t",
         statistic = cf[["t value"]], df = cf[["df"]], n = nobs(fit),
         country_rounds = groups[["cntry_round"]], countries = groups[["cntry"]], note = note)
}

ref_fe <- do.call(fit_spec, c(list(ess), REFERENCE))
ref_fe_59 <- do.call(fit_spec, c(list(filter(ess, !(cntry == "IT" & essround == 2))), REFERENCE))
fe_row <- \(fit, label, note) {
  tibble(id = label, b = fit$b, se = fit$se, statistic_type = "t", statistic = fit$stat, df = fit$df,
         n = fit$n, country_rounds = fit$n_country_rounds, countries = fit$n_clusters, note = note)
}
reproduction <- bind_rows(
  tibble(id = "SCORE_jp7tr", b = 0.1780, se = 0.00428, statistic_type = "t", statistic = 41.583,
         df = 69400, n = 69522, country_rounds = 56, countries = 16,
         note = "Published: SCORE reproduction from source data (osf.io/jp7tr), focal analysis, lmer with REML; df as printed (6.940e+04)."),
  repro_row(repro_no_it2, "course_repro_59",
            "The model of the SCORE reproduction refitted on the course extract without the Italian round 2, as the integrated round files held it."),
  repro_row(repro_all, "course_repro_60",
            "The same model on all 60 country-rounds of the course extract, the Italian round 2 included."),
  fe_row(ref_fe_59, "course_reference_fe_59",
         "The reference specification of the curve on the respondents of the SCORE reproduction (no Italian round 2): fixed effects for country-round, errors clustered by country (t on 15 df); the browser lab can fit it."),
  fe_row(ref_fe, "course_reference_fe",
         "The reference specification of the curve: fixed effects for country-round, errors clustered by country (t on 15 df).")
)

write_csv(reproduction, "cases/trust/data/display_reproduction.csv", na = "")

# The marks. Every mark is the coefficient of concern about immigration on
# distrust in parliament (0-10 per point), the scale of the curve.
marks <- bind_rows(
  tibble(id = "OA", label = "Original author, Table 3, parliament (published)",
         statistic_type = NA, statistic = NA, n = 110732, b = 0.17, on_chart = TRUE, data = "original",
         note = "Three-level model fitted in HLM, b = 0.17 with SE printed as 0.00 and p = 0.000, so no test statistic can be recovered; N = 110,732 respondents, 59 country-rounds, 16 countries."),
  tibble(id = "SCORE_jp7tr", label = "SCORE reproduction from source data (osf.io/jp7tr)",
         statistic_type = "t", statistic = 41.583, n = 69522, b = 0.1780, on_chart = TRUE, data = "original",
         note = "The model of Table 3 in lme4 on the four integrated ESS files, complete answers to every scale, b = 0.1780 (SE 0.00428)."),
  tibble(id = "SCORE_65wm6", label = "SCORE reproduction from source data (osf.io/e48gd)",
         statistic_type = "t", statistic = 42.148, n = 68113, b = 0.18124605, on_chart = TRUE, data = "original",
         note = "The model of Table 3 in lme4 on the file that Multi100 later gave the analysts; Switzerland drops out for want of unemployment data. The SCORE record of the reproduction."),
  analysts |>
    transmute(id, label = id, statistic_type, statistic = statistic_recorded, n = n_refit, b = b_refit,
              on_chart = TRUE, data = "original",
              note = if_else(matches_record,
                             "Placed by the course refit of the analysis, which reproduces the recorded statistic.",
                             "Placed by the course refit of the analysis, which reproduces the recorded sample and comes within 0.1% of the recorded F.")),
  tibble(id = "SCORE_1634", label = "SCORE replication on ESS round 5 (osf.io/rpumq)",
         statistic_type = "t", statistic = 21.1737371, n = 17134, b = 0.1828843, on_chart = TRUE, data = "new",
         note = "Replication on the 2010 round: 13 of the 16 countries (the integrated file of the round holds no Austrian or Italian sample, and Portugal lacks household income), a two-level model with post-stratification weights, SE 0.00864."),
  tibble(id = "SCORE_65wm6_DAR", label = "SCORE replication on ESS rounds 5-9 (osf.io/e48gd)",
         statistic_type = "t", statistic = 50.11, n = 90286, b = 0.185703303, on_chart = TRUE, data = "new",
         note = "Data-analytic replication on 72 country-rounds of 16 countries, 2010-2018, SE 0.00371.")
)

write_csv(marks, "cases/trust/data/reference_marks.csv", na = "")
