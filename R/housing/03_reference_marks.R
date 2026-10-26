################################################################################
# Title:        03_reference_marks.R
# Purpose:      The five Multi100 analyses, each refitted from its public script
#               on the public data and checked against the value that Multi100
#               recorded, and the reference marks of the curve: the published
#               result of the original authors, its refit on the blinded file,
#               and the five analyses placed in percentage points.
# Reads:        cases/housing/data/housing_audit.csv, data-raw/housing/multi100/multi100_einstein_extract.csv
# Writes:       cases/housing/data/analysts5.csv, cases/housing/data/reference_marks.csv
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
source("R/housing/_tools.R")

audit <- read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)
PAPER <- "Einstein_AmJourPoliSci_2017_mxyQ"

m100 <- read_csv("data-raw/housing/multi100/multi100_einstein_extract.csv", show_col_types = FALSE) |>
  filter(paper_id == PAPER) |>
  transmute(
    id = analyst_id, discipline = primary_discipline, hours = total_hours,
    task1 = case_when(
      str_starts(task1_categorisation, "The results show evidence for the relationship") ~ "evidence for the claim",
      str_starts(task1_categorisation, "The results do not show") ~ "no evidence either way",
      str_detect(task1_categorisation, "null-hypothesis") ~ "evidence for the null",
      str_detect(task1_categorisation, "opposite") ~ "evidence for the opposite"
    ),
    direction = direction_of_result,
    statistic_type = reanalysis_type_of_statistic, statistic_recorded = reanalysis_statistic_report,
    df_recorded = reanalysis_degrees_of_freedom_1, n_recorded = reanalysis_model_sample_size,
    r_recorded = reanalysis_correlation_coef, d_recorded = reanalysis_cohens_d
  )

replies <- filter(audit, reply == 1, !is.na(named))
replies_hw <- filter(replies, group != "black") |> mutate(hisp = as.integer(group == "hispanic"))

# Average marginal effect of a Hispanic against a white name in a three-group logit.
ame_three_group <- function(fit, data) {
  p_h <- predict(fit, newdata = mutate(data, group = "hispanic"), type = "response")
  p_w <- predict(fit, newdata = mutate(data, group = "white"), type = "response")
  100 * mean(p_h - p_w)
}
three_group <- \(data) mutate(data, group = factor(group, levels = c("white", "black", "hispanic")))

# 8PTRZ: chi-square test of independence, Hispanic against all other names, replies
# only, with the continuity correction that chisq.test() applies to a 2 x 2 table
tab_8ptrz <- table(replies$group == "hispanic", replies$named)
x2_8ptrz <- unname(chisq.test(tab_8ptrz)$statistic)

# UYV3F: logit, Hispanic against white names, replies only, no covariates
fit_uyv3f <- glm(named ~ hisp, family = binomial(), data = replies_hw)
z_uyv3f <- unname(coef(summary(fit_uyv3f))["hisp", "z value"])

# EWFMI: logit with Hispanic names as the base group, the sex of the name, the
# poverty third entered as a number and the hard-to-find email (Stata ib2.race)
ewfmi_data <- replies |>
  filter(!is.na(poverty_third)) |>
  mutate(group = factor(group, levels = c("hispanic", "black", "white")),
         poverty_num = as.integer(factor(poverty_third, levels = c("lower", "middle", "top"))))
fit_ewfmi <- glm(named ~ group + name_sex + poverty_num + hard_email, family = binomial(), data = ewfmi_data)
z_ewfmi <- unname(coef(summary(fit_ewfmi))["groupwhite", "z value"])
pp_ewfmi <- 100 * mean(
  predict(fit_ewfmi, newdata = mutate(ewfmi_data, group = factor("hispanic", levels = levels(group))), type = "response") -
    predict(fit_ewfmi, newdata = mutate(ewfmi_data, group = factor("white", levels = levels(group))), type = "response")
)

# 25N5D: two-proportion test (prop.test, continuity correction) on all delivered
# emails, a non-reply counted as no greeting; the five uncoded replies stay in the
# denominators and so count as no greeting
hw_all <- filter(audit, !is.na(reply), group != "black")
counts_25n5d <- hw_all |>
  group_by(group) |>
  summarise(x = sum(reply == 1 & named %in% 1), n = n()) |>
  arrange(desc(group == "hispanic"))
x2_25n5d <- unname(prop.test(counts_25n5d$x, counts_25n5d$n)$statistic)

# P9W8Y: three-group logit on replies, an omnibus likelihood-ratio test of the
# three groups and Tukey-adjusted pairwise comparisons; Multi100 recorded no statistic
fit_p9w8y <- glm(named ~ group, family = binomial(), data = three_group(replies))
lr_p9w8y <- unname(anova(update(fit_p9w8y, . ~ 1), fit_p9w8y, test = "LRT")$Deviance[2])
z_p9w8y <- unname(coef(summary(fit_p9w8y))["grouphispanic", "z value"])

refits <- tribble(
  ~id,     ~comparison,                ~population,            ~method,                                                    ~statistic_refit, ~n_refit,           ~b_pp_refit,
  "8PTRZ", "Hispanic vs other names",  "replies",              "chi-square test of independence (continuity-corrected)",  x2_8ptrz,         nrow(replies),      fit_spec(audit, "nonhispanic", "replies", "drop", "lpm", "none")$b_pp,
  "UYV3F", "Hispanic vs white names",  "replies",              "logit, no covariates",                                     z_uyv3f,          nrow(replies_hw),   fit_spec(audit, "white", "replies", "drop", "logit", "none")$b_pp,
  "EWFMI", "Hispanic vs white names (three-group model)", "replies", "logit with name sex, poverty third (as a number) and hard-to-find email", z_ewfmi, nobs(fit_ewfmi), pp_ewfmi,
  "25N5D", "Hispanic vs white names",  "all delivered emails",  "two-proportion test (continuity-corrected)",               x2_25n5d,         sum(counts_25n5d$n), fit_spec(audit, "white", "all", "not_named", "lpm", "none")$b_pp,
  "P9W8Y", "three groups",             "replies",              "logit, omnibus likelihood-ratio test and Tukey-adjusted pairs", lr_p9w8y,   nrow(replies),      ame_three_group(fit_p9w8y, three_group(replies))
)

analysts <- m100 |>
  left_join(refits, by = "id") |>
  mutate(matches_record = abs(abs(statistic_refit) - abs(statistic_recorded)) < 0.0015 &
           n_refit == n_recorded)

stopifnot(nrow(analysts) == 5,
          sum(analysts$matches_record, na.rm = TRUE) == 4,
          is.na(analysts$statistic_recorded[analysts$id == "P9W8Y"]))

write_csv(analysts, "cases/housing/data/analysts5.csv", na = "")

# The published result: Table 1, model 3 (logit, N = 549), and the change in the
# predicted probability from a white to a Hispanic name, other variables at
# their means, -20.0 points [-30.5, -10.0] (accepted manuscript, pp. 18 and 37).
oa_blinded <- glm(named ~ group + black_third + hispanic_third + pop_third + poverty_third + hard_email,
                  family = binomial(), data = three_group(replies))

marks <- bind_rows(
  tibble(id = "OA", label = "Original authors, Table 1, model 3 (published)",
         statistic_type = "z", statistic = -0.82 / 0.22, n = 549, b_pp = -20.0, on_chart = TRUE,
         note = "Logit coefficient -0.82 (SE 0.22) with continuous 2012 ACS covariates; the change in predicted probability at the means is -20.0 points, 95% CI -30.5 to -10.0."),
  tibble(id = "OA_blinded", label = "Original authors' model refitted on the public file",
         statistic_type = "z", statistic = unname(coef(summary(oa_blinded))["grouphispanic", "z value"]),
         n = nobs(oa_blinded), b_pp = ame_three_group(oa_blinded, three_group(replies) |> filter(!is.na(pop_third))),
         on_chart = FALSE,
         note = "The covariates are the thirds that the public file holds in place of the continuous values, so the refit approximates the published model by design."),
  analysts |>
    transmute(id, label = id, statistic_type = coalesce(statistic_type, "LR chi-square (2 df)"),
              statistic = coalesce(statistic_recorded, statistic_refit), n = n_refit,
              b_pp = b_pp_refit, on_chart = TRUE,
              note = if_else(is.na(statistic_recorded),
                             "No statistic recorded by Multi100; placed by the course refit of the analysis.",
                             "Placed by the course refit of the analysis, which reproduces the recorded statistic."))
)

write_csv(marks, "cases/housing/data/reference_marks.csv", na = "")
