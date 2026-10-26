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
# Last updated: 2026-10-26
################################################################################

source("R/housing/_tools.R")

audit <- readr::read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)
PAPER <- "Einstein_AmJourPoliSci_2017_mxyQ"

m100 <- readr::read_csv("data-raw/housing/multi100/multi100_einstein_extract.csv", show_col_types = FALSE) |>
  datawizard::data_filter(paper_id == PAPER) |>
  datawizard::data_modify(task1 = dplyr::case_when(
    stringr::str_starts(task1_categorisation, "The results show evidence for the relationship") ~ "evidence for the claim",
    stringr::str_starts(task1_categorisation, "The results do not show") ~ "no evidence either way",
    stringr::str_detect(task1_categorisation, "null-hypothesis") ~ "evidence for the null",
    stringr::str_detect(task1_categorisation, "opposite") ~ "evidence for the opposite"
  )) |>
  datawizard::data_select(c(
    id = "analyst_id", discipline = "primary_discipline", hours = "total_hours", task1 = "task1",
    direction = "direction_of_result",
    statistic_type = "reanalysis_type_of_statistic", statistic_recorded = "reanalysis_statistic_report",
    df_recorded = "reanalysis_degrees_of_freedom_1", n_recorded = "reanalysis_model_sample_size",
    r_recorded = "reanalysis_correlation_coef", d_recorded = "reanalysis_cohens_d"
  ))

# The models of all three groups take the groups of names as a factor under
# another name, and data without the column `group`, which marginaleffects
# refuses as a column name.
replies <- audit |>
  datawizard::data_filter(reply == 1 & !is.na(named)) |>
  datawizard::data_modify(ethnicity = factor(group, levels = c("white", "black", "hispanic")))
replies3 <- datawizard::data_select(replies, exclude = "group")
replies_hw <- replies |>
  datawizard::data_filter(group != "black") |>
  datawizard::data_modify(hisp = as.integer(group == "hispanic"))

# The average marginal effect of a Hispanic against a white name in a model of
# all three groups: the mean over the replies of the predicted probability
# with a Hispanic name minus that with a white name (marginaleffects;
# Arel-Bundock, Greifer and Heiss 2024, Journal of Statistical Software).
HISP_VS_WHITE <- list(ethnicity = c("white", "hispanic"))

# 8PTRZ: chi-square test of independence, Hispanic against all other names, replies
# only, with the continuity correction that chisq.test() applies to a 2 x 2 table
tab_8ptrz <- stats::xtabs(~ I(group == "hispanic") + named, data = replies)  # r-style: keep – the contingency table that chisq.test() takes
x2_8ptrz <- unname(stats::chisq.test(tab_8ptrz)$statistic)

# UYV3F: logit, Hispanic against white names, replies only, no covariates
fit_uyv3f <- stats::glm(named ~ hisp, family = stats::binomial(), data = replies_hw)
z_uyv3f <- parameters::model_parameters(fit_uyv3f, ci_method = "wald") |>
  datawizard::data_filter(Parameter == "hisp") |>
  datawizard::data_extract("z")

# EWFMI: logit with Hispanic names as the base group, the sex of the name, the
# poverty third entered as a number and the hard-to-find email (Stata ib2.race)
ewfmi_data <- replies |>
  datawizard::data_filter(!is.na(poverty_third)) |>
  datawizard::data_modify(
    ethnicity = factor(group, levels = c("hispanic", "black", "white")),
    poverty_num = as.integer(factor(poverty_third, levels = c("lower", "middle", "top")))
  ) |>
  datawizard::data_select(exclude = "group")
fit_ewfmi <- stats::glm(named ~ ethnicity + name_sex + poverty_num + hard_email,
                        family = stats::binomial(), data = ewfmi_data)
z_ewfmi <- parameters::model_parameters(fit_ewfmi, ci_method = "wald") |>
  datawizard::data_filter(Parameter == "ethnicitywhite") |>
  datawizard::data_extract("z")
pp_ewfmi <- 100 * marginaleffects::avg_comparisons(fit_ewfmi, variables = HISP_VS_WHITE)$estimate

# 25N5D: two-proportion test (prop.test, continuity correction) on all delivered
# emails, a non-reply counted as no greeting; the five uncoded replies stay in the
# denominators and so count as no greeting. data_summary() orders the groups
# alphabetically, so the Hispanic names come first.
counts_25n5d <- audit |>
  datawizard::data_filter(!is.na(reply) & group != "black") |>
  datawizard::data_summary(greeted = sum(reply == 1 & named %in% 1), emails = n(), by = "group")
x2_25n5d <- unname(stats::prop.test(counts_25n5d$greeted, counts_25n5d$emails)$statistic)

# P9W8Y: three-group logit on replies, an omnibus likelihood-ratio test of the
# three groups and Tukey-adjusted pairwise comparisons; Multi100 recorded no statistic
fit_p9w8y <- stats::glm(named ~ ethnicity, family = stats::binomial(), data = replies3)
lr_p9w8y <- unname(stats::anova(stats::update(fit_p9w8y, . ~ 1), fit_p9w8y, test = "LRT")$Deviance[2])

refits <- tibble::tribble(
  ~id,     ~comparison,                ~population,            ~method,                                                    ~statistic_refit, ~n_refit,           ~b_pp_refit,
  "8PTRZ", "Hispanic vs other names",  "replies",              "chi-square test of independence (continuity-corrected)",  x2_8ptrz,         nrow(replies),      fit_spec(audit, "nonhispanic", "replies", "drop", "lpm", "none")$b_pp,
  "UYV3F", "Hispanic vs white names",  "replies",              "logit, no covariates",                                     z_uyv3f,          nrow(replies_hw),   fit_spec(audit, "white", "replies", "drop", "logit", "none")$b_pp,
  "EWFMI", "Hispanic vs white names (three-group model)", "replies", "logit with name sex, poverty third (as a number) and hard-to-find email", z_ewfmi, stats::nobs(fit_ewfmi), pp_ewfmi,
  "25N5D", "Hispanic vs white names",  "all delivered emails",  "two-proportion test (continuity-corrected)",               x2_25n5d,         sum(counts_25n5d$emails), fit_spec(audit, "white", "all", "not_named", "lpm", "none")$b_pp,
  "P9W8Y", "three groups",             "replies",              "logit, omnibus likelihood-ratio test and Tukey-adjusted pairs", lr_p9w8y,   nrow(replies),      100 * marginaleffects::avg_comparisons(fit_p9w8y, variables = HISP_VS_WHITE)$estimate
)

analysts <- m100 |>
  datawizard::data_join(refits, join = "left", by = "id") |>
  datawizard::data_modify(matches_record = abs(abs(statistic_refit) - abs(statistic_recorded)) < 0.0015 &
                            n_refit == n_recorded)

stopifnot(nrow(analysts) == 5,
          sum(analysts$matches_record, na.rm = TRUE) == 4,
          is.na(analysts$statistic_recorded[analysts$id == "P9W8Y"]))

readr::write_csv(analysts, "cases/housing/data/analysts5.csv", na = "")  # r-style: keep – datawizard::data_write() writes other bytes

# The published result: Table 1, model 3 (logit, N = 549), and the change in the
# predicted probability from a white to a Hispanic name, other variables at
# their means, -20.0 points [-30.5, -10.0] (accepted manuscript, pp. 18 and 37).
oa_blinded <- stats::glm(named ~ ethnicity + black_third + hispanic_third + pop_third + poverty_third + hard_email,
                         family = stats::binomial(), data = replies3)
z_oa_blinded <- parameters::model_parameters(oa_blinded, ci_method = "wald") |>
  datawizard::data_filter(Parameter == "ethnicityhispanic") |>
  datawizard::data_extract("z")

marks <- dplyr::bind_rows(
  tibble::tibble(id = "OA", label = "Original authors, Table 1, model 3 (published)",
                 statistic_type = "z", statistic = -0.82 / 0.22, n = 549, b_pp = -20.0, on_chart = TRUE,
                 note = "Logit coefficient -0.82 (SE 0.22) with continuous 2012 ACS covariates; the change in predicted probability at the means is -20.0 points, 95% CI -30.5 to -10.0."),
  tibble::tibble(id = "OA_blinded", label = "Original authors' model refitted on the public file",
                 statistic_type = "z", statistic = z_oa_blinded,
                 n = stats::nobs(oa_blinded),
                 b_pp = 100 * marginaleffects::avg_comparisons(oa_blinded, variables = HISP_VS_WHITE)$estimate,
                 on_chart = FALSE,
                 note = "The covariates are the thirds that the public file holds in place of the continuous values, so the refit approximates the published model by design."),
  analysts |>
    dplyr::transmute(id, label = id, statistic_type = dplyr::coalesce(statistic_type, "LR chi-square (2 df)"),
                     statistic = dplyr::coalesce(statistic_recorded, statistic_refit), n = n_refit,
                     b_pp = b_pp_refit, on_chart = TRUE,
                     note = dplyr::if_else(is.na(statistic_recorded),
                                           "No statistic recorded by Multi100; placed by the course refit of the analysis.",
                                           "Placed by the course refit of the analysis, which reproduces the recorded statistic."))
)

readr::write_csv(marks, "cases/housing/data/reference_marks.csv", na = "")  # r-style: keep – datawizard::data_write() writes other bytes
