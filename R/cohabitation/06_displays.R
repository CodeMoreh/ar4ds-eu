################################################################################
# Title:        06_displays.R
# Purpose:      The teaching displays beside the curve: the life table of the
#               two cohorts (overall, by education and by race-ethnicity), the
#               shares that married or separated within three years, the change
#               for each group in the interaction models of the paper, the
#               off-curve options at the reference settings, the rules of
#               inference, and the cohorts of the SCORE file against the rebuild;
#               and the tables from which the browser lab reads Task 2 and the
#               parts of Task 3 that need the union file.
# Reads:        cases/cohabitation/data/cohab_unions.csv,
#               cases/cohabitation/data/score_person_months.csv (the person-month
#               file of SCORE, checked by stage 04)
# Writes:       cases/cohabitation/data/display_lifetable.csv,
#               cases/cohabitation/data/display_shares.csv,
#               cases/cohabitation/data/display_subgroups.csv,
#               cases/cohabitation/data/display_options.csv,
#               cases/cohabitation/data/display_inference.csv,
#               cases/cohabitation/data/display_score_file.csv, and lab_cycles.csv,
#               lab_outcomes.csv, lab_composition.csv, lab_example.csv,
#               lab_months.csv, lab_married.csv, lab_score_check.csv and
#               lab_repaired.csv in cases/cohabitation/data/
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/cohabitation/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" and
# the codes of the cases, strata and clusters as numbers
unions <- readr::read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                          col_types = readr::cols(caseid = readr::col_character(),
                                                  stratum = readr::col_character(),
                                                  psu = readr::col_character()))

# The unions of the paper with the time and the outcome that the life table
# reads: a union is at risk for `duration + 1` months, the month of moving in
# being month 1, and ends in a marriage, a separation or (intact) censoring in
# the last of them, as in the person-month file
paper <- spec_unions(unions) |>
  datawizard::data_modify(cohort = dplyr::if_else(later == 1, "2005-2010", "1990-1995"),
                          month = duration + 1,
                          status = factor(outcome, levels = c("intact", "marriage", "separation")))

groups <- dplyr::bind_rows(
  paper |> datawizard::data_modify(group_type = "all", group = "all"),
  paper |> datawizard::data_modify(group_type = "education", group = educ),
  paper |> datawizard::data_modify(group_type = "race", group = race)
)

# The multi-decrement life table: in every month of the union the weighted
# share of the unions still intact at its start that married or separated in
# it, and the cumulative shares that had married, separated or were still
# cohabiting by its end (separation competes with marriage, so the shares sum
# to one). survival::survfit() with a status whose first level is censoring
# gives the Aalen-Johansen estimator of the three shares (Aalen and Johansen
# 1978, Scandinavian Journal of Statistics; Therneau and Grambsch 2000), with
# the survey weights; a second fit without them counts the unions.
lifetable <- groups |>
  dplyr::group_by(cohort, group_type, group) |>
  dplyr::group_modify(\(u, key) {
    months <- seq_len(max(u$month))
    weighted <- survival::survfit(survival::Surv(month, status) ~ 1, data = u, weights = weight) |>
      summary(times = months)
    counted <- survival::survfit(survival::Surv(month, status) ~ 1, data = u) |>
      summary(times = months)
    tibble::tibble(month = months,
                   at_risk = counted$n.risk[, 1], w_at_risk = weighted$n.risk[, 1],
                   marriages = counted$n.event[, 2], separations = counted$n.event[, 3],
                   h_marriage = weighted$n.event[, 2] / weighted$n.risk[, 1],
                   h_separation = weighted$n.event[, 3] / weighted$n.risk[, 1],
                   cohabiting = weighted$pstate[, 1], married = weighted$pstate[, 2],
                   separated = weighted$pstate[, 3])
  }) |>
  dplyr::ungroup()

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lifetable, "cases/cohabitation/data/display_lifetable.csv")

# Within three years (by the end of month 36) from the life table, beside the
# share of unions that had become a marriage by the interview, which ignores
# how long each union was observed
shares <- lifetable |>
  datawizard::data_filter(month == 36) |>
  datawizard::data_select(c("cohort", "group_type", "group", married_3y = "married", separated_3y = "separated",
                            cohabiting_3y = "cohabiting")) |>
  dplyr::left_join(groups |>
                     datawizard::data_summary(unions = n(), months_mean = mean(duration + 1),
                                              married_by_interview = stats::weighted.mean(outcome == "marriage", weight),
                                              by = c("cohort", "group_type", "group")),
                   by = c("cohort", "group_type", "group"))

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(shares, "cases/cohabitation/data/display_shares.csv")

# The change for each group in the interaction models of the paper (Model 1:
# education x period; Model 2: race-ethnicity x period): the coefficient of the
# later cohort plus that of its interaction with the group, with the
# design-based error of the sum from survey::svycontrast() (Lumley 2004), t
# on the df of the model and its 95% interval. VGAM names the coefficients of
# the marriage equation `:1`.
group_changes <- function(adjust, groups_of) {
  m <- fit_model(unions, adjust = adjust)
  purrr::imap(groups_of, \(term, group) {
    terms <- paste0(c("later", if (!is.na(term)) term), ":1")
    change <- survey::svycontrast(m$fit, stats::setNames(rep(1, length(terms)), terms))
    b <- stats::coef(change)[[1]]
    se <- survey::SE(change)[[1]]
    ci <- stats::confint(change, df = m$df)
    tibble::tibble(model = adjust, group = group, b = b, se = se, t = b / se, df = m$df,
                   p = 2 * stats::pt(-abs(b / se), m$df), ci_low = ci[1, 1], ci_high = ci[1, 2])
  }) |>
    dplyr::bind_rows()
}

subgroups <- dplyr::bind_rows(
  group_changes("educ_period", c(college = NA, some_college = "later_some_college", hs = "later_hs",
                                 less_hs = "later_less_hs")),
  group_changes("race_period", c(white = NA, black = "later_black", hispanic = "later_hispanic",
                                 other = "later_other"))
)

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(subgroups, "cases/cohabitation/data/display_subgroups.csv")

# Options off the curve, each fitted once at the reference settings
options <- list(
  list(adjust = "race_period"), list(adjust = "educ_period"), list(adjust = "total_duration"),
  list(time = "none"), list(model = "lpm"), list(education = "score"), list(short = "keep"),
  list(comparison = "2002_2006"), list(comparison = "1995_2011"), list(comparison = "2002_2011"),
  list(comparison = "2006_2011")
) |>
  purrr::map(\(opt) purrr::exec(fit_spec, unions = unions, !!!opt) |>
               dplyr::mutate(changed = names(opt), option = opt[[1]], .before = 1)) |>
  dplyr::bind_rows()

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(options, "cases/cohabitation/data/display_options.csv")

inference <- c("design", "woman", "model") |>
  purrr::map(\(e) fit_spec(unions, errors = e)) |>
  dplyr::bind_rows()

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(inference, "cases/cohabitation/data/display_inference.csv")

# The cohorts of the person-month file of SCORE against the rebuild: unions,
# outcomes and person-months by cycle. dplyr for the first summary, because
# datawizard::data_summary() turns every column into text once one summary is
# text.
score <- readr::read_csv("cases/cohabitation/data/score_person_months.csv", show_col_types = FALSE) |>
  datawizard::data_modify(cycle = dplyr::case_when(P95 == 1 ~ "1995", P02 == 1 ~ "2002", P10 == 1 ~ "2006-10",
                                                   P13 == 1 ~ "2011-13", P15 == 1 ~ "2013-15"))
score_cycles <- score |>
  dplyr::summarise(months = dplyr::n(),
                   outcome = c("intact", "marriage", "separation")[OUTCOME[which.max(MONTHNUM)]],
                   .by = c(cycle, CASEID)) |>
  datawizard::data_summary(unions = n(), marriages = sum(outcome == "marriage"),
                           separations = sum(outcome == "separation"), intact = sum(outcome == "intact"),
                           person_months = sum(months), by = "cycle") |>
  datawizard::data_modify(source = "SCORE file") |>
  datawizard::data_relocate("source")
rebuilt_cycles <- unions |>
  datawizard::data_filter(in_window, cycle != "2006-10" | from_2005) |>
  datawizard::data_summary(unions = n(), marriages = sum(outcome == "marriage"),
                           separations = sum(outcome == "separation"), intact = sum(outcome == "intact"),
                           person_months = sum(duration + 1), by = "cycle") |>
  datawizard::data_modify(source = "rebuilt") |>
  datawizard::data_relocate("source")

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(dplyr::bind_rows(score_cycles, rebuilt_cycles), "cases/cohabitation/data/display_score_file.csv")

# ---- The tables of the browser lab ---------------------------------------------
# The browser lab of the case fits nothing: every number of its Task 2, and of
# the two parts of Task 3 that need the union file, is read from the tables
# below, each the object that the code of the workspace report produces, run
# here on the same files with the same code, so that the lab and the workspace
# give the same numbers. The lab reproduces 5X3KG and counts the unions of the
# person-month file live, on the file itself.

# The unions of each survey, and the unions of the paper by how they ended
lab_cycles <- datawizard::data_summary(unions, unions = n(), by = "cycle")
lab_paper_unions <- spec_unions(unions)
lab_outcomes <- datawizard::data_summary(lab_paper_unions, unions = n(), by = c("cycle", "outcome"))

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_cycles, "cases/cohabitation/data/lab_cycles.csv")
# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_outcomes, "cases/cohabitation/data/lab_outcomes.csv")

# Weighted shares of Hispanic and Black women and of graduates, and the mean
# age at which the couple moved in
lab_composition <- lab_paper_unions |>
  datawizard::data_summary(unions    = n(),
                           hispanic  = round(100 * stats::weighted.mean(race == "hispanic", weight), 1),
                           black     = round(100 * stats::weighted.mean(race == "black", weight), 1),
                           college   = round(100 * stats::weighted.mean(educ == "college", weight), 1),
                           mean_age  = round(stats::weighted.mean(age_start, weight), 1),
                           by = "cycle")

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_composition, "cases/cohabitation/data/lab_composition.csv")

# The months of one union of 1995 that became a marriage in its third month,
# and every month of every union of the paper by how it ended
lab_example <- lab_paper_unions |>
  datawizard::data_filter(cycle == "1995", outcome == "marriage", duration == 2) |>
  utils::head(1) |>
  person_months() |>
  datawizard::data_select(c("cycle", "caseid", "month", "y"))
lab_months <- spec_frame(unions) |>
  datawizard::data_summary(months = n(), by = "y")

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_example, "cases/cohabitation/data/lab_example.csv")
# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_months, "cases/cohabitation/data/lab_months.csv")

# The share of each cohort that had married by the end of each month, from the
# Aalen-Johansen estimator of survival::survfit() with marriage and separation
# as competing outcomes and an intact union censored at the interview
# (Therneau and Grambsch 2000): the data of the figure of Task 2
lab_life <- lab_paper_unions |>
  datawizard::data_modify(months = duration + 1,
                          ended = factor(outcome, levels = c("intact", "marriage", "separation")),
                          cohort = factor(later, levels = 0:1, labels = c("begun 1990–1995", "begun 2005–2010")))
lab_fit <- survival::survfit(survival::Surv(months, ended) ~ cohort, data = lab_life, weights = weight)
lab_steps <- summary(lab_fit, times = 1:61)
lab_married <- tibble::tibble(cohort = sub("cohort=", "", lab_steps$strata), month = lab_steps$time,
                              married = lab_steps$pstate[, lab_fit$states == "marriage"])

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_married, "cases/cohabitation/data/lab_married.csv")

# Task 3: which of the 929 unions of 1995 in the window of the paper are in the
# person-month file, one row per union of the file taken from its last month
lab_score_unions <- score |>
  datawizard::data_filter(P95 == 1 | P10 == 1) |>
  dplyr::slice_max(MONTHNUM, n = 1, by = CASEID) |>
  datawizard::data_modify(survey = dplyr::if_else(P95 == 1, "1995", "2006-2010"),
                          outcome = c("intact", "marriage", "separation")[OUTCOME])
lab_score_check <- lab_paper_unions |>
  datawizard::data_filter(cycle == "1995") |>
  datawizard::data_modify(in_score_file = caseid %in% as.character(lab_score_unions$CASEID)) |>
  datawizard::data_summary(unions = n(), by = c("outcome", "in_score_file"))

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_score_check, "cases/cohabitation/data/lab_score_check.csv")

# Task 3, the optional extension: the months of the file in its own
# conventions with the 1995 rows rebuilt from the 929 unions of the paper (an
# intact union contributes as many months as it lasted and a union that ended
# one more, with its outcome in the last), beside the months of 2006-2010 from
# the file, and how many of each record a marriage; the lab runs the test of
# two proportions on these counts
lab_1995 <- lab_paper_unions |>
  datawizard::data_filter(cycle == "1995") |>
  datawizard::data_modify(MONTHNUM = duration + (outcome != "intact"),
                          ended = factor(outcome, levels = c("intact", "marriage", "separation")))
lab_rebuilt <- lab_1995 |>
  survival::survSplit(cut = seq_len(max(lab_1995$MONTHNUM) - 1), end = "MONTHNUM", event = "ended") |>
  datawizard::data_modify(OUTCOME = as.integer(ended), P95 = 1, P10 = 0) |>
  datawizard::data_select(c("OUTCOME", "P95", "P10"))
lab_rows_2010 <- score |>
  datawizard::data_filter(P10 == 1) |>
  datawizard::data_select(c("OUTCOME", "P95", "P10"))
lab_repaired <- dplyr::bind_rows(lab_rebuilt, lab_rows_2010)
lab_repaired_counts <- tibble::tibble(
  survey = c("1995 (rebuilt)", "2006-2010"),
  marriages = c(sum(lab_repaired$P95 == 1 & lab_repaired$OUTCOME == 2),
                sum(lab_repaired$P10 == 1 & lab_repaired$OUTCOME == 2)),
  months = c(sum(lab_repaired$P95), sum(lab_repaired$P10))
)

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(lab_repaired_counts, "cases/cohabitation/data/lab_repaired.csv")
