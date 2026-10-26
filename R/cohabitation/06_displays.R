################################################################################
# Title:        06_displays.R
# Purpose:      The teaching displays beside the curve: the life table of the
#               two cohorts (overall, by education and by race-ethnicity), the
#               shares that married or separated within three years, the change
#               for each group in the interaction models of the paper, the
#               off-curve options at the reference settings, the rules of
#               inference, and the cohorts of the SCORE file against the rebuild.
# Reads:        cases/cohabitation/data/cohab_unions.csv,
#               cases/cohabitation/data/score_person_months.csv (the person-month
#               file of SCORE, checked by stage 04)
# Writes:       cases/cohabitation/data/display_lifetable.csv,
#               cases/cohabitation/data/display_shares.csv,
#               cases/cohabitation/data/display_subgroups.csv,
#               cases/cohabitation/data/display_options.csv,
#               cases/cohabitation/data/display_inference.csv,
#               cases/cohabitation/data/display_score_file.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

library(tidyverse)
source("R/cohabitation/_tools.R")

unions <- read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                   col_types = cols(caseid = col_character(), stratum = col_character(), psu = col_character()))
paper <- spec_unions(unions) |>
  mutate(cohort = if_else(later == 1, "2005-2010", "1990-1995"))

# The multi-decrement life table: in every month of the union the weighted
# share of the unions still intact at its start that married or separated in
# it, and the cumulative shares that had married, separated or were still
# cohabiting by its end (separation competes with marriage, so the shares sum
# to one). Months are counted as in the person-month file, the month of moving
# in being month 1.
life_table <- function(u) {
  u |>
    person_months() |>
    group_by(month) |>
    summarise(at_risk = n(), w_at_risk = sum(weight),
              marriages = sum(y == "marriage"), separations = sum(y == "separation"),
              h_marriage = sum(weight * (y == "marriage")) / w_at_risk,
              h_separation = sum(weight * (y == "separation")) / w_at_risk, .groups = "drop") |>
    mutate(cohabiting = cumprod(1 - h_marriage - h_separation),
           cohabiting_start = lag(cohabiting, default = 1),
           married = cumsum(cohabiting_start * h_marriage),
           separated = cumsum(cohabiting_start * h_separation)) |>
    select(-cohabiting_start)
}

groups <- bind_rows(
  paper |> mutate(group_type = "all", group = "all"),
  paper |> mutate(group_type = "education", group = educ),
  paper |> mutate(group_type = "race", group = race)
)

lifetable <- groups |>
  group_by(cohort, group_type, group) |>
  group_modify(\(u, key) life_table(u)) |>
  ungroup()

write_csv(lifetable, "cases/cohabitation/data/display_lifetable.csv")

# Within three years (by the end of month 36) from the life table, beside the
# share of unions that had become a marriage by the interview, which ignores
# how long each union was observed
shares <- lifetable |>
  filter(month == 36) |>
  select(cohort, group_type, group, married_3y = married, separated_3y = separated,
         cohabiting_3y = cohabiting) |>
  left_join(groups |>
              group_by(cohort, group_type, group) |>
              summarise(unions = n(), months_mean = mean(duration + 1),
                        married_by_interview = weighted.mean(outcome == "marriage", weight), .groups = "drop"),
            by = c("cohort", "group_type", "group"))

write_csv(shares, "cases/cohabitation/data/display_shares.csv")

# The change for each group in the interaction models of the paper (Model 1:
# education x period; Model 2: race-ethnicity x period): the coefficient of the
# later cohort plus that of its interaction with the group, with design-based
# errors of the sum
group_changes <- function(adjust, groups_of) {
  m <- fit_model(unions, adjust = adjust)
  imap(groups_of, \(term, group) {
    terms <- c("later", if (!is.na(term)) term)
    b <- sum(m$coef[terms, "marriage"])
    se <- sqrt(sum(m$vcov[paste0("marriage:", terms), paste0("marriage:", terms)]))
    tibble(model = adjust, group = group, b = b, se = se, t = b / se, df = m$df, p = 2 * pt(-abs(b / se), m$df))
  }) |>
    bind_rows()
}

subgroups <- bind_rows(
  group_changes("educ_period", c(college = NA, some_college = "later_some_college", hs = "later_hs",
                                 less_hs = "later_less_hs")),
  group_changes("race_period", c(white = NA, black = "later_black", hispanic = "later_hispanic",
                                 other = "later_other"))
)

write_csv(subgroups, "cases/cohabitation/data/display_subgroups.csv")

# Options off the curve, each fitted once at the reference settings
options <- list(
  list(adjust = "race_period"), list(adjust = "educ_period"), list(adjust = "total_duration"),
  list(time = "none"), list(model = "lpm"), list(education = "score"), list(short = "keep"),
  list(comparison = "2002_2006"), list(comparison = "1995_2011"), list(comparison = "2002_2011"),
  list(comparison = "2006_2011")
) |>
  map(\(opt) do.call(fit_spec, c(list(unions = unions), opt)) |>
        mutate(changed = names(opt), option = opt[[1]], .before = 1)) |>
  bind_rows()

write_csv(options, "cases/cohabitation/data/display_options.csv")

inference <- c("design", "woman", "model") |>
  map(\(e) fit_spec(unions, errors = e)) |>
  bind_rows()

write_csv(inference, "cases/cohabitation/data/display_inference.csv")

# The cohorts of the person-month file of SCORE against the rebuild: unions,
# outcomes and person-months by cycle
score <- read_csv("cases/cohabitation/data/score_person_months.csv", show_col_types = FALSE) |>
  mutate(cycle = case_when(P95 == 1 ~ "1995", P02 == 1 ~ "2002", P10 == 1 ~ "2006-10",
                           P13 == 1 ~ "2011-13", P15 == 1 ~ "2013-15"))
score_cycles <- score |>
  group_by(cycle, CASEID) |>
  summarise(months = n(), outcome = c("intact", "marriage", "separation")[OUTCOME[which.max(MONTHNUM)]],
            .groups = "drop") |>
  group_by(cycle) |>
  summarise(unions = n(), marriages = sum(outcome == "marriage"), separations = sum(outcome == "separation"),
            intact = sum(outcome == "intact"), person_months = sum(months), .groups = "drop") |>
  mutate(source = "SCORE file", .before = 1)
rebuilt_cycles <- unions |>
  filter(in_window, cycle != "2006-10" | from_2005) |>
  group_by(cycle) |>
  summarise(unions = n(), marriages = sum(outcome == "marriage"), separations = sum(outcome == "separation"),
            intact = sum(outcome == "intact"), person_months = sum(duration + 1), .groups = "drop") |>
  mutate(source = "rebuilt", .before = 1)

write_csv(bind_rows(score_cycles, rebuilt_cycles), "cases/cohabitation/data/display_score_file.csv")
