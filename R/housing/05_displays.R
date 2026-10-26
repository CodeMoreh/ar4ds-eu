################################################################################
# Title:        05_displays.R
# Purpose:      The teaching displays that sit beside the curve: the six names,
#               the other outcomes, the subgroups that the curve leaves out, the
#               randomisation distribution of the reference specification, and
#               the power of the original and the replication designs.
# Reads:        cases/housing/data/housing_audit.csv
# Writes:       cases/housing/data/display_names.csv, cases/housing/data/display_outcomes.csv,
#               cases/housing/data/display_subgroups.csv, cases/housing/data/display_ri.csv,
#               cases/housing/data/display_power.csv
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
source("R/housing/_tools.R")

audit <- read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)

# Wilson interval in percent, which stays inside 0-100 for small groups
wilson <- function(x, n, side) {
  z <- qnorm(0.975)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  100 * if (side == "lower") centre - half else centre + half
}

summarise_names <- function(data) {
  data |>
    summarise(
      sent = n(),
      bounced = sum(is.na(reply)),
      delivered = sum(!is.na(reply)),
      replies = sum(reply == 1, na.rm = TRUE),
      coded = sum(reply == 1 & !is.na(named), na.rm = TRUE),
      named = sum(named == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      reply_pct = 100 * replies / delivered,
      reply_lo = wilson(replies, delivered, "lower"), reply_hi = wilson(replies, delivered, "upper"),
      named_pct = 100 * named / coded,
      named_lo = wilson(named, coded, "lower"), named_hi = wilson(named, coded, "upper"),
      named_all_pct = 100 * named / delivered
    )
}

names_tab <- bind_rows(
  audit |> group_by(level = "name", name, group, name_sex) |> summarise_names(),
  audit |> group_by(level = "group", group) |> summarise_names() |> mutate(name = group)
)
write_csv(names_tab, "cases/housing/data/display_names.csv")

# Each outcome as a difference in percentage points from white names, HC2 errors
diff_from_white <- function(data, outcome, contrast) {
  frame <- data |>
    filter(group %in% c("white", contrast), !is.na(.data[[outcome]])) |>
    mutate(hisp = as.integer(group == contrast), y = .data[[outcome]])
  fit <- lm(y ~ hisp, data = frame)
  b <- unname(coef(fit)["hisp"])
  se <- sqrt(vcov_hc2(fit)["hisp", "hisp"])
  tibble(outcome = outcome, contrast = contrast, b_pp = 100 * b, se_pp = 100 * se,
         t = b / se, df = fit$df.residual, p = 2 * pt(-abs(b / se), fit$df.residual), n = nrow(frame))
}

outcomes_data <- audit |>
  filter(!is.na(reply)) |>
  mutate(named_any = if_else(reply == 0, 0, named))

outcomes <- expand_grid(outcome = c("reply", "reply24", "named", "named_any"),
                        contrast = c("hispanic", "black")) |>
  pmap(\(outcome, contrast) diff_from_white(outcomes_data, outcome, contrast)) |>
  bind_rows()
write_csv(outcomes, "cases/housing/data/display_outcomes.csv")

# Subgroups that the curve leaves out because each is a different question
replies <- filter(audit, reply == 1, !is.na(named))
subgroup <- function(data, label) {
  diff_from_white(data, "named", "hispanic") |> mutate(subgroup = label, .before = 1)
}
subgroups <- bind_rows(
  subgroup(replies, "all names"),
  subgroup(filter(replies, name_sex == "male"), "male names (Santiago against Brett)"),
  subgroup(filter(replies, name_sex == "female"), "female names (Gabriela against Emily)"),
  subgroup(filter(replies, hispanic_official == 1), "replies from officials with Hispanic names"),
  subgroup(filter(replies, hispanic_official == 0), "replies from other officials"),
  subgroup(filter(replies, hispanic_third == "lower"), "communities in the lower third of percent Hispanic"),
  subgroup(filter(replies, hispanic_third == "middle"), "communities in the middle third of percent Hispanic"),
  subgroup(filter(replies, hispanic_third == "top"), "communities in the top third of percent Hispanic")
)
write_csv(subgroups, "cases/housing/data/display_subgroups.csv")

# Randomisation distribution of the reference specification: the names shuffled
# among the white and Hispanic replies, 4,999 times
set.seed(20261025)
ref_frame <- spec_frame(audit, "white", "replies", "drop")
ri <- tibble(draw = 1:4999) |>
  mutate(b_pp = map_dbl(draw, \(i) {
    h <- sample(ref_frame$hisp)
    100 * (mean(ref_frame$y[h == 1]) - mean(ref_frame$y[h == 0]))
  }))
write_csv(ri, "cases/housing/data/display_ri.csv")

# Power of a two-sided test at the 5% level for a gap of a given size from a
# white rate of 61%, at the group sizes of the original (replies, Hispanic
# against white) and of the replication (738 replies in two equal groups)
power <- expand_grid(design = c("original (175 and 187 replies)", "replication (369 and 369 replies)"),
                     gap_pp = seq(2, 25, by = 1)) |>
  mutate(n_per_group = if_else(str_starts(design, "original"), 181, 369),
         power = map2_dbl(gap_pp, n_per_group, \(g, n)
                          power.prop.test(n = n, p1 = 0.61, p2 = 0.61 - g / 100)$power))
write_csv(power, "cases/housing/data/display_power.csv")
