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
# Last updated: 2026-10-26
################################################################################

source("R/housing/_tools.R")

audit <- readr::read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)

# Counts and shares of one set of emails, with 95% Wilson intervals in percent,
# which stay inside 0-100 for small groups: the score interval of
# stats::prop.test() without the continuity correction (Wilson 1927; Newcombe
# 1998, Statistics in Medicine). The verbs are those of dplyr, because the data
# arrive grouped by the caller.
summarise_names <- function(data) {
  data |>
    dplyr::summarise(
      sent = dplyr::n(),
      bounced = sum(is.na(reply)),
      delivered = sum(!is.na(reply)),
      replies = sum(reply == 1, na.rm = TRUE),
      coded = sum(reply == 1 & !is.na(named), na.rm = TRUE),
      named = sum(named == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      reply_pct = 100 * replies / delivered,
      reply_lo = purrr::map2_dbl(replies, delivered, \(x, n) 100 * stats::prop.test(x, n, correct = FALSE)$conf.int[1]),
      reply_hi = purrr::map2_dbl(replies, delivered, \(x, n) 100 * stats::prop.test(x, n, correct = FALSE)$conf.int[2]),
      named_pct = 100 * named / coded,
      named_lo = purrr::map2_dbl(named, coded, \(x, n) 100 * stats::prop.test(x, n, correct = FALSE)$conf.int[1]),
      named_hi = purrr::map2_dbl(named, coded, \(x, n) 100 * stats::prop.test(x, n, correct = FALSE)$conf.int[2]),
      named_all_pct = 100 * named / delivered
    )
}

names_tab <- dplyr::bind_rows(
  audit |> dplyr::group_by(level = "name", name, group, name_sex) |> summarise_names(),
  audit |> dplyr::group_by(level = "group", group) |> summarise_names() |> dplyr::mutate(name = group)
)
readr::write_csv(names_tab, "cases/housing/data/display_names.csv")  # r-style: keep – datawizard::data_write() writes other bytes

# Each outcome as a difference in percentage points from white names, as a
# linear probability model with HC2 errors from parameters and sandwich, and
# its 95% interval
diff_from_white <- function(data, outcome, contrast) {
  frame <- data |>
    dplyr::filter(group %in% c("white", contrast), !is.na(.data[[outcome]])) |>
    dplyr::mutate(hisp = as.integer(group == contrast), y = .data[[outcome]])
  fit <- stats::lm(y ~ hisp, data = frame)
  est <- parameters::model_parameters(fit, vcov = "HC2") |>
    datawizard::data_filter(Parameter == "hisp")
  tibble::tibble(outcome = outcome, contrast = contrast, b_pp = 100 * est$Coefficient,
                 se_pp = 100 * est$SE, t = est$t, df = est$df_error, p = est$p, n = nrow(frame),
                 ci_low = 100 * est$CI_low, ci_high = 100 * est$CI_high)
}

outcomes_data <- audit |>
  datawizard::data_filter(!is.na(reply)) |>
  datawizard::data_modify(named_any = dplyr::if_else(reply == 0, 0, named))

outcomes <- tidyr::expand_grid(outcome = c("reply", "reply24", "named", "named_any"),
                               contrast = c("hispanic", "black")) |>
  purrr::pmap(\(outcome, contrast) diff_from_white(outcomes_data, outcome, contrast)) |>
  dplyr::bind_rows()
readr::write_csv(outcomes, "cases/housing/data/display_outcomes.csv")  # r-style: keep – datawizard::data_write() writes other bytes

# Subgroups that the curve leaves out because each is a different question
replies <- datawizard::data_filter(audit, reply == 1 & !is.na(named))
subgroup <- function(data, label) {
  diff_from_white(data, "named", "hispanic") |> dplyr::mutate(subgroup = label, .before = 1)
}
subgroups <- dplyr::bind_rows(
  subgroup(replies, "all names"),
  subgroup(datawizard::data_filter(replies, name_sex == "male"), "male names (Santiago against Brett)"),
  subgroup(datawizard::data_filter(replies, name_sex == "female"), "female names (Gabriela against Emily)"),
  subgroup(datawizard::data_filter(replies, hispanic_official == 1), "replies from officials with Hispanic names"),
  subgroup(datawizard::data_filter(replies, hispanic_official == 0), "replies from other officials"),
  subgroup(datawizard::data_filter(replies, hispanic_third == "lower"), "communities in the lower third of percent Hispanic"),
  subgroup(datawizard::data_filter(replies, hispanic_third == "middle"), "communities in the middle third of percent Hispanic"),
  subgroup(datawizard::data_filter(replies, hispanic_third == "top"), "communities in the top third of percent Hispanic")
)
readr::write_csv(subgroups, "cases/housing/data/display_subgroups.csv")  # r-style: keep – datawizard::data_write() writes other bytes

# Randomisation distribution of the reference specification: the names
# reshuffled among the white and Hispanic replies, 4,999 times, by complete
# random assignment of as many Hispanic names as the design gave them
# (ri2; Gerber and Green 2012, chapter 3), with the gap recomputed each time
set.seed(20261025)
ref_frame <- spec_frame(audit, "white", "replies", "drop") |>
  datawizard::data_select(c("y", "hisp"))
ri_ref <- ri2::conduct_ri(y ~ hisp, assignment = "hisp",
                          declaration = randomizr::declare_ra(N = nrow(ref_frame), m = sum(ref_frame$hisp)),
                          sharp_hypothesis = 0, IPW = FALSE, data = ref_frame, sims = 4999)
ri <- tibble::tibble(draw = 1:4999, b_pp = 100 * ri_ref$sims_df$est_sim)
readr::write_csv(ri, "cases/housing/data/display_ri.csv")  # r-style: keep – datawizard::data_write() writes other bytes

# Power of a two-sided test at the 5% level for a gap of a given size from a
# white rate of 61%, at the group sizes of the original (replies, Hispanic
# against white) and of the replication (738 replies in two equal groups),
# from the normal approximation of stats::power.prop.test()
power <- tidyr::expand_grid(design = c("original (175 and 187 replies)", "replication (369 and 369 replies)"),
                            gap_pp = seq(2, 25, by = 1)) |>
  datawizard::data_modify(
    n_per_group = dplyr::if_else(stringr::str_starts(design, "original"), 181, 369),
    power = purrr::map2_dbl(gap_pp, n_per_group, \(g, n)
                            stats::power.prop.test(n = n, p1 = 0.61, p2 = 0.61 - g / 100)$power)
  )
readr::write_csv(power, "cases/housing/data/display_power.csv")  # r-style: keep – datawizard::data_write() writes other bytes
