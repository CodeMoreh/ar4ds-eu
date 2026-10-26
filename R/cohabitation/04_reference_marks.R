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

source("R/cohabitation/_tools.R")

# readr rather than datawizard::data_read(), which reads empty cells as "" and
# the codes of the cases, strata and clusters as numbers
unions <- readr::read_csv("cases/cohabitation/data/cohab_unions.csv", show_col_types = FALSE,
                          col_types = readr::cols(caseid = readr::col_character(),
                                                  stratum = readr::col_character(),
                                                  psu = readr::col_character()))

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
if (!file.exists(score_path)) utils::download.file(SCORE_URL, score_path, mode = "wb", quiet = TRUE)
stopifnot(unname(tools::md5sum(score_path)) == SCORE_MD5)
score <- readr::read_csv(score_path, show_col_types = FALSE)

# The same file with its 1995 cohort rebuilt: the 929 unions of the paper in
# the conventions of the SCORE file (an intact union contributes as many months
# as it lasted, a union that ended one more month, with the outcome in the last;
# AGE is the age at the start rounded up; education with the associate degree
# as college). The four other cycles are left as SCORE built them. The months
# come from survival::survSplit() (Therneau and Grambsch 2000), as in
# person_months() of _tools.R, with the length of a union counted as SCORE
# counts it.
unions_1995 <- unions |>
  spec_unions() |>
  datawizard::data_filter(cycle == "1995") |>
  datawizard::data_modify(MONTHNUM = duration + (outcome != "intact"),
                          status = factor(outcome, levels = c("intact", "marriage", "separation")))
rebuilt_1995 <- unions_1995 |>
  survival::survSplit(cut = seq_len(max(unions_1995$MONTHNUM) - 1), end = "MONTHNUM", event = "status",
                      start = "month_start") |>
  datawizard::data_modify(
    CASEID = as.numeric(caseid),
    OUTCOME = as.integer(status),
    LESSHIGH = as.integer(educ_score == "less_hs"), HIGH = as.integer(educ_score == "hs"),
    SOMECOLLEGE = as.integer(educ_score == "some_college"), COLLEGEPLUS = as.integer(educ_score == "college"),
    P95 = 1, P10 = 0, P13 = 0, P15 = 0,
    ONH = as.integer(race == "other"), HSP = as.integer(race == "hispanic"), AA = as.integer(race == "black"),
    NHW = as.integer(race == "white"), COH1DUR = duration, AGE = ceiling(age_start), P02 = 0
  ) |>
  datawizard::data_select(c("CASEID", "MONTHNUM", "OUTCOME", "LESSHIGH", "HIGH", "SOMECOLLEGE", "COLLEGEPLUS",
                            "P95", "P10", "P13", "P15", "ONH", "HSP", "AA", "NHW", "COH1DUR", "AGE", "P02"))
repaired <- dplyr::bind_rows(rebuilt_1995, datawizard::data_filter(score, P95 == 0))

# The five analyses. Each keeps the choices of the script of its analyst; the
# last month of every woman, which three of them analyse, is the row with the
# largest MONTHNUM of each CASEID, in the order of CASEID as the scripts left it.
# The rows are chosen with dplyr inside the functions, as in _tools.R.

# 73FDT (R, nnet): the last month of every woman, a multinomial model of the
# outcome on five period dummies (all five beside the intercept, as in the
# script) with the duration and age; then a regression of the five marriage
# coefficients on the middle year of each period, whose F is the result.
analysis_73fdt <- function(d) {
  women <- d |>
    dplyr::slice_max(MONTHNUM, n = 1, by = CASEID) |>
    dplyr::arrange(CASEID)
  fit <- nnet::multinom(OUTCOME ~ P95 + P02 + P10 + P13 + P15 + COH1DUR + AGE, data = women, trace = FALSE)
  est <- tibble::tibble(year = c(1995, 2002, 2008, 2012, 2014),
                        estimate = stats::coef(fit)["2", c("P95", "P02", "P10", "P13", "P15")])
  slope <- summary(stats::lm(estimate ~ year, data = est))
  tibble::tibble(stat_type = "F", stat = slope$fstatistic[["value"]], df1 = 1, df2 = 3, n = nrow(women),
                 estimate = stats::coef(slope)["year", "Estimate"], p = stats::coef(slope)["year", "Pr(>|t|)"])
}

# N8ZYW (jamovi): the last month of the women of 1995 and 2006-2010, a logit of
# marriage on the later period, two education dummies, two race dummies, the
# total duration and age
analysis_n8zyw <- function(d) {
  women <- d |>
    dplyr::filter(P95 == 1 | P10 == 1) |>
    dplyr::slice_max(MONTHNUM, n = 1, by = CASEID) |>
    dplyr::arrange(CASEID)
  co <- stats::glm(I(OUTCOME == 2) ~ HIGH + COLLEGEPLUS + P10 + HSP + AA + COH1DUR + AGE,
                   family = stats::binomial(), data = women) |>
    parameters::model_parameters(keep = "^P10$", ci_method = "wald")
  tibble::tibble(stat_type = "z", stat = co$z, df1 = NA, df2 = NA, n = nrow(women),
                 estimate = co$Coefficient, p = co$p)
}

# 456AP (R, VGAM): every person-month of 2002 and 2013-2015, a multinomial model
# with staying together as reference, on the later period, education, age and
# race, without duration; nnet gives the same maximum-likelihood fit as vglm
analysis_456ap <- function(d) {
  months <- d |>
    dplyr::filter(P02 == 1 | P15 == 1) |>
    dplyr::mutate(outcome = factor(OUTCOME, levels = 1:3, labels = c("intact", "marry", "break")))
  co <- nnet::multinom(outcome ~ P15 + LESSHIGH + HIGH + SOMECOLLEGE + AGE + ONH + HSP + AA, data = months,
                       trace = FALSE, maxit = 500) |>
    parameters::model_parameters(keep = "^P15$") |>
    datawizard::data_filter(Response == "marry")
  tibble::tibble(stat_type = "z", stat = co$z, df1 = NA, df2 = NA, n = nrow(months),
                 estimate = co$Coefficient, p = co$p)
}

# 5X3KG (R): the share of person-month rows that record a marriage in 1995
# against 2006-2010, compared by prop.test() with the continuity correction
analysis_5x3kg <- function(d) {
  rows <- dplyr::filter(d, P95 == 1 | P10 == 1)
  test <- stats::prop.test(x = c(sum(rows$P95 == 1 & rows$OUTCOME == 2), sum(rows$P10 == 1 & rows$OUTCOME == 2)),
                           n = c(sum(rows$P95), sum(rows$P10)), correct = TRUE)
  tibble::tibble(stat_type = "chi2", stat = test$statistic[["X-squared"]], df1 = 1, df2 = NA, n = nrow(rows),
                 estimate = unname(test$estimate[2] - test$estimate[1]), p = test$p.value)
}

# JDA15 (R): the last month of every woman of the five cycles, a logit of
# marriage (against separation or an intact union) on education interacted with
# race, age, the total duration and a linear term for years since 1995
analysis_jda15 <- function(d) {
  women <- d |>
    dplyr::slice_max(MONTHNUM, n = 1, by = CASEID) |>
    dplyr::arrange(CASEID) |>
    dplyr::mutate(
      education = factor(dplyr::case_when(LESSHIGH == 1 ~ "lesshigh", HIGH == 1 ~ "high",
                                          SOMECOLLEGE == 1 ~ "somecollege", COLLEGEPLUS == 1 ~ "collegeplus"),
                         levels = c("lesshigh", "high", "somecollege", "collegeplus")),
      race = factor(dplyr::case_when(AA == 1 ~ "black", NHW == 1 ~ "white", HSP == 1 ~ "hispanic",
                                     ONH == 1 ~ "other"),
                    levels = c("black", "hispanic", "other", "white")),
      year = dplyr::case_when(P95 == 1 ~ 1995, P02 == 1 ~ 2002, P10 == 1 ~ 2010, P13 == 1 ~ 2013,
                              P15 == 1 ~ 2015) - 1995
    )
  co <- stats::glm(I(OUTCOME == 2) ~ education * race + AGE + COH1DUR + year, family = stats::binomial(),
                   data = women) |>
    parameters::model_parameters(keep = "^year$", ci_method = "wald")
  tibble::tibble(stat_type = "z", stat = co$z, df1 = NA, df2 = NA, n = nrow(women),
                 estimate = co$Coefficient, p = co$p)
}

ANALYSES <- list(`73FDT` = analysis_73fdt, N8ZYW = analysis_n8zyw, `456AP` = analysis_456ap,
                 `5X3KG` = analysis_5x3kg, JDA15 = analysis_jda15)

m100 <- readr::read_csv("data-raw/cohabitation/multi100/multi100_kuo_extract.csv", show_col_types = FALSE) |>
  datawizard::data_select(c(
    id = "analyst_id", discipline = "primary_discipline", hours = "total_hours",
    task1 = "task1_categorisation_plotting", direction = "direction_of_result", software = "task2_software",
    statistic_type = "reanalysis_type_of_statistic", statistic_recorded = "reanalysis_statistic_report",
    df1_recorded = "reanalysis_degrees_of_freedom_1", df2_recorded = "reanalysis_degrees_of_freedom_2",
    n_recorded = "reanalysis_model_sample_size", r_recorded = "reanalysis_correlation_coef",
    d_recorded = "reanalysis_cohens_d"
  ))

refits <- purrr::imap(ANALYSES, \(analysis, id) {
  on_score <- analysis(score)
  on_repaired <- analysis(repaired)
  tibble::tibble(
    id = id,
    stat_type_refit = on_score$stat_type, stat_refit = on_score$stat, n_refit = on_score$n,
    estimate_refit = on_score$estimate, p_refit = on_score$p,
    r_refit = r_multi100(on_score$stat, on_score$stat_type, on_score$df2, on_score$n),
    stat_repaired = on_repaired$stat, n_repaired = on_repaired$n,
    estimate_repaired = on_repaired$estimate, p_repaired = on_repaired$p,
    r_repaired = r_multi100(on_repaired$stat, on_repaired$stat_type, on_repaired$df2, on_repaired$n)
  )
}) |>
  dplyr::bind_rows()

# Choices of each analysis, from the Task 2 reports and the scripts on the OSF
choices <- tibble::tribble(
  ~id,     ~comparison,                      ~unit,                           ~outcome_model,                                        ~controls,
  "73FDT", "five cycles, 1995 to 2013-2015", "one row per woman (last month)", "multinomial logit, then a regression of five coefficients on year", "total duration, age",
  "N8ZYW", "1995 against 2006-2010",         "one row per woman (last month)", "logit of marriage against no marriage",                "two education and two race dummies, total duration, age",
  "456AP", "2002 against 2013-2015",         "person-months",                  "multinomial logit, staying together as reference",     "education, race, age; no duration",
  "5X3KG", "1995 against 2006-2010",         "person-month rows",              "two-sample test of proportions (prop.test)",           "none",
  "JDA15", "five cycles, years since 1995",  "one row per woman (last month)", "logit of marriage against no marriage",                "education x race, age, total duration"
)

analysts <- m100 |>
  dplyr::left_join(choices, by = "id") |>
  dplyr::left_join(refits, by = "id") |>
  datawizard::data_modify(matches_record = dplyr::case_when(
    is.na(statistic_recorded) ~ NA,
    .default = abs(abs(stat_refit) - abs(statistic_recorded)) < 0.006 & n_refit == n_recorded
  ))

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(analysts, "cases/cohabitation/data/analysts5.csv", na = "")

# The SCORE replication (report osf.io/s2hez, script Phase I Kuo (JWzJ).R): the
# person-months of 2002 against those of 2011-2013 and 2013-2015 pooled,
# unweighted multinomial model with education x period; the claim tested is the
# some-college x period interaction. The same script on 2002 against 2006-2010
# is what the report calls its reproduction. dplyr in the body, because the
# rows depend on the argument `later_cycles`.
score_interaction <- function(d, later_cycles) {
  months <- d |>
    dplyr::filter(P02 == 1 | dplyr::if_any(dplyr::all_of(later_cycles), \(v) v == 1)) |>
    dplyr::mutate(period = as.integer(dplyr::if_any(dplyr::all_of(later_cycles), \(v) v == 1)),
                  outcome = stats::relevel(factor(OUTCOME, levels = 1:3,
                                                  labels = c("Stay Intact", "Marry", "Break Up")),
                                           ref = "Stay Intact"),
                  AGE2 = AGE^2, LESSHIGH_PERIOD = period * LESSHIGH, HIGH_PERIOD = period * HIGH,
                  SOMECOLLEGE_PERIOD = period * SOMECOLLEGE)
  co <- nnet::multinom(outcome ~ period + AA + HSP + ONH + LESSHIGH + HIGH + SOMECOLLEGE + LESSHIGH_PERIOD +
                         HIGH_PERIOD + SOMECOLLEGE_PERIOD + COH1DUR + AGE + AGE2, data = months,
                       trace = FALSE, maxit = 1000) |>
    parameters::model_parameters(keep = "^SOMECOLLEGE_PERIOD$") |>
    datawizard::data_filter(Response == "Marry")
  tibble::tibble(b = co$Coefficient, se = co$SE, stat = co$z, n = nrow(months),
                 n_women = dplyr::n_distinct(months$CASEID))
}
score_replication <- score_interaction(score, c("P13", "P15"))
score_reproduction <- score_interaction(score, "P10")

# Models 1 and 2 of Table 2 on the rebuilt unions: every coefficient of both
# equations with its design-based error, beside the published values
model_table <- function(adjust) {
  m <- fit_model(unions, adjust = adjust)
  tibble::tibble(equation = rep(colnames(m$coef), each = nrow(m$coef)), term = rep(rownames(m$coef), ncol(m$coef)),
                 b = as.vector(m$coef), se = sqrt(diag(m$vcov)))
}

# The published table, one row per term and equation; datawizard::data_to_long()
# has no ".value" to split each column name into an equation and a statistic
published <- tibble::tribble(
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
  tidyr::pivot_longer(-c(model, term), names_to = c("equation", ".value"), names_sep = "_") |>
  datawizard::data_rename(c(b_published = "b", se_published = "se"))

reproduction <- dplyr::bind_rows(model_table("educ_period") |> datawizard::data_modify(model = "1"),
                                 model_table("race_period") |> datawizard::data_modify(model = "2")) |>
  dplyr::left_join(published, by = c("model", "equation", "term")) |>
  datawizard::data_relocate(c("model", "equation", "term"))

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(reproduction, "cases/cohabitation/data/display_reproduction.csv", na = "")

# The marks. The published coefficient is that of Model 2, the change for
# non-Hispanic white women in a model with race-ethnicity x period; Multi100
# recorded it as z = -4.8786 on 54,291 person-months.
oa_refit <- fit_spec(unions, adjust = "race_period")
reference <- fit_spec(unions)
later_cycles <- c("2002_2011", "1995_2011") |>
  purrr::map(\(cmp) fit_spec(unions, comparison = cmp)) |>
  dplyr::bind_rows()

marks <- dplyr::bind_rows(
  tibble::tibble(mark = "OA", label = "OAs 2016", source = "Table 2, Model 2 (race-ethnicity x period), published",
                 b = -0.683, se = 0.14, stat_type = "z", stat = -0.683 / 0.14, df = NA, n = 54291, on_chart = TRUE),
  tibble::tibble(mark = "OA_refit", label = "OA refit",
                 source = "Model 2 refitted on the rebuilt unions, design-based errors",
                 b = oa_refit$b, se = oa_refit$se, stat_type = "t", stat = oa_refit$stat, df = oa_refit$df,
                 n = oa_refit$n, z_model = oa_refit$z_model, on_chart = FALSE),
  tibble::tibble(mark = "reference", label = "Reference", source = "The reference specification of the curve",
                 b = reference$b, se = reference$se, stat_type = "t", stat = reference$stat, df = reference$df,
                 n = reference$n, z_model = reference$z_model, on_chart = FALSE),
  analysts |>
    dplyr::transmute(mark = id, label = id,
                     source = paste0("Multi100 analyst, refitted on the SCORE file: ", outcome_model, "; ", comparison),
                     b = estimate_refit, se = NA, stat_type = stat_type_refit, stat = stat_refit,
                     df = dplyr::if_else(stat_type_refit == "F", 3, NA), n = n_refit,
                     on_chart = id == "456AP"),
  tibble::tibble(mark = "SCORE_replication", label = "SCORE replication",
                 source = "Some college x period, 2002 against 2011-2015, SCORE file, unweighted (refit of osf.io/u8x9s)",
                 b = score_replication$b, se = score_replication$se, stat_type = "z", stat = score_replication$stat,
                 df = NA, n = score_replication$n, on_chart = FALSE),
  tibble::tibble(mark = "SCORE_reproduction", label = "SCORE reproduction",
                 source = "Some college x period, 2002 against 2006-2010, SCORE file, unweighted (refit of osf.io/u8x9s)",
                 b = score_reproduction$b, se = score_reproduction$se, stat_type = "z", stat = score_reproduction$stat,
                 df = NA, n = score_reproduction$n, on_chart = FALSE),
  later_cycles |>
    dplyr::transmute(mark = paste0("course_", comparison),
                     label = paste("Course,", stringr::str_replace(comparison, "_", " vs ")),
                     source = "The reference specification on other NSFG cycles (rebuilt unions)",
                     b, se, stat_type = "t", stat, df, n, z_model, on_chart = FALSE)
) |>
  # r by the Multi100 rule: from the recorded or refitted statistic, and for the
  # course fits from the conventional z on the person-months, as on the curve;
  # the p of the F of 73FDT comes from its own regression
  datawizard::data_modify(
    p = dplyr::case_when(mark == "73FDT" ~ analysts$p_refit[analysts$id == "73FDT"],
                         stat_type == "t" ~ 2 * stats::pt(-abs(stat), df),
                         stat_type == "z" ~ 2 * stats::pnorm(-abs(stat)),
                         stat_type == "chi2" ~ stats::pchisq(stat, 1, lower.tail = FALSE)),
    r = dplyr::if_else(is.na(z_model), r_multi100(stat, stat_type, df, n), r_multi100(z_model, "z", NA, n))
  )

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(marks, "cases/cohabitation/data/reference_marks.csv", na = "")
