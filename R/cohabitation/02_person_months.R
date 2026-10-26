################################################################################
# Title:        02_person_months.R
# Purpose:      The person-month file of the case: one row for every month in
#               which a first premarital cohabitation of the two cohorts was at
#               risk of ending, built by person_months() of _tools.R, so that
#               the file is the data that every specification of the curve
#               starts from before its own choices are applied.
# Reads:        cases/cohabitation/data/cohab_unions.csv
# Writes:       cases/cohabitation/data/cohab_months.csv
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

# Both cohorts in their wider window (every union begun no more than five
# years before the interview); `from_2005` marks the unions that the window of
# the paper keeps in the later cohort.
months <- unions |>
  spec_unions(cohorts = "five_years") |>
  person_months() |>
  datawizard::data_modify(year_of_union = pmin(ceiling(month / 12), 5),
                          age_start = round(age_start, 3)) |>
  datawizard::data_select(c("cycle", "caseid", "month", "year_of_union", "y", "later", "from_2005",
                            "age_start", "race", "educ", "weight", "stratum", "psu"))

stopifnot(nrow(months) == sum(spec_unions(unions, "five_years")$duration + 1))

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(months, "cases/cohabitation/data/cohab_months.csv")
