################################################################################
# Title:        01_unions.R
# Purpose:      Builds the first premarital cohabitations of the women of five
#               NSFG cycles (1995, 2002, 2006-2010, 2011-2013, 2013-2015) from
#               the public-use female respondent files: start, end and outcome
#               of the union, age at its start, education, race-ethnicity, the
#               weight and the design variables. The only stage that reads the
#               NSFG files.
# Reads:        the fixed-width female respondent files of the NSFG and their SAS
#               setup files, in the NSFG folder of the data hub (NSFG_DIR)
# Writes:       cases/cohabitation/data/cohab_unions.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

# The raw seam: the NSFG folder of the data hub, as downloaded from
# ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/ (original file names).
# Set NSFG_DIR to read a copy elsewhere.
NSFG_DIR <- Sys.getenv("NSFG_DIR", "D:/ACADEMIC/DATA/SURVEYS - national other/NSFG (USA)")
DAT_DIR <- file.path(NSFG_DIR, "Datasets", "NSFG")
SETUP_DIR <- file.path(DAT_DIR, "sas")

CYCLES <- tibble::tribble(
  ~cycle,    ~dat,                          ~setup,                         ~weight,        ~stratum,  ~psu,
  "1995",    "1995FemRespData.dat",         "1995FemRespSetup.sas",         "POST_WT",      "COL_STR", "PANEL",
  "2002",    "2002FemResp.dat",             "2002FemRespInput.sas",         "FINALWGT",     "SEST",    "SECU_R",
  "2006-10", "2006_2010_FemResp.dat",       "2006_2010_FemRespSetup.sas",   "WGTQ1Q16",     "SEST",    "SECU",
  "2011-13", "2011_2013_FemRespData.dat",   "2011_2013_FemRespSetup.sas",   "WGT2011_2013", "SEST",    "SECU",
  "2013-15", "2013_2015_FemRespData.dat",   "2013_2015_FemRespSetup.sas",   "WGT2013_2015", "SEST",    "SECU"
)

# Column positions from the INPUT statement of a SAS setup file: a name, an
# optional `$` and a column or a range of columns. SAScii::parse.SAScii() reads
# the setup files of 2002, 2011-2013 and 2013-2015 identically but stops on the
# encoding of the 2006-2010 file and places 1,100 variables of the 1995 file
# elsewhere; asciiSetupReader stops on all five.
setup_positions <- function(setup_file) {
  lines <- readr::read_lines(setup_file)
  first <- stringr::str_which(lines, stringr::regex("^\\s*input\\s*$|^\\s*input\\s", ignore_case = TRUE))[1]
  last <- first + stringr::str_which(lines[first:length(lines)], ";")[1] - 1
  block <- paste(lines[first:last], collapse = " ")
  stringr::str_match_all(block, "([A-Za-z_][A-Za-z0-9_]*)\\s+(?:\\$\\s*)?(\\d+)(?:\\s*-\\s*(\\d+))?")[[1]] |>
    tibble::as_tibble(.name_repair = ~ c("match", "name", "begin", "end")) |>
    dplyr::transmute(name = toupper(name), begin = as.integer(begin),
                     end = dplyr::coalesce(as.integer(end), as.integer(begin))) |>
    dplyr::filter(name != "INPUT") |>
    dplyr::distinct(name, .keep_all = TRUE)
}

read_nsfg <- function(dat_file, setup_file, vars) {
  pos <- setup_positions(setup_file) |>
    dplyr::filter(name %in% vars)
  missing_vars <- setdiff(vars, pos$name)
  if (length(missing_vars) > 0) stop("Not in ", basename(setup_file), ": ", paste(missing_vars, collapse = ", "))
  readr::read_fwf(dat_file, readr::fwf_positions(pos$begin, pos$end, pos$name),
                  col_types = readr::cols(.default = readr::col_character()), progress = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                \(v) readr::parse_number(stringr::str_trim(v), na = c("", "."))))
}

# The 1995 file carries the start (COHAB1) and the outcome (COHOUT) of the first
# cohabitation but not its end, which is taken from the source of COHAB1 as the
# recode specification describes it: the date of the marriage to the partner for
# a cohabitation that became a marriage, the date on which the couple stopped
# living together (the last time) for one that dissolved, the interview for one
# that is intact. Century months (months since December 1899) above 9996 are
# the codes for not ascertained, refused and don't know.
union_1995 <- function(spec) {
  husband_start <- c(STRTOGH1 = "MARDAT01", STRTOGH2 = "MARDAT02", STRTOGH3 = "MARDAT03",
                     STRTOGH4 = "MARDAT04")
  other_start <- paste0("STRTOTH", 0:7)
  other_stop <- paste0("STOPOTH", 0:7)
  vars <- c("CASEID", "BDAYCENM", "A_DOI", "AGE", "FMARITAL", "FMARNO", paste0("MARDAT0", 1:5),
            "HIEDUC", "HISPRACE", "COHEVER", "COHAB1", "COHSTAT", "COHOUT", "COHAB1_I", "COHOUT_I",
            names(husband_start), "STRTOGCH", "WNSTRTCP", other_start, other_stop,
            spec$weight, spec$stratum, spec$psu)
  raw <- read_nsfg(file.path(DAT_DIR, spec$dat), file.path(SETUP_DIR, spec$setup), vars) |>
    dplyr::mutate(dplyr::across(c(BDAYCENM, A_DOI, dplyr::starts_with("MARDAT"), dplyr::all_of(names(husband_start)),
                                  STRTOGCH, WNSTRTCP, dplyr::all_of(other_start), dplyr::all_of(other_stop)),
                                \(v) dplyr::if_else(v >= 9997, NA_real_, v)))

  # The marriage date that belongs to the husband with whom the first
  # cohabitation began; the current husband is the most recent marriage
  marriage_dates <- raw |>
    dplyr::select(CASEID, dplyr::all_of(paste0("MARDAT0", 1:5))) |>
    tidyr::pivot_longer(-CASEID, names_to = "mardat", values_to = "end_marriage")
  marriage_end <- raw |>
    dplyr::select(CASEID, COHAB1, FMARNO, dplyr::all_of(names(husband_start)), STRTOGCH) |>
    tidyr::pivot_longer(c(dplyr::all_of(names(husband_start)), STRTOGCH), names_to = "source", values_to = "start") |>
    dplyr::filter(!is.na(start), start == COHAB1) |>
    dplyr::mutate(mardat = dplyr::if_else(source == "STRTOGCH", paste0("MARDAT0", pmin(FMARNO, 5)),
                                          husband_start[source])) |>
    dplyr::left_join(marriage_dates, by = c("CASEID", "mardat")) |>
    dplyr::arrange(CASEID, source) |>
    dplyr::summarise(end_marriage = dplyr::first(end_marriage), source_marriage = dplyr::first(source),
                     .by = CASEID)

  # The stop date of the first former partner whose start date is COHAB1
  dissolution_end <- raw |>
    dplyr::select(CASEID, COHAB1, dplyr::all_of(other_start), dplyr::all_of(other_stop)) |>
    tidyr::pivot_longer(-c(CASEID, COHAB1), names_to = c(".value", "partner"),
                        names_pattern = "(STRTOTH|STOPOTH)(\\d)") |>
    dplyr::filter(!is.na(STRTOTH), STRTOTH == COHAB1) |>
    dplyr::arrange(CASEID, partner) |>
    dplyr::summarise(end_dissolution = dplyr::first(STOPOTH), .by = CASEID)

  raw |>
    dplyr::left_join(marriage_end, by = "CASEID") |>
    dplyr::left_join(dissolution_end, by = "CASEID") |>
    dplyr::transmute(
      cycle = spec$cycle, caseid = as.character(CASEID), cm_birth = BDAYCENM, cm_interview = A_DOI,
      age_interview = AGE, cohstat = COHSTAT, cohout = COHOUT, cm_start = COHAB1,
      cm_end = dplyr::case_when(COHOUT == 1 ~ A_DOI,
                                COHOUT %in% 2:3 ~ dplyr::coalesce(end_marriage, MARDAT01),
                                COHOUT == 4 ~ end_dissolution),
      imputed = COHAB1_I %in% 1 | COHOUT_I %in% 1,
      hieduc = HIEDUC, hieduc_fixed = HIEDUC, hisprace = HISPRACE,
      weight = .data[[spec$weight]], stratum = .data[[spec$stratum]], psu = .data[[spec$psu]]
    )
}

# From 2002 the recodes carry the duration of the first cohabitation (COH1DUR,
# months from its start to the interview, the marriage or the end of living
# together), so its end is COHAB1 + COH1DUR.
#
# HIEDUC of 2006-2010 codes some women with a GED below high school: from the
# second year of interviewing, women with 12 or fewer years of school, a GED and
# no degree kept their years of school. `hieduc_fixed` applies the correction
# published by NCHS (HIEDUC_correction_2006_2010.pdf).
union_recoded <- function(spec) {
  fix_vars <- if (spec$cycle == "2006-10") c("HAVEDEG", "DIPGED", "EDUCAT", "QUESTYEAR")
  vars <- c("CASEID", "CMBIRTH", "CMINTVW", "AGER", "HIEDUC", "HISPRACE", "COHAB1", "COHSTAT",
            "COHOUT", "COH1DUR", "COHAB1_I", "COHOUT_I", "COH1DUR_I", fix_vars,
            spec$weight, spec$stratum, spec$psu)
  raw <- read_nsfg(file.path(DAT_DIR, spec$dat), file.path(SETUP_DIR, spec$setup), vars)
  raw <- if (spec$cycle == "2006-10") {
    dplyr::mutate(raw, HIEDUC_FIXED = dplyr::if_else(HAVEDEG %in% c(NA, 5) & DIPGED %in% 2 &
                                                       dplyr::between(EDUCAT, 9, 12) & QUESTYEAR >= 2,
                                                     9, HIEDUC))
  } else {
    dplyr::mutate(raw, HIEDUC_FIXED = HIEDUC)
  }
  raw |>
    dplyr::mutate(dplyr::across(c(CMBIRTH, CMINTVW, COHAB1), \(v) dplyr::if_else(v >= 9997, NA_real_, v))) |>
    dplyr::transmute(
      cycle = spec$cycle, caseid = as.character(CASEID), cm_birth = CMBIRTH, cm_interview = CMINTVW,
      age_interview = AGER, cohstat = COHSTAT, cohout = COHOUT, cm_start = COHAB1,
      cm_end = cm_start + COH1DUR,
      imputed = COHAB1_I %in% 1 | COHOUT_I %in% 1 | COH1DUR_I %in% 1,
      hieduc = HIEDUC, hieduc_fixed = HIEDUC_FIXED, hisprace = HISPRACE,
      weight = .data[[spec$weight]], stratum = .data[[spec$stratum]], psu = .data[[spec$psu]]
    )
}

women <- CYCLES |>
  dplyr::group_split(cycle) |>
  purrr::map(\(spec) if (spec$cycle == "1995") union_1995(spec) else union_recoded(spec)) |>
  dplyr::bind_rows()

# One row per woman whose first cohabitation came before any marriage, with
# the outcome in three categories and the timing in months. `in_window` marks
# the unions of the analysis of the paper: begun no more than five years
# before the interview and lasting at least a month; in 2006-2010 the paper
# also keeps only unions begun in 2005 or later, `from_2005`.
#
# Education at interview in four groups. HIEDUC codes completed grades below 9
# (in 1995 from 1, later from 5), 9 a high school diploma or GED, 10 some
# college without a degree, 11 an associate degree and 12 or more a bachelor's
# degree or higher. The shares of Table 1 of the paper come out with the
# associate degree counted as some college (`educ`) and with the corrected
# HIEDUC of 2006-2010; the SCORE file counts it as college and uses HIEDUC as
# released (`educ_score`). datawizard::recode_values() turns these numeric
# codes into NA rather than into words.
unions <- women |>
  dplyr::filter(cohstat == 2) |>
  dplyr::mutate(
    outcome = dplyr::case_when(cohout == 1 ~ "intact", cohout %in% 2:3 ~ "marriage", cohout == 4 ~ "separation"),
    duration = cm_end - cm_start,
    since_start = cm_interview - cm_start,
    year_start = 1900 + (cm_start - 1) %/% 12,
    age_start = (cm_start - cm_birth) / 12,
    educ = dplyr::case_when(hieduc_fixed <= 8 ~ "less_hs", hieduc_fixed == 9 ~ "hs",
                            hieduc_fixed %in% 10:11 ~ "some_college", hieduc_fixed >= 12 ~ "college"),
    educ_score = dplyr::case_when(hieduc <= 8 ~ "less_hs", hieduc == 9 ~ "hs", hieduc == 10 ~ "some_college",
                                  hieduc >= 11 ~ "college"),
    race = dplyr::case_when(hisprace == 1 ~ "hispanic", hisprace == 2 ~ "white", hisprace == 3 ~ "black",
                            hisprace == 4 ~ "other"),
    in_window = since_start <= 60 & duration >= 1,
    from_2005 = cm_start >= (2005 - 1900) * 12 + 1,
    stratum = paste(cycle, stratum, sep = "_"),
    psu = paste(stratum, psu, sep = "_")
  ) |>
  dplyr::select(cycle, caseid, weight, stratum, psu, cm_interview, cm_start, cm_end, year_start, since_start,
                duration, outcome, age_start, age_interview, educ, educ_score, race, hieduc, hieduc_fixed,
                hisprace, imputed,
                in_window, from_2005) |>
  dplyr::arrange(cycle, caseid)

stopifnot(!anyNA(unions$outcome), !anyNA(unions$cm_start))

# Every stratum and cluster of each survey holds at least one woman of the
# file, so the design-based errors of _tools.R can take the design from it:
# the clusters of a subset that hold none of its women still count in their
# strata, as survey counts them for a domain.
design_all <- women |>
  dplyr::distinct(cycle, psu = paste(cycle, stratum, psu, sep = "_"))
stopifnot(setequal(design_all$psu, unions$psu), nrow(women) == 10847 + 7643 + 12279 + 5601 + 5699)

# r-style: keep – datawizard::data_write() writes other bytes than readr
readr::write_csv(unions, "cases/cohabitation/data/cohab_unions.csv", na = "")
