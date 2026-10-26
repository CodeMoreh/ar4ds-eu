################################################################################
# Title:        01_extract.R
# Purpose:      The analysis extract of the immigration-and-trust case: every
#               respondent of the 16 countries of the original study in ESS
#               rounds 1-4, with the outcomes, the immigration items and the
#               controls of the model built as the paper describes them. The
#               only stage that reads licensed survey files.
# Reads:        the cumulative ESS file of rounds 1-10 and the separate Italy
#               file of round 2 (ESS2IT) in the data hub,
#               data-raw/trust/party_codes.csv, data-raw/trust/cabinets.csv
# Writes:       cases/trust/data/trust_ess.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

# The one raw seam. ESS_DIR is the ESS folder of the data hub; ESS_FILE and
# ESS_IT2_FILE override the two files inside it one at a time.
ESS_DIR <- Sys.getenv("ESS_DIR", "D:/ACADEMIC/DATA/SURVEYS - cross-national/ESS")  # r-style: keep – the one overridable raw path
ESS_FILE <- Sys.getenv("ESS_FILE", file.path(ESS_DIR, "ESS_1-10_downloaded_2024.03.rds"))
ESS_IT2_FILE <- Sys.getenv("ESS_IT2_FILE",
                           file.path(ESS_DIR, "Not_included_in_cumulative_files", "ESS2IT.zip"))

COUNTRIES <- c("AT", "BE", "CH", "DE", "DK", "ES", "FI", "FR", "GB", "GR", "IE", "IT",
               "NL", "NO", "PT", "SE")

party_codes <- readr::read_csv("data-raw/trust/party_codes.csv", show_col_types = FALSE)
cabinets <- readr::read_csv("data-raw/trust/cabinets.csv", show_col_types = FALSE)
vote_vars <- unique(party_codes$variable)

SURVEY_VARS <- c("cntry", "essround", "idno", "dweight", "pspwght", "pweight", "ctzcntr", "blgetmg",
                 "trstprl", "trstplt", "trstlgl", "imbgeco", "imueclt", "imwbcnt",
                 "happy", "stflife", "sclmeet", "ppltrst", "pplfair", "pplhlp",
                 "stfeco", "hincfel", "stfhlth", "stfedu", "stfgov", "lrscale",
                 "hinctnt", "hinctnta", "vote", "agea", "gndr", "edulvla",
                 "inwyr", "inwmm", "inwdd", "inwyys", "inwmms", "inwdds")

# The cumulative file stores answers as factors whose levels are the codes, with
# the missing-value codes already set to NA; the Stata file of Italy stores them
# as labelled numbers. as_code() reads both as plain numbers.
as_code <- \(x) if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(haven::zap_labels(x))

# dplyr::any_of() keeps the columns that a file holds; each round has its own vote variables
cumulative <- readRDS(ESS_FILE) |>
  datawizard::data_filter(essround <= 4, cntry %in% COUNTRIES) |>
  dplyr::select(dplyr::any_of(c(SURVEY_VARS, vote_vars)))
invisible(gc())

# Italy, round 2: fielded in 2006 and released as a country file of its own,
# outside the cumulative file. It is unpacked to a temporary folder.
unpacked <- unzip(ESS_IT2_FILE, files = "ESS2IT.dta", exdir = tempfile("ess2it"))
italy_r2 <- haven::read_dta(unpacked) |>
  dplyr::select(dplyr::any_of(c(SURVEY_VARS, vote_vars)))

ess <- dplyr::bind_rows(
  dplyr::mutate(cumulative, dplyr::across(-c(cntry, essround), as_code)),
  dplyr::mutate(italy_r2, dplyr::across(-c(cntry, essround), as_code), cntry = as.character(cntry))
)
rm(cumulative, italy_r2)
invisible(gc())

# Interview date: rounds 1-2 record it as inwyr/inwmm/inwdd, rounds 3-4 as the
# start of the interview (inwyys/inwmms/inwdds). A missing date, or one outside
# the years in which rounds 1-4 were fielded, takes the median date of its
# country-round, so that the cabinet in office can still be read.
ess <- ess |>
  dplyr::mutate(date = lubridate::make_date(dplyr::coalesce(inwyys, inwyr), dplyr::coalesce(inwmms, inwmm),
                                            dplyr::coalesce(inwdds, inwdd)),
                date = dplyr::if_else(dplyr::between(date, as.Date("2002-01-01"), as.Date("2010-12-31")),
                                      date, NA)) |>
  dplyr::mutate(date = dplyr::coalesce(date, stats::median(date, na.rm = TRUE)), .by = c(cntry, essround))

# Party voted for in the last national election: one variable per country-round
# (two in Germany, for the constituency and the list vote). A respondent votes
# far right, or for the cabinet, when either German vote does.
held_votes <- intersect(vote_vars, names(ess))
votes <- ess |>
  datawizard::data_select(c("cntry", "essround", "idno", "date", held_votes)) |>
  datawizard::data_to_long(select = held_votes, names_to = "variable", values_to = "code",
                           values_drop_na = TRUE) |>
  dplyr::inner_join(party_codes, by = c("cntry", "essround", "variable", "code"))

# dplyr keeps the list column of party keys, which datawizard::data_summary() would flatten
in_office <- cabinets |>
  dplyr::summarise(parlgov_ids = list(parlgov_id), .by = c(cntry, cabinet, start_date)) |>
  datawizard::data_arrange(c("cntry", "start_date"))

# The parties in office on each date: the cabinet whose start date is the last
# one before it. dplyr, because datawizard cannot see the arguments of a function.
cabinet_at <- \(country, dates) {
  office <- dplyr::filter(in_office, cntry == country)
  office$parlgov_ids[findInterval(as.numeric(dates), as.numeric(office$start_date))]
}

# dplyr for the grouped steps over 110,000 respondents
vote_flags <- votes |>
  dplyr::mutate(cabinet_ids = cabinet_at(dplyr::first(cntry), date), .by = cntry) |>
  dplyr::mutate(in_cabinet = purrr::map2_lgl(parlgov_id, cabinet_ids, \(ids, cab) {
    !is.na(ids) && any(as.integer(stringr::str_split_1(ids, ";")) %in% cab)
  })) |>
  dplyr::summarise(winner = as.integer(any(in_cabinet)), far_right_vote = as.integer(any(far_right == 1)),
                   .by = c(cntry, essround, idno))

# Household income: twelve bands in rounds 1-3 and deciles in round 4, so the
# paper standardises it within each survey (country-round), over every
# respondent who answered.
# The standardisation within each country-round comes from datawizard::standardize()
# (the mean and SD of the answers in that country-round)
trust_ess <- ess |>
  dplyr::left_join(vote_flags, by = c("cntry", "essround", "idno")) |>
  dplyr::mutate(hh_income = dplyr::coalesce(hinctnt, hinctnta)) |>
  dplyr::mutate(hh_income_z = as.numeric(datawizard::standardize(hh_income)), .by = c(cntry, essround)) |>
  dplyr::transmute(
    cntry, essround, idno,
    citizen = as.integer(ctzcntr == 1), minority = as.integer(blgetmg == 1),
    dweight = round(dweight, 4), pspwght = round(pspwght, 4), pweight = signif(pweight, 5),
    distrust_parl = 10 - trstprl, distrust_pol = 10 - trstplt, distrust_legal = 10 - trstlgl,
    imm_econ = 10 - imbgeco, imm_culture = 10 - imueclt, imm_place = 10 - imwbcnt,
    unhappy = 10 - happy, life_dissat = 10 - stflife, meet_rarely = 8 - sclmeet,
    distrust_people = 10 - ppltrst, unfair_people = 10 - pplfair, unhelpful_people = 10 - pplhlp,
    econ_dissat = 10 - stfeco, income_difficulty = hincfel,
    health_dissat = 10 - stfhlth, educ_dissat = 10 - stfedu, gov_dissat = 10 - stfgov,
    left_right = lrscale, hh_income_z = round(hh_income_z, 3),
    voted = dplyr::case_when(vote == 1 ~ 1L, vote %in% 2:3 ~ 0L),
    winner = dplyr::coalesce(winner, 0L), far_right_vote = dplyr::coalesce(far_right_vote, 0L),
    age = agea, female = as.integer(gndr == 2),
    education = dplyr::if_else(edulvla %in% 1:5, edulvla, NA_real_)
  ) |>
  datawizard::data_arrange(c("cntry", "essround", "idno"))

# The counts of the paper: 120,080 respondents in all (note 40), and 110,732
# citizens who do not belong to an ethnic minority, by country as in Table 1.
TABLE1 <- c(AT = 6310, BE = 6648, CH = 6551, DE = 10719, DK = 5843, ES = 7189, FI = 7946,
            FR = 6835, GB = 7905, GR = 6249, IE = 7187, IT = 2698, NL = 7344, NO = 6653,
            PT = 7406, SE = 7249)
paper_n <- trust_ess |>
  datawizard::data_filter(citizen == 1, minority == 0) |>
  datawizard::data_summary(n = n(), by = "cntry")
stopifnot(nrow(trust_ess) == 120080,
          !anyDuplicated(datawizard::data_select(trust_ess, c("cntry", "essround", "idno"))),
          all(paper_n$n == TABLE1[paper_n$cntry]))

# The respondent number is left out of the published file to keep it near 10 MB;
# rows keep the order of cntry, essround and idno. A copy with idno and the
# interview date stays in the gitignored cache, for checks against the files of
# the SCORE reproductions.
# readr writes the bytes of the committed file; datawizard::data_write() does not
readr::write_csv(datawizard::data_select(trust_ess, exclude = "idno"), "cases/trust/data/trust_ess.csv", na = "")
dir.create("data-raw/trust/cache", showWarnings = FALSE)
ess |>
  dplyr::distinct(cntry, essround, idno, date) |>
  dplyr::right_join(trust_ess, by = c("cntry", "essround", "idno")) |>
  saveRDS("data-raw/trust/cache/trust_ess_ids.rds")
