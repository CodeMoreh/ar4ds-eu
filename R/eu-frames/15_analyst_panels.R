##############################################################################
# Title:        15_analyst_panels.R
# Purpose:      Builds, from the sixteen GESIS Eurobarometer waves, the two
#               aggregate files on which the Task 2 results of analysts KQXUE
#               and 018OL reproduce exactly, so that Task 3 of the one-day
#               formats can offer three reproductions on aggregate data (C6HJR
#               on the course panel, and these two). Each file follows the
#               deposited code of its analyst step by step, slips included:
#               - KQXUE (task2.do, osf.io/vycgd): every wave, EB 62.0 appended
#                 twice, the Turkish Cypriot sample pooled into Cyprus and
#                 Northern Ireland into the United Kingdom, the cosmopolitan
#                 share of each respondent's mentions with respondents who
#                 mention nothing left out, unweighted country-year means, and
#                 unemployment as a share of the population aged 20-64.
#               - 018OL (multi100_Teney2016_task-2.Rmd, osf.io/nz7ka): the mean
#                 of the five cosmopolitan items, found in each file by its
#                 label, averaged in cells of country, year and wave; Northern
#                 Ireland and the Turkish Cypriot sample fall out at the EU-27
#                 filter; the year comes from the names of the files of the
#                 analyst, which put EB 79.3 (spring 2013) in 2012; and
#                 unemployment of those aged 15-74 in the active population.
#               Both unemployment series are cut from the une_rt_a_h table of
#               Eurostat as published in December 2023, which is the vintage
#               on which KQXUE reproduces: Eurostat later revised Portugal
#               2013 (12.9 to 13.0 on the KQXUE slice). The script stops unless
#               both recorded statistics come back to the third decimal.
# Reads:        <raw_dir>/ZA*.dta (GESIS scientific-use files, local only),
#               data-raw/eu-frames/eurostat/une_rt_a_h_2023-12-22.tsv.gz,
#               data-raw/eu-frames/eurostat/une_rt_a_h.tsv.gz (for the check
#               of the two vintages)
# Writes:       cases/eu-frames/data/analyst_KQXUE.csv (270 country-years),
#               cases/eu-frames/data/analyst_018OL.csv (420 country-wave cells);
#               aggregates only, which the GESIS terms allow to be published
# Author:       Chris Moreh
# Last updated: 2026-10-27
##############################################################################

source("R/eu-frames/_paths.R")

# KQXUE dates each wave by its fieldwork month (the `date` lines of task2.do);
# 018OL reads the year from the first four characters of its own file names
waves <- tibble::tribble(
  ~file,                ~eb,    ~year, ~year_018ol, ~cntry_v,   ~items_from, ~items_style,
  "ZA4056_v1-0-1.dta", "61.0",  2004,  2004,        "isocntry", 63,          "v",
  "ZA4229_v1-1-0.dta", "62.0",  2004,  2004,        "v7",       105,         "v",
  "ZA4411_v1-1-0.dta", "63.4",  2005,  2005,        "v7",       89,          "v",
  "ZA4414_v1-1-0.dta", "64.2",  2005,  2005,        "v7",       118,         "v",
  "ZA4506_v1-0-1.dta", "65.2",  2006,  2006,        "v7",       98,          "v",
  "ZA4530_v2-1-0.dta", "67.2",  2007,  2007,        "v7",       130,         "v",
  "ZA4744_v5-0-0.dta", "69.2",  2008,  2008,        "v7",       218,         "v",
  "ZA4819_v3-0-2.dta", "70.1",  2008,  2008,        "v7",       234,         "v",
  "ZA4994_v3-0-0.dta", "72.4",  2009,  2009,        "v7",       221,         "v",
  "ZA5234_v2-0-1.dta", "73.4",  2010,  2010,        "v7",       277,         "v",
  "ZA5449_v2-2-0.dta", "74.2",  2010,  2010,        "v7",       306,         "v",
  "ZA5481_v2-0-1.dta", "75.3",  2011,  2011,        "v7",       315,         "v",
  "ZA5567_v2-0-1.dta", "76.3",  2011,  2011,        "isocntry", 0,           "qa12",
  "ZA5612_v2-0-0.dta", "77.3",  2012,  2012,        "isocntry", 0,           "qa15",
  "ZA5685_v2-0-0.dta", "78.1",  2012,  2012,        "isocntry", 0,           "qa13",
  "ZA5689_v2-0-0.dta", "79.3",  2013,  2012,        "isocntry", 0,           "qa14"
)

eu27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB",
          "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
          "SE", "SI", "SK")

# the first three letters of each item label after "EU MEANING: ", which is how
# 018OL names the items; the fourteenth reads EXT or, from EB 77.3, NOT
item_stems <- c("PEA", "ECO", "DEM", "SOC", "TRA", "CUL", "STR", "EUR", "UNE", "BUR",
                "WAS", "LOS", "MOR")

## ---- the battery of every wave --------------------------------------------
## The fourteen items sit at the same positions in all sixteen files, and the
## check on their labels shows that selecting them by label, as 018OL does,
## and by position, as KQXUE does, picks the same columns.

resp <- purrr::map(seq_len(nrow(waves)), \(i) {
  w <- waves[i, ]
  items <- if (w$items_style == "v") paste0("v", w$items_from + 0:13) else
    paste0(w$items_style, "_", 1:14)
  d <- haven::read_dta(file.path(raw_dir, w$file), col_select = dplyr::all_of(c(w$cntry_v, items)))
  stems <- unname(substr(sub(".*EU MEANING: ", "",
                             purrr::map_chr(d[items], \(x) attr(x, "label"))), 1, 3))
  stopifnot("the battery is not where the wave table puts it" =
              identical(stems[1:13], item_stems) && stems[14] %in% c("EXT", "NOT"))
  d |>
    dplyr::select(dplyr::all_of(c(w$cntry_v, items))) |>
    purrr::set_names(c("iso", paste0("it", 1:14))) |>
    dplyr::mutate(iso = as.character(iso),
                  dplyr::across(dplyr::starts_with("it"), \(x) as.numeric(x)),
                  eb = w$eb, year = w$year, year_018ol = w$year_018ol)
}) |>
  dplyr::bind_rows()

stopifnot("framing items not coded 0/1" =
            all(unlist(resp[paste0("it", 1:14)]) %in% c(0, 1)))

## ---- the unemployment series, December 2023 vintage -----------------------
## The bulk file of 2023 has the old layout: one key column "age,unit,sex,geo\time"
## and the years in descending order, with flags after a space and ":" for a
## missing cell. Eurostat writes Greece EL and the United Kingdom UK.

une_2023 <- readr::read_tsv("data-raw/eu-frames/eurostat/une_rt_a_h_2023-12-22.tsv.gz",
                            col_types = readr::cols(.default = "c"), progress = FALSE) |>
  purrr::set_names(\(x) trimws(sub("^.*\\\\.*$", "key", x))) |>
  tidyr::separate_wider_delim(key, delim = ",", names = c("age", "unit", "sex", "geo")) |>
  tidyr::pivot_longer(-c(age, unit, sex, geo), names_to = "year", values_to = "unemp") |>
  dplyr::mutate(year = as.integer(year),
                unemp = suppressWarnings(as.numeric(sub(" .*$", "", trimws(unemp)))),
                cntry = dplyr::case_when(geo == "EL" ~ "GR", geo == "UK" ~ "GB", .default = geo))

# the 2026 retrieval has a leading freq column, so its key has five fields
une_2026 <- readr::read_tsv("data-raw/eu-frames/eurostat/une_rt_a_h.tsv.gz",
                            col_types = readr::cols(.default = "c"), progress = FALSE) |>
  purrr::set_names(\(x) trimws(sub("^.*\\\\.*$", "key", x))) |>
  tidyr::separate_wider_delim(key, delim = ",", names = c("freq", "age", "unit", "sex", "geo")) |>
  tidyr::pivot_longer(-c(freq, age, unit, sex, geo), names_to = "year", values_to = "unemp") |>
  dplyr::mutate(year = as.integer(year),
                unemp = suppressWarnings(as.numeric(sub(" .*$", "", trimws(unemp)))),
                cntry = dplyr::case_when(geo == "EL" ~ "GR", geo == "UK" ~ "GB", .default = geo))

slice_kqxue <- une_2023 |>
  datawizard::data_filter(age == "Y20-64" & unit == "PC_POP" & sex == "T" &
                            cntry %in% eu27 & year %in% 2004:2013) |>
  datawizard::data_select(select = c("cntry", "year", "unemp"))
slice_018ol <- une_2023 |>
  datawizard::data_filter(age == "Y15-74" & unit == "PC_ACT" & sex == "T" &
                            cntry %in% eu27 & year %in% 2004:2012) |>
  datawizard::data_select(select = c("cntry", "year", "unemp"))

# The two vintages agree on both slices except Portugal 2013, which only the
# KQXUE slice uses
vintage_gap <- dplyr::bind_rows(
  dplyr::mutate(slice_kqxue, slice = "Y20-64 PC_POP"),
  dplyr::mutate(slice_018ol, slice = "Y15-74 PC_ACT")) |>
  dplyr::left_join(
    dplyr::bind_rows(
      une_2026 |> dplyr::filter(age == "Y20-64", unit == "PC_POP", sex == "T") |>
        dplyr::transmute(cntry, year, slice = "Y20-64 PC_POP", unemp_2026 = unemp),
      une_2026 |> dplyr::filter(age == "Y15-74", unit == "PC_ACT", sex == "T") |>
        dplyr::transmute(cntry, year, slice = "Y15-74 PC_ACT", unemp_2026 = unemp)),
    by = c("cntry", "year", "slice")) |>
  datawizard::data_filter(abs(unemp - unemp_2026) > 1e-9)
stopifnot("the two vintages of une_rt_a_h differ beyond Portugal 2013" =
            nrow(vintage_gap) == 1 && vintage_gap$cntry == "PT" && vintage_gap$year == 2013)

## ---- KQXUE: country-years ---------------------------------------------------
## task2.do appends the temporary file of EB 62.0 twice (lines 441-442), so
## every respondent of autumn 2004 counts twice in the 2004 means of the
## fifteen older member states; EB 61.0 holds no sample of the newer states,
## whose 2004 means are unchanged by the duplicate. A respondent who mentions
## none of the thirteen items has a zero denominator, which Stata turns into a
## missing share, so the collapse leaves that respondent out.

kqxue_resp <- resp |>
  datawizard::data_modify(cntry = datawizard::recode_values(
    iso, recode = list(DE = c("DE-E", "DE-W"), GB = c("GB-GBN", "GB-NIR"), CY = "CY-TCC"))) |>
  datawizard::data_filter(cntry %in% eu27) |>
  dplyr::mutate(n_all   = datawizard::row_sums(dplyr::pick(it1:it7, it9:it14)),
                n_cosmo = datawizard::row_sums(dplyr::pick(it1, it3, it5, it6, it7)),
                cosmo   = dplyr::if_else(n_all == 0, NA_real_, n_cosmo / n_all))

kqxue_once <- kqxue_resp |>
  datawizard::data_filter(!is.na(cosmo)) |>
  datawizard::data_summary(cosmo_once = mean(cosmo), n_once = length(cosmo),
                           by = c("cntry", "year"))

kqxue <- kqxue_resp |>
  dplyr::bind_rows(datawizard::data_filter(kqxue_resp, eb == "62.0")) |>
  datawizard::data_filter(!is.na(cosmo)) |>
  datawizard::data_summary(cosmo = mean(cosmo), n = length(cosmo), by = c("cntry", "year")) |>
  dplyr::left_join(kqxue_once, by = c("cntry", "year")) |>
  dplyr::left_join(slice_kqxue, by = c("cntry", "year")) |>
  # r-style: keep – dplyr::arrange() sorts in the C locale on every machine
  dplyr::arrange(cntry, year) |>
  datawizard::data_select(select = c("cntry", "year", "cosmo", "n", "cosmo_once", "n_once", "unemp"))

stopifnot("the KQXUE panel is not 270 complete country-years" =
            nrow(kqxue) == 270 && !anyNA(kqxue) && sum(kqxue$n) == 423328)

## ---- 018OL: country-wave cells ----------------------------------------------
## The year that the cells carry, and on which unemployment is merged, is the
## year of the file names of 018OL; year_fieldwork keeps the true one.

o18 <- resp |>
  datawizard::data_modify(cntry = datawizard::recode_values(
    iso, recode = list(DE = c("DE-E", "DE-W"), GB = "GB-GBN"))) |>
  datawizard::data_filter(cntry %in% eu27) |>
  dplyr::mutate(pos = datawizard::row_means(dplyr::pick(it1, it3, it5, it6, it7))) |>
  datawizard::data_summary(pos = mean(pos), n = length(pos),
                           by = c("year_018ol", "year", "cntry", "eb")) |>
  # dplyr::rename(), which renames all three at once; one at a time, year would be taken twice
  dplyr::rename(year_fieldwork = year, year = year_018ol) |>
  dplyr::mutate(wave = paste("EB", eb)) |>
  dplyr::left_join(slice_018ol, by = c("cntry", "year")) |>
  # r-style: keep – dplyr::arrange() sorts in the C locale on every machine
  dplyr::arrange(cntry, wave) |>
  datawizard::data_select(select = c("cntry", "wave", "year", "year_fieldwork", "pos", "n", "unemp"))

stopifnot("the 018OL file is not 420 complete cells" = nrow(o18) == 420 && !anyNA(o18))

## ---- the recorded statistics -------------------------------------------------
## KQXUE fitted xtreg cosmo unemployment, fe: the within estimator with
## conventional errors on 270 - 27 - 1 = 242 df, which a linear model with a
## dummy for each country reproduces. 018OL fitted lmer() with random
## intercepts for country, year and wave and read the Satterthwaite df of
## lmerTest (Kuznetsova, Brockhoff and Christensen 2017, Journal of
## Statistical Software); its rescaling of the outcome to 0-1 changes the
## coefficient but neither t nor df.

fit_kqxue <- stats::lm(cosmo ~ unemp + factor(cntry), data = kqxue)
fit_kqxue_once <- stats::lm(cosmo_once ~ unemp + factor(cntry), data = kqxue)
fit_018ol <- lmerTest::lmer(pos ~ unemp + (1 | cntry) + (1 | year) + (1 | wave), data = o18)
fit_018ol_field <- lmerTest::lmer(pos ~ unemp + (1 | cntry) + (1 | year_fieldwork) + (1 | wave),
                                  data = o18 |>
                                    dplyr::select(-unemp) |>
                                    dplyr::left_join(
                                      une_2023 |>
                                        dplyr::filter(age == "Y15-74", unit == "PC_ACT", sex == "T") |>
                                        dplyr::select(cntry, year_fieldwork = year, unemp),
                                      by = c("cntry", "year_fieldwork")))

repro <- list(
  "KQXUE, as deposited"                   = fit_kqxue,
  "KQXUE, EB 62.0 counted once"           = fit_kqxue_once,
  "018OL, as deposited"                   = fit_018ol,
  "018OL, EB 79.3 in its fieldwork year"  = fit_018ol_field) |>
  purrr::imap(\(m, label) {
    p <- parameters::model_parameters(m, keep = "^unemp$", effects = "fixed",
                                      ci_method = if (inherits(m, "lm")) "wald" else "satterthwaite")
    tibble::tibble(analysis = label, b = p$Coefficient, se = p$SE, t = p$t, df = p$df_error)
  }) |>
  purrr::list_rbind()

stopifnot("KQXUE does not reproduce t = -7.169 on 242 df" =
            round(repro$t[1], 3) == -7.169 && repro$df[1] == 242,
          "018OL does not reproduce t = -1.206 on 396.9 df" =
            round(repro$t[3], 3) == -1.206 && round(repro$df[3], 1) == 396.9)

# r-style: keep – readr writes the bytes that the site and the workspace compare
readr::write_csv(kqxue, out_path("analyst_KQXUE.csv"))
readr::write_csv(o18, out_path("analyst_018OL.csv"))

print(repro, width = 100)
