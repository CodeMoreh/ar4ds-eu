##############################################################################
# Title:        02_macro_series.R
# Purpose:      Assembles the unemployment series that the four Eurostat-using
#               Multi100 analysts merged, so that the measure axis can refit
#               the menu on each of them rather than on the World Bank series
#               that the course panel carries. Eurostat publishes unemployment
#               broken down by age band, sex and unit, and the choice among
#               those slices IS the predictor: each slice gives different
#               country-year values, so the band is the identity of the
#               variable being tested, not a covariate. The four slices are
#               documented in the deposits of the analysts (018OL names its
#               filter in code; KEVF1's is recovered from the merged file that
#               they deposited; KQXUE's from its keepdim() call; PRL47's is
#               inferred from the five band names in their code, since the
#               file that they merged is local and undeposited).
#               data-raw/eu-frames/eurostat/README.md records the retrieval.
# Reads:        une_rt_a_h.tsv.gz, tipsun20.tsv.gz, lfsa_urgan.tsv.gz and
#               nama_10_gdp_growth.tsv.gz in data-raw/eu-frames/eurostat/,
#               cases/eu-frames/data/EUframes_cy.csv
# Writes:       cases/eu-frames/data/macro/analyst_unemployment.csv (long: one
#               row per analyst x country x year, 2004-2013, on the country
#               codes of the panel),
#               cases/eu-frames/data/macro/predictor_measures.csv (the same
#               series keyed by measure rather than by analyst, plus a Eurostat
#               GDP growth series, for the measure axis)
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr)
})
source("R/eu-frames/_paths.R")
dir.create(out_path("macro"), showWarnings = FALSE, recursive = TRUE)

## ---- the wide TSV of Eurostat ---------------------------------------------
## The first header cell names the non-time dimensions, comma-separated, with
## "\TIME_PERIOD" appended; the rest are years padded with spaces. Values carry
## observation flags after a space ("10.7 b") and ":" marks a missing cell.

read_eurostat <- function(path) {
  raw  <- readr::read_tsv(path, col_types = readr::cols(.default = "c"),
                          progress = FALSE)
  dims <- strsplit(sub("\\\\.*$", "", names(raw)[1]), ",")[[1]]
  raw |>
    set_names(c("key", trimws(names(raw)[-1]))) |>
    separate_wider_delim(key, delim = ",", names = dims) |>
    pivot_longer(-all_of(dims), names_to = "year", values_to = "value") |>
    mutate(year  = as.integer(year),
           value = suppressWarnings(as.numeric(sub(" .*$", "", trimws(value))))) |>
    filter(!is.na(value))
}

une_h <- read_eurostat("data-raw/eu-frames/eurostat/une_rt_a_h.tsv.gz")
tips  <- read_eurostat("data-raw/eu-frames/eurostat/tipsun20.tsv.gz")
lfs   <- read_eurostat("data-raw/eu-frames/eurostat/lfsa_urgan.tsv.gz")

## ---- the four analyst slices ----------------------------------------------

pick <- function(tab, analyst, series, ...) {
  tab |>
    filter(freq == "A", ...) |>
    transmute(analyst_id = analyst, series = series, geo, year, unemp = value)
}

series <- bind_rows(
  pick(une_h, "018OL", "une_rt_a_h Y15-74 PC_ACT T",
       age == "Y15-74", unit == "PC_ACT", sex == "T"),
  pick(une_h, "KQXUE", "une_rt_a_h Y20-64 PC_POP T",
       age == "Y20-64", unit == "PC_POP", sex == "T"),
  pick(tips,  "KEVF1", "tipsun20 Y15-74 PC_ACT T",
       age == "Y15-74", unit == "PC_ACT", sex == "T"),
  pick(lfs,   "PRL47", "lfsa_urgan Y25-64 PC T TOTAL",
       age == "Y25-64", unit == "PC", sex == "T", citizen == "TOTAL")
)

## ---- onto the country codes of the panel ----------------------------------
## Eurostat writes Greece EL and the United Kingdom UK; the panel uses the ISO
## codes GR and GB. 018OL recodes the same pair in their own script.

panel <- datawizard::data_read("cases/eu-frames/data/EUframes_cy.csv", verbose = FALSE)

analyst_unemp <- series |>
  mutate(cntry = recode(geo, EL = "GR", UK = "GB")) |>
  filter(cntry %in% unique(panel$cntry), year %in% 2004:2013) |>
  select(analyst_id, series, cntry, year, unemp) |>
  bind_rows(
    panel |>
      transmute(analyst_id = "C6HJR",
                series = "World Bank SL.UEM.TOTL.NE.ZS",
                cntry, year, unemp)
  ) |>
  arrange(analyst_id, cntry, year)

datawizard::data_write(analyst_unemp, out_path("macro", "analyst_unemployment.csv"),
                       verbose = FALSE)

## ---- the same series keyed by measure, for the multiverse -----------------
## The measure axis crosses the canonical menu with the series, so the series
## are needed under names that describe the variable rather than the analyst
## who chose it. The Eurostat growth series carries no United Kingdom row:
## Eurostat has withdrawn UK national accounts, so choosing it costs a country.

growth_es <- read_eurostat("data-raw/eu-frames/eurostat/nama_10_gdp_growth.tsv.gz") |>
  mutate(cntry = recode(geo, EL = "GR", UK = "GB")) |>
  filter(cntry %in% unique(panel$cntry), year %in% 2004:2013) |>
  transmute(measure = "es_growth", predictor = "growth",
            source = "Eurostat nama_10_gdp B1GQ CLV_PCH_PRE",
            cntry, year, value)

measures <- analyst_unemp |>
  mutate(measure = recode(analyst_id,
                          C6HJR = "wb_nat",     `018OL` = "es_1574_act",
                          KQXUE = "es_2064_pop", PRL47  = "es_2564_lfs",
                          KEVF1 = "es_1574_tips"),
         predictor = "unemp") |>
  transmute(measure, predictor, source = series, cntry, year, value = unemp) |>
  bind_rows(
    panel |> transmute(measure = "wb_growth", predictor = "growth",
                       source = "World Bank NY.GDP.MKTP.KD.ZG",
                       cntry, year, value = growth),
    growth_es
  ) |>
  arrange(predictor, measure, cntry, year)

datawizard::data_write(measures, out_path("macro", "predictor_measures.csv"),
                       verbose = FALSE)

## ---- coverage and divergence from the World Bank series -------------------

analyst_unemp |>
  left_join(panel |> select(cntry, year, wb = unemp), by = c("cntry", "year")) |>
  summarise(cells      = n(),
            countries  = n_distinct(cntry),
            years      = paste0(min(year), "-", max(year)),
            mean_rate  = round(mean(unemp), 2),
            r_with_wb  = round(cor(unemp, wb), 4),
            mean_abs_d = round(mean(abs(unemp - wb)), 3),
            .by = c(analyst_id, series)) |>
  as.data.frame() |>
  print(row.names = FALSE)
