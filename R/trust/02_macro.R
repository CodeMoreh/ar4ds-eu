################################################################################
# Title:        02_macro.R
# Purpose:      The country-round and country variables of the original model
#               (far-right vote share, social protection expenditure, quality of
#               governance, GDP per head, unemployment, long-term immigration),
#               one row per country-round of the extract. They enter only the
#               multilevel models off the curve: fixed effects for country-round
#               absorb every one of them.
# Reads:        data-raw/trust/mclaren_appendix.csv; the World Bank (WGI, WDI)
#               and Eurostat series, fetched once into data-raw/trust/ as small
#               committed snapshots
# Writes:       cases/trust/data/trust_macro.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

RAW <- "data-raw/trust"
ISO <- tibble::tribble(
  ~cntry, ~iso3,  ~eurostat,
  "AT",   "AUT",  "AT",
  "BE",   "BEL",  "BE",
  "CH",   "CHE",  "CH",
  "DE",   "DEU",  "DE",
  "DK",   "DNK",  "DK",
  "ES",   "ESP",  "ES",
  "FI",   "FIN",  "FI",
  "FR",   "FRA",  "FR",
  "GB",   "GBR",  "UK",
  "GR",   "GRC",  "EL",
  "IE",   "IRL",  "IE",
  "IT",   "ITA",  "IT",
  "NL",   "NLD",  "NL",
  "NO",   "NOR",  "NO",
  "PT",   "PRT",  "PT",
  "SE",   "SWE",  "SE"
)
YEARS <- 2000:2009

# World Bank API (version 2): one indicator for the 16 countries and the OECD
# members aggregate (OED), 2000-2009.
wb_series <- function(indicator, source = NULL) {
  url <- paste0("https://api.worldbank.org/v2/country/", paste(c(ISO$iso3, "OED"), collapse = ";"),
                "/indicator/", indicator, "?format=json&per_page=1000&date=",
                min(YEARS), ":", max(YEARS), if (!is.null(source)) paste0("&source=", source))
  rows <- jsonlite::fromJSON(url)[[2]]
  tibble::tibble(iso3 = rows$countryiso3code, year = as.integer(rows$date), value = rows$value)
}

# Eurostat dissemination API, JSON-stat: the values are stored in the order of
# the dimensions, so a value's position gives its country and year.
eurostat_social_protection <- function() {
  url <- paste0("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/spr_exp_type",
                "?format=JSON&lang=EN&spdeps=TOTAL&unit=EUR_HAB&",
                paste0("geo=", ISO$eurostat, collapse = "&"),
                "&sinceTimePeriod=", min(YEARS), "&untilTimePeriod=", max(YEARS))
  js <- jsonlite::fromJSON(url)
  geo <- names(sort(unlist(js$dimension$geo$category$index)))
  time <- names(sort(unlist(js$dimension$time$category$index)))
  cells <- tidyr::expand_grid(geo = geo, year = as.integer(time)) |>
    dplyr::mutate(position = as.character(dplyr::row_number() - 1))
  tibble::tibble(position = names(js$value), eur_per_head = unlist(js$value)) |>
    dplyr::left_join(cells, by = "position") |>
    dplyr::select(geo, year, eur_per_head)
}

# Snapshots: fetched when absent, committed, and read from disk on every later
# run. Delete a file to fetch it again. readr reads and writes them, because
# datawizard::data_read() and data_write() give other types and bytes.
snapshot <- function(file, fetch) {
  path <- file.path(RAW, file)
  if (!file.exists(path)) readr::write_csv(fetch(), path)
  readr::read_csv(path, show_col_types = FALSE)
}

wgi <- snapshot("worldbank_wgi.csv", \() {
  c(cc = "GOV_WGI_CC.EST", ge = "GOV_WGI_GE.EST", pv = "GOV_WGI_PV.EST",
    rq = "GOV_WGI_RQ.EST", rl = "GOV_WGI_RL.EST", va = "GOV_WGI_VA.EST") |>
    purrr::imap(\(code, name) dplyr::rename(wb_series(code, source = 3), !!name := value)) |>
    purrr::reduce(dplyr::full_join, by = c("iso3", "year")) |>
    dplyr::filter(iso3 != "OED") |>
    dplyr::arrange(iso3, year)
})

wdi <- snapshot("worldbank_wdi.csv", \() {
  dplyr::full_join(
    dplyr::rename(wb_series("NY.GDP.PCAP.PP.CD"), gdp_pc_ppp = value),
    dplyr::rename(wb_series("SL.UEM.TOTL.ZS"), unemployment = value),
    by = c("iso3", "year")
  ) |>
    dplyr::arrange(iso3, year)
})

social <- snapshot("eurostat_social_protection.csv", eurostat_social_protection)

# The governance index is the mean of the six indicators, missing where any
# one is. The WGI were published every second year before 2002, so 2001 has no
# value; a country-round whose reference year is 2001 takes the value of 2000,
# as the SCORE source-data reproduction did (osf.io/e48gd).
wgi_items <- c("cc", "ge", "pv", "rq", "rl", "va")
wgi_mean <- wgi |>
  dplyr::mutate(wgi = datawizard::row_means(dplyr::pick(dplyr::all_of(wgi_items)), min_valid = 6)) |>
  datawizard::data_select(c("iso3", "year", "wgi"))
wgi_2001 <- wgi_mean |>
  datawizard::data_filter(year == 2000) |>
  datawizard::data_modify(year = 2001L)
wgi_mean <- dplyr::bind_rows(wgi_mean, wgi_2001) |>
  datawizard::data_filter(!is.na(wgi))

# GDP per head as the paper measures it, "using the average OECD gdp/capita as
# the base": an index on which the OECD members average 100.
gdp_index <- wdi |>
  dplyr::mutate(oecd = gdp_pc_ppp[iso3 == "OED"], .by = year) |>
  datawizard::data_filter(iso3 != "OED") |>
  dplyr::transmute(iso3, year, gdp_index = 100 * gdp_pc_ppp / oecd, unemployment)

# The country-rounds of the extract: the 59 of the appendix of the paper, and
# Ireland in round 4, which the appendix lists without data (its fieldwork began
# in September 2009; Ireland had no far-right party in the appendix in any round).
appendix <- readr::read_csv(file.path(RAW, "mclaren_appendix.csv"), show_col_types = FALSE)
rounds <- appendix |>
  datawizard::data_filter(!is.na(fieldwork_start) | (cntry == "IE" & essround == 4)) |>
  datawizard::data_modify(from_appendix = !is.na(fieldwork_start),
                          fieldwork_start = dplyr::coalesce(fieldwork_start, 2009),
                          far_right_share = dplyr::coalesce(far_right_share, 0))

# Each series is read for the year before the start of fieldwork (the paper
# states the lag for social protection, GDP and unemployment); the governance
# index follows the same rule.
social <- datawizard::data_rename(social, select = c(eurostat = "geo"))

# dplyr for the joins on columns of different names, which datawizard::data_join() does not take
trust_macro <- rounds |>
  dplyr::left_join(ISO, by = "cntry") |>
  datawizard::data_modify(reference_year = as.integer(fieldwork_start - 1)) |>
  dplyr::left_join(social, by = c("eurostat", "reference_year" = "year")) |>
  dplyr::left_join(wgi_mean, by = c("iso3", "reference_year" = "year")) |>
  dplyr::left_join(gdp_index, by = c("iso3", "reference_year" = "year")) |>
  dplyr::transmute(cntry, essround, fieldwork_start, reference_year, from_appendix,
                   far_right_share, social_protection = round(eur_per_head, 2), wgi = round(wgi, 4),
                   gdp_index = round(gdp_index, 2), unemployment = round(unemployment, 3),
                   long_term_imm = as.integer(!cntry %in% c("GR", "ES", "PT", "IT", "IE"))) |>
  datawizard::data_arrange(c("cntry", "essround"))

stopifnot(nrow(trust_macro) == 60, !anyNA(trust_macro))

readr::write_csv(trust_macro, "cases/trust/data/trust_macro.csv")
