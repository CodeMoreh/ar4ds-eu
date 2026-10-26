################################################################################
# Title:        00_party_codes.R
# Purpose:      The party codes behind two controls of the original study: a
#               vote for a party that sat in the national cabinet on the day of
#               the interview ('winner'), and a vote for one of the far-right
#               parties listed in the web appendix of the paper. Builds the
#               lookup tables that R/trust/01_extract.R applies to the ESS
#               party-vote questions of rounds 1-4.
# Reads:        the Party Facts crosswalk of ESS party codes (GitHub, pinned
#               commit), the Party Facts table of external party keys and the
#               ParlGov cabinet table, fetched once into data-raw/trust/cache/
# Writes:       data-raw/trust/party_codes.csv, data-raw/trust/cabinets.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

CACHE <- "data-raw/trust/cache"
SOURCES <- c(
  essprt = "https://raw.githubusercontent.com/hdigital/partyfactsdata/f336bca731a049e1d2b2d8865ac774144950e14e/import/essprtv/essprt-all.csv",
  external = "https://partyfacts.herokuapp.com/download/external-parties-csv/",
  cabinet = "https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv"
)

# The development tables of Party Facts and ParlGov change over time; the cache
# keeps the copy that built the committed lookups. Delete it to refresh.
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)
cached <- purrr::imap_chr(SOURCES, \(url, key) {
  path <- file.path(CACHE, paste0(key, ".csv"))
  if (!file.exists(path)) utils::download.file(url, path, mode = "wb", quiet = TRUE)
  path
})

COUNTRIES <- c(AT = "AUT", BE = "BEL", CH = "CHE", DE = "DEU", DK = "DNK", ES = "ESP",
               FI = "FIN", FR = "FRA", GB = "GBR", GR = "GRC", IE = "IRL", IT = "ITA",
               NL = "NLD", NO = "NOR", PT = "PRT", SE = "SWE")

# ESS party codes of the party-vote questions (not the party-closeness ones)
essprt <- readr::read_csv(cached[["essprt"]], show_col_types = FALSE) |>
  datawizard::data_filter(ess_cntry %in% names(COUNTRIES), essround <= 4,
                          stringr::str_starts(ess_variable, "prtv")) |>
  dplyr::transmute(cntry = ess_cntry, essround, variable = ess_variable, code = ess_party_id,
                   party = ess_party, technical = !is.na(technical), partyfacts_id)

# Italy, round 2, was released as a separate country file (ESS2IT) that Party
# Facts does not cover. Its list repeats the 2001 parties of round 1 under the
# same codes, plus three codes of its own.
italy_r2 <- essprt |>
  datawizard::data_filter(cntry == "IT", essround == 1) |>
  datawizard::data_modify(essround = 2, variable = "prtvtait") |>
  dplyr::bind_rows(tibble::tribble(
    ~cntry, ~essround, ~variable,  ~code, ~party,                      ~technical, ~partyfacts_id,
    "IT",   2,         "prtvtait", 6,     "SVP (Sudtirol Volkspartei)", FALSE,      NA,
    "IT",   2,         "prtvtait", 18,    "Scheda bianca",              TRUE,       NA,
    "IT",   2,         "prtvtait", 19,    "Ulivo",                      FALSE,      NA
  ))
essprt <- dplyr::bind_rows(essprt, italy_r2)

parlgov_keys <- readr::read_csv(cached[["external"]], show_col_types = FALSE,
                                col_select = c(dataset_key, dataset_party_id, partyfacts_id)) |>
  datawizard::data_filter(dataset_key == "parlgov") |>
  dplyr::transmute(partyfacts_id, parlgov_id = as.integer(dataset_party_id))

# Electoral alliances that ParlGov records as cabinet parties of their own: the
# ESS codes of their member parties are linked to the alliance by hand.
alliances <- tibble::tribble(
  ~cntry, ~essround, ~code, ~parlgov_id, ~why,
  "BE",   1,         13,    454L,        "PRL-FDF list = ParlGov PRL (Verhofstadt I)",
  "BE",   1,         13,    915L,        "PRL-FDF list, MR from 2002 (Verhofstadt II)",
  "BE",   1,         3,     1113L,       "SP, SP.A-Spirit from 2003 (Verhofstadt II)",
  "FI",   1,         10,    1292L,       "Left Alliance = ParlGov DL|VAS (Lipponen III)",
  "BE",   4,         2,     972L,        "CD&V + N-VA list = ParlGov CD+NVA (Verhofstadt IV, Leterme I)",
  "BE",   4,         2,     723L,        "CD&V + N-VA list, CD&V alone in Van Rompuy and Leterme II",
  "BE",   4,         5,     1113L,       "SP.A + Spirit = ParlGov SPa+Spi",
  "BE",   4,         8,     1110L,       "Open VLD + Vivant = ParlGov PVV|VLD",
  "IT",   2,         1,     1048L,       "DS ran in L'Ulivo, ParlGov CeS (Prodi II)",
  "IT",   2,         2,     1048L,       "La Margherita ran in L'Ulivo, ParlGov CeS (Prodi II)",
  "IT",   2,         19,    1048L,       "Ulivo = ParlGov CeS (Prodi II)",
  "IT",   2,         4,     910L,        "Girasole joined the Greens (FdV, Prodi II)",
  "IT",   2,         10,    226L,        "CCD-CDU merged into the UDC, ParlGov UC (Berlusconi III)"
)

# The far-right parties of the web appendix of the paper (McLaren 2011,
# eprints.nottingham.ac.uk/1566), matched to the ESS codes by name. The appendix
# lists no far-right party for Finland, Greece in rounds 1-2, Ireland and Spain;
# its far-right parties of Sweden (Sweden Democrats), the United Kingdom in
# rounds 1, 2 and 4 (British National Party), Portugal in rounds 1-3 (National
# Renewal Party), the Netherlands in round 3 (One NL, Party for Freedom) and
# Norway (The Democrats) have no code of their own in the ESS lists, so their
# voters count as not voting far right. The pattern guards the match by name.
far_right <- tibble::tribble(
  ~cntry, ~essround, ~code, ~pattern,
  "AT", 1, 3,  "FP",
  "AT", 2, 3,  "FP",
  "AT", 3, 3,  "FP",
  "BE", 1, 8,  "Vlaams Blok",
  "BE", 1, 15, "(?i)front national",
  "BE", 2, 7,  "Vlaams Blok",
  "BE", 2, 11, "(?i)front national",
  "BE", 3, 7,  "Vlaams Blok",
  "BE", 3, 11, "(?i)front national",
  "BE", 4, 7,  "Vlaams Belang",
  "BE", 4, 11, "(?i)front national",
  "CH", 1, 4,  "Swiss People",
  "CH", 2, 4,  "Swiss People",
  "CH", 3, 4,  "Swiss People",
  "CH", 4, 4,  "(?i)swiss people",
  "DE", 1, 6,  "Republi",
  "DE", 2, 6,  "Republi",
  "DE", 3, 6,  "Republi",
  "DE", 3, 7,  "NPD",
  "DE", 4, 6,  "Republi",
  "DE", 4, 7,  "NPD",
  "DK", 1, 6,  "Dansk Folkeparti",
  "DK", 2, 6,  "Dansk Folkeparti",
  "DK", 3, 6,  "Dansk Folkeparti",
  "DK", 4, 5,  "Dansk Folkeparti",
  "FR", 1, 3,  "Front National|FN",
  "FR", 2, 3,  "Front National|FN",
  "FR", 3, 3,  "(?i)front national|FN",
  "FR", 4, 2,  "Front National|FN",
  "GB", 3, 7,  "British National",
  "GR", 4, 5,  "LAOS",
  "IT", 1, 9,  "Alleanza Nazionale",
  "IT", 2, 9,  "Alleanza Nazionale",
  "NL", 1, 4,  "Fortuyn",
  "NL", 2, 4,  "Fortuyn",
  "NL", 3, 4,  "Fortuyn",
  "NL", 4, 4,  "Fortuyn",
  "NL", 4, 11, "^Party for Freedom$",
  "NO", 1, 8,  "Fremskritt",
  "NO", 2, 8,  "Fremskritt",
  "NO", 3, 8,  "Fremskritt",
  "NO", 4, 8,  "Fremskritt",
  "PT", 4, 8,  "PNR|Renovador"
) |>
  datawizard::data_modify(far_right = 1L)

# Germany asks two votes (prtv*de1, prtv*de2) with one list of codes, so the
# ParlGov keys and the far-right flags attach to a code of a country-round.
# dplyr for the joins, whose row order and many-to-many check datawizard::data_join() does not share
parlgov_ids <- essprt |>
  dplyr::left_join(parlgov_keys, by = "partyfacts_id", relationship = "many-to-many") |>
  datawizard::data_select(c("cntry", "essround", "code", "parlgov_id")) |>
  dplyr::bind_rows(datawizard::data_select(alliances, c("cntry", "essround", "code", "parlgov_id"))) |>
  datawizard::data_filter(!is.na(parlgov_id)) |>
  dplyr::distinct() |>
  datawizard::data_summary(parlgov_id = paste(sort(parlgov_id), collapse = ";"),
                           by = c("cntry", "essround", "code"))

party_codes <- essprt |>
  dplyr::left_join(parlgov_ids, by = c("cntry", "essround", "code")) |>
  dplyr::left_join(far_right, by = c("cntry", "essround", "code")) |>
  datawizard::data_modify(far_right = dplyr::coalesce(far_right, 0L))

matched <- party_codes |>
  datawizard::data_filter(far_right == 1) |>
  dplyr::distinct(cntry, essround, code, party, pattern) |>
  datawizard::data_modify(ok = purrr::map2_lgl(party, pattern, \(p, re) stringr::str_detect(p, re)))
stopifnot(nrow(matched) == nrow(far_right), all(matched$ok))

party_codes <- party_codes |>
  datawizard::data_select(c("cntry", "essround", "variable", "code", "party", "technical",
                            "partyfacts_id", "parlgov_id", "far_right")) |>
  datawizard::data_arrange(c("cntry", "essround", "variable", "code"))

# Cabinets that could be in office during the fieldwork of rounds 1-4 (2002-2010):
# the cabinet in office on 1 January 2001 and every later one up to 2010, with
# their member parties.
# The second filter runs within each country, which dplyr expresses with .by
cabinets <- readr::read_csv(cached[["cabinet"]], show_col_types = FALSE) |>
  datawizard::data_filter(country_name_short %in% COUNTRIES, cabinet_party == 1,
                          start_date <= as.Date("2010-12-31")) |>
  dplyr::filter(start_date >= max(start_date[start_date <= as.Date("2001-01-01")]),
                .by = country_name_short) |>
  dplyr::transmute(cntry = names(COUNTRIES)[match(country_name_short, COUNTRIES)],
                   cabinet = cabinet_name, start_date, caretaker,
                   parlgov_id = as.integer(party_id), party = party_name_short) |>
  # dplyr sorts the party names in the C locale, as the committed file has them;
  # datawizard::data_arrange() sorts in the locale of the machine
  dplyr::arrange(cntry, start_date, party)

# readr writes the bytes of the committed files; datawizard::data_write() does not
readr::write_csv(party_codes, "data-raw/trust/party_codes.csv", na = "")
readr::write_csv(cabinets, "data-raw/trust/cabinets.csv", na = "")
