##############################################################################
# Title:        01_person_file.R
# Purpose:      Builds the respondent-level file from the 16 GESIS Eurobarometer
#               waves (ZA4994 for 2009). It carries both the relative-frequency
#               framing scales of the published design and the raw item counts
#               that they are built from, because the five Multi100 analysts
#               did not agree on which of the two is the outcome: KQXUE and
#               PRL47 model a share, KEVF1 models a raw count of five items,
#               018OL models the mean of the same five. Covariates follow the
#               audited wave mappings: W1 nation weight, D11 age, D10 gender,
#               the GESIS education recode in four bands. Macro variables are
#               joined from the course panel. No model is fitted here.
#               The two closing codes of the battery, OTHER (spontaneous) and
#               DK, travel with the items, because a respondent who mentions
#               none of the 13 items is one of three different people: someone
#               who said "don't know", someone whose answer was not on the
#               list, and someone with no tick at all. The Stata code of the
#               original study divided by the mentions total with no zero
#               guard, so those respondents were dropped; the Multi100
#               reanalyses scored them 0. Both conventions need the split.
#               The *_un columns are the sensitivity set of the original code
#               (eb_ml_ESR.do): every scale rebuilt without the item
#               "unemployment", which shares content with the predictor.
# Reads:        <raw_dir>/ZA*.dta (GESIS scientific-use files, local only),
#               cases/eu-frames/data/EUframes_cy.csv
# Writes:       _planning_data/person_level.rds (GESIS-derived microdata,
#               never committed or redistributed)
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(purrr)
})
source("R/eu-frames/_paths.R")

waves <- tibble::tribble(
  ~file,                ~eb,    ~year, ~cntry_v,    ~items_from, ~items_style, ~w1,   ~age,    ~gender, ~edu_rec, ~edu_scheme,
  "ZA4056_v1-0-1.dta", "61.0",  2004,  "isocntry",  63,   "v",   "v9",  "v331",  "v330",  "v329",  "cat11",
  "ZA4229_v1-1-0.dta", "62.0",  2004,  "v7",        105,  "v",   "v8",  "v429",  "v428",  "v427",  "cat11",
  "ZA4411_v1-1-0.dta", "63.4",  2005,  "v7",        89,   "v",   "v8",  "v411",  "v410",  "v409",  "cat11",
  "ZA4414_v1-1-0.dta", "64.2",  2005,  "v7",        118,  "v",   "v8",  "v440",  "v439",  "v438",  "cat11",
  "ZA4506_v1-0-1.dta", "65.2",  2006,  "v7",        98,   "v",   "v8",  "v3311", "v3310", "v3309", "cat11",
  "ZA4530_v2-1-0.dta", "67.2",  2007,  "v7",        130,  "v",   "v8",  "v549",  "v548",  "v547",  "cat11",
  "ZA4744_v5-0-0.dta", "69.2",  2008,  "v7",        218,  "v",   "v8",  "v768",  "v767",  "v766",  "cat11",
  "ZA4819_v3-0-2.dta", "70.1",  2008,  "v7",        234,  "v",   "v8",  "v671",  "v670",  "v669",  "cat11",
  "ZA4994_v3-0-0.dta", "72.4",  2009,  "v7",        221,  "v",   "v8",  "v585",  "v584",  "v583",  "cat11",
  "ZA5234_v2-0-1.dta", "73.4",  2010,  "v7",        277,  "v",   "v8",  "v556",  "v555",  "v554",  "cat11",
  "ZA5449_v2-2-0.dta", "74.2",  2010,  "v7",        306,  "v",   "v8",  "v603",  "v602",  "v601",  "cat11",
  "ZA5481_v2-0-1.dta", "75.3",  2011,  "v7",        315,  "v",   "v8",  "v616",  "v615",  "v614",  "cat11",
  "ZA5567_v2-0-1.dta", "76.3",  2011,  "isocntry",  0,    "qa12","w1",  "d11",   "d10",   "d8r2",  "band3",
  "ZA5612_v2-0-0.dta", "77.3",  2012,  "isocntry",  0,    "qa15","w1",  "d11",   "d10",   "d8r2",  "band3",
  "ZA5685_v2-0-0.dta", "78.1",  2012,  "isocntry",  0,    "qa13","w1",  "d11",   "d10",   "d8r2",  "band3",
  "ZA5689_v2-0-0.dta", "79.3",  2013,  "isocntry",  0,    "qa14","w1",  "d11",   "d10",   "d8r2",  "band3"
)

eu27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB",
          "GR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
          "SE", "SI", "SK")

# positions among the 14 EU-meaning items; Euro (8) is excluded throughout,
# following footnote 1 of the original paper
dim_idx <- list(cosmo = c(1, 3, 5, 6, 7), util = c(2, 4),
                comm  = c(9, 12, 13, 14),  lib  = c(10, 11))

# harmonised four-band education from the GESIS recode; 'no full-time
# education' (11) folds into the lowest band
edu_harmonise <- function(x, scheme) {
  x <- as.numeric(x)
  if (scheme == "cat11") {
    case_when(x %in% c(1, 2, 11) ~ "le15", x %in% 3:6 ~ "e16_19",
              x %in% 7:9 ~ "e20plus", x == 10 ~ "studying",
              TRUE ~ NA_character_)
  } else {
    case_when(x %in% c(1, 11) ~ "le15", x == 2 ~ "e16_19",
              x == 3 ~ "e20plus", x == 10 ~ "studying",
              TRUE ~ NA_character_)
  }
}

read_wave <- function(w) {
  # 14 items, then OTHER (spontaneous) and DK at the two following positions,
  # a layout that holds in all 16 files under both naming schemes
  items <- if (w$items_style == "v") {
    paste0("v", w$items_from:(w$items_from + 15))
  } else {
    paste0(w$items_style, "_", 1:16)
  }
  cols <- c(w$cntry_v, items, w$w1, w$age, w$gender, w$edu_rec)
  # read_dta returns the columns in file order, so the requested order is restored
  d <- read_dta(file.path(raw_dir, w$file), col_select = all_of(cols)) |>
    select(all_of(cols)) |>
    set_names(c("cntry", paste0("it", 1:14), "oth", "dk", "w1", "age", "gender", "edu_raw")) |>
    mutate(across(c(starts_with("it"), oth, dk), \(x) as.numeric(x)))
  vals <- unique(na.omit(unlist(d[c(paste0("it", 1:14), "oth", "dk")])))
  stopifnot("framing items not coded 0/1" = all(vals %in% c(0, 1)))
  d |>
    mutate(cntry = as.character(cntry),
           cntry = recode(cntry, "DE-E" = "DE", "DE-W" = "DE",
                          "GB-GBN" = "GB", "GB-NIR" = "GB"),
           eb = w$eb, year = w$year,
           w1 = as.numeric(w1),
           age = as.numeric(age),
           female = as.integer(as.numeric(gender) == 2),
           edu4 = edu_harmonise(edu_raw, w$edu_scheme)) |>
    select(-gender, -edu_raw) |>
    filter(cntry %in% eu27)
}

ind <- map(seq_len(nrow(waves)), \(i) read_wave(waves[i, ])) |> bind_rows()

# n_* are counts of mentions; the scales divide each by the respondent's own
# total, per Teney (2016: 623), scoring 0 for a respondent who mentions nothing
ind <- ind |>
  mutate(
    n_cosmo = rowSums(across(all_of(paste0("it", dim_idx$cosmo)))),
    n_util  = rowSums(across(all_of(paste0("it", dim_idx$util)))),
    n_comm  = rowSums(across(all_of(paste0("it", dim_idx$comm)))),
    n_lib   = rowSums(across(all_of(paste0("it", dim_idx$lib)))),
    n_all   = rowSums(across(all_of(paste0("it", c(1:7, 9:14))))),
    cosmo = ifelse(n_cosmo == 0, 0, n_cosmo / n_all),
    util  = ifelse(n_util  == 0, 0, n_util  / n_all),
    comm  = ifelse(n_comm  == 0, 0, n_comm  / n_all),
    lib   = ifelse(n_lib   == 0, 0, n_lib   / n_all),
    pos   = ifelse(n_cosmo + n_util == 0, 0, (n_cosmo + n_util) / n_all),
    neg   = ifelse(n_comm  + n_lib  == 0, 0, (n_comm  + n_lib)  / n_all),
    # who the respondents with no mention are; "other" takes precedence over
    # "dk" where both are ticked, since an "other" answer is an opinion
    oth = as.integer(oth == 1), dk = as.integer(dk == 1),
    noop_type = case_when(n_all > 0 ~ "answered", oth == 1 ~ "other",
                          dk == 1 ~ "dk", TRUE ~ "blank"),
    # the *_un set: item 9 ("unemployment") leaves the communitarian cluster and
    # the denominator, so a respondent who mentioned only that item has no
    # mentions left and falls to 0 under the same convention
    n_all_un  = n_all  - it9,
    n_comm_un = n_comm - it9,
    cosmo_un = ifelse(n_cosmo == 0, 0, n_cosmo / n_all_un),
    util_un  = ifelse(n_util  == 0, 0, n_util  / n_all_un),
    comm_un  = ifelse(n_comm_un == 0, 0, n_comm_un / n_all_un),
    lib_un   = ifelse(n_lib   == 0, 0, n_lib   / n_all_un),
    pos_un   = ifelse(n_cosmo + n_util == 0, 0, (n_cosmo + n_util) / n_all_un),
    neg_un   = ifelse(n_comm_un + n_lib == 0, 0, (n_comm_un + n_lib) / n_all_un)
  ) |>
  select(-starts_with("it"))

panel <- datawizard::data_read("cases/eu-frames/data/EUframes_cy.csv", verbose = FALSE)
ind <- ind |>
  left_join(panel |> select(cntry, year, unemp, growth, bailout),
            by = c("cntry", "year"))

dir.create(private_dir, showWarnings = FALSE)
saveRDS(ind, person_file)

## ---- validation: aggregation must reproduce the course panel ---------------

chk <- ind |>
  summarise(mcosmo = mean(cosmo), mutil = mean(util), mcomm = mean(comm),
            mlib = mean(lib), mpos = mean(pos), mneg = mean(neg),
            n_cy = n(), .by = c(cntry, year)) |>
  inner_join(panel, by = c("cntry", "year"), suffix = c("_new", "_pub"))

max_diff <- map_dbl(c("mcosmo", "mutil", "mcomm", "mlib", "mpos", "mneg"),
                    \(v) max(abs(chk[[paste0(v, "_new")]] - chk[[paste0(v, "_pub")]])))
stopifnot("the respondent file does not reproduce the panel" =
            nrow(chk) == 270 && all(max_diff < 1e-12) && all(chk$n_cy_new == chk$n_cy_pub))

tibble(respondents = nrow(ind),
       no_mention = sum(ind$n_all == 0),
       no_mention_without_item9 = sum(ind$n_all_un == 0),
       complete_cases = sum(complete.cases(ind[, c("age", "female", "edu4", "w1")]))) |>
  print()
print(table(ind$noop_type))
