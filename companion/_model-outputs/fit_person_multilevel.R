###############################################################################
# Title:        Multilevel reference numbers from the real respondents
# Purpose:      Capture what the licensed file gives for the models the
#               individual-level companion page runs live on the twin, so the
#               page can show both and the reader can see whether they agree.
#               The statistics module already displays these fits as captured
#               text; what is written here is the numbers behind them, in a
#               form a page can read and compare against.
#
#               Three models, in the order the argument needs them: country
#               level only, which counts every respondent as fresh information
#               about a predictor that varies 270 times; country and
#               country-year, which does not; and a random slope, which asks
#               whether the effect differs by country.
#
# Reads:        the respondent file, through _load_respondents.R (licensed, local only)
# Writes:       companion/_model-outputs/person_multilevel_real.csv
#               Coefficients, variance components and degrees of freedom.
#               Aggregate, and committed.
# Author:       Chris Moreh
# Last updated: 2026-07-31
###############################################################################
source("companion/_model-outputs/_load_respondents.R")
ind <- ind |>
  datawizard::data_modify(cy = paste(cntry, year, sep = "_"))

specs <- list(
  two_level   = cosmo ~ unemp + (1 | cntry),
  three_level = cosmo ~ unemp + (1 | cntry) + (1 | cy),
  slopes      = cosmo ~ unemp + (1 + unemp | cntry) + (1 | cy)
)

summarise_fit <- function(m, label) {
  p  <- summary(m)$coefficients
  vc <- as.data.frame(lme4::VarCorr(m))  # r-style: keep – the VarCorr method lists the variance components as rows
  dplyr::bind_rows(
    tibble::tibble(model = label, kind = "fixed", term = rownames(p),
           value = p[, "Estimate"], se = p[, "Std. Error"],
           statistic = p[, "t value"], df = p[, "df"]),
    tibble::tibble(model = label, kind = "random",
           term = dplyr::if_else(is.na(vc$var2), paste0(vc$grp, ": ", vc$var1),
                         paste0(vc$grp, ": ", vc$var1, " ~ ", vc$var2)),
           value = vc$sdcor, se = NA, statistic = NA, df = NA),
    tibble::tibble(model = label, kind = "n", term = c("respondents", "cells", "countries"),
           value = c(nobs(m), dplyr::n_distinct(ind$cy), dplyr::n_distinct(ind$cntry)),
           se = NA, statistic = NA, df = NA))
}

out <- purrr::imap(specs, \(f, nm) summarise_fit(lmerTest::lmer(f, data = ind), nm)) |>
  purrr::list_rbind()

readr::write_csv(out |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 6))),
          "companion/_model-outputs/person_multilevel_real.csv")

# What the levels do to the evidence
out |>
  datawizard::data_filter(kind == "fixed", term == "unemp") |>
  datawizard::data_select(select = c("model", "value", "se", "statistic", "df")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) signif(x, 4))) |>
  print()
# The variance components
out |>
  datawizard::data_filter(kind == "random") |>
  datawizard::data_select(select = c("model", "term", "value")) |>
  datawizard::data_modify(value = signif(value, 4)) |>
  print()
