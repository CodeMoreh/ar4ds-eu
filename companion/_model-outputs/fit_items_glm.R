###############################################################################
# Title:        Binary-outcome models on single EU-meaning items
# Purpose:      Compute the tables the binary-outcomes companion page displays:
#               one item of the EU-meaning battery as the outcome, modelled as
#               a probability. Prevalence by education band and by year; a
#               logit and a probit of the same item on the respondent
#               characteristics, side by side, with odds ratios and average
#               marginal effects; the same logit with cluster-robust errors
#               and with country and year fixed effects, where the national
#               unemployment rate enters; the cross-level interaction between
#               education and unemployment on one item; and a random-intercept
#               logit for the cluster structure. Every table is an aggregate
#               summary of the kind a published appendix carries.
# Reads:        data_private/eb_respondents.rds  (licensed, local only; the
#               file that prep/01_build_respondents.R builds)
# Writes:       companion/_model-outputs/items_glm_prevalence.csv
#               companion/_model-outputs/items_glm_coefficients.csv
#               companion/_model-outputs/items_glm_ame.csv
#               companion/_model-outputs/items_glm_interaction.csv
#               companion/_model-outputs/items_glm_random.csv
# Author:       Chris Moreh
# Last updated: 2026-09-09
###############################################################################
raw <- Sys.getenv("EB_RESPONDENTS", unset = "_workspaces/eu-frames/data_private/eb_respondents.rds")
d <- readRDS(raw) |>
  datawizard::data_modify(cy    = paste(cntry, year, sep = "_"),
         edu3  = factor(edu3, levels = c("low", "mid", "high")),
         ses   = factor(ses, levels = c("manual", "self_employed", "manager",
                                        "white_collar", "house_person",
                                        "unemployed", "retired", "student")),
         age_c = age - mean(age, na.rm = TRUE)) |>
  datawizard::data_filter(!is.na(edu3), !is.na(female), !is.na(age))

# The macro series join by country-year from the committed panel.
panel <- readr::read_csv("cases/eu-frames/data/EUframes_cy.csv", show_col_types = FALSE) |>
  datawizard::data_select(select = c("cntry", "year", "unemp", "growth"))
d <- d |>
  dplyr::left_join(panel, by = c("cntry", "year")) |>
  datawizard::data_modify(unemp_c = unemp - mean(panel$unemp))

# Two items carry the page: "unemployment" (it9), the item that shares content
# with the predictor, and "peace" (it1), the most-named positive item after freedom to travel.
items <- c(it9 = "Unemployment", it1 = "Peace")

# --- 1. prevalence ------------------------------------------------------------
# Two named lists are concatenated and unnamed before binding: bind_rows()
# given two named lists of the same names nests them into a tibble of tibbles.
prevalence <- c(
  purrr::imap(items, \(lab, it) d |>
    dplyr::summarise(share = mean(.data[[it]]), n = dplyr::n(), .by = edu3) |>
    dplyr::mutate(item = it, label = lab, by = "education", group = as.character(edu3)) |>
    dplyr::select(item, label, by, group, share, n)),
  purrr::imap(items, \(lab, it) d |>
    dplyr::summarise(share = mean(.data[[it]]), n = dplyr::n(), .by = year) |>
    dplyr::mutate(item = it, label = lab, by = "year", group = as.character(year)) |>
    dplyr::select(item, label, by, group, share, n))
) |> unname() |> purrr::list_rbind()

# --- 2. logit and probit, side by side ------------------------------------------
# Same right-hand side under both links, conventional errors first, so the
# page can show that the two links order the coefficients identically and
# differ by a scale factor, while the average marginal effects agree closely.
rhs <- "edu3 + age_c + female + ses + urban"
fit_link <- function(it, link) {
  fixest::feglm(as.formula(paste(it, "~", rhs)), data = d, family = binomial(link = link))
}
tidy_fit <- function(m, it, link, vcov_label) {
  ct <- fixest::coeftable(m)
  tibble::tibble(item = it, link = link, vcov = vcov_label, term = rownames(ct),
         estimate = ct[, 1], se = ct[, 2], z = ct[, 3], p = ct[, 4])
}

coefficients <- purrr::imap(items, \(lab, it) {
  logit  <- fit_link(it, "logit")
  probit <- fit_link(it, "probit")
  # The same logit with errors clustered by country-year, which is the
  # structure the respondents actually sit in.
  logit_cl <- summary(logit, vcov = ~cy)
  ct <- fixest::coeftable(logit_cl)
  clustered <- tibble::tibble(item = it, link = "logit", vcov = "clustered by country-year",
                      term = rownames(ct), estimate = ct[, 1], se = ct[, 2],
                      z = ct[, 3], p = ct[, 4])
  dplyr::bind_rows(tidy_fit(logit, it, "logit", "conventional"),
            tidy_fit(probit, it, "probit", "conventional"),
            clustered)
}) |> purrr::list_rbind() |>
  datawizard::data_modify(odds_ratio = dplyr::if_else(link == "logit", exp(estimate), NA_real_))

out <- "companion/_model-outputs/"
readr::write_csv(prevalence,   paste0(out, "items_glm_prevalence.csv"))
readr::write_csv(coefficients, paste0(out, "items_glm_coefficients.csv"))

# --- 3. average marginal effects ------------------------------------------------
# What a coefficient on the link scale is worth in percentage points, averaged
# over the respondents as they are: the quantity a reader can compare across
# links and across models. The average is taken over a random fifty thousand
# of the respondents rather than all four hundred thousand, which changes the
# third decimal at most and turns a long computation into a short one; the
# models themselves are fitted on everyone.
set.seed(20260909)
ame_rows <- dplyr::slice_sample(d, n = 50000)
ame <- purrr::imap(items, \(lab, it) {
  purrr::map(c("logit", "probit"), \(link) {
    m <- fit_link(it, link)
    marginaleffects::avg_slopes(m, variables = c("edu3", "age_c", "female"), newdata = ame_rows,
               vcov = ~cy) |>
      tibble::as_tibble() |>
      dplyr::transmute(item = it, link = link, term, contrast,
                estimate, se = std.error, p = p.value)
  }) |> purrr::list_rbind()
}) |> purrr::list_rbind()
readr::write_csv(ame, paste0(out, "items_glm_ame.csv"))

# --- 4. unemployment on one item, with and without fixed effects ------------------
# The national unemployment rate varies only across country-years. Without
# fixed effects its coefficient compares countries with one another; with
# country and year fixed effects it compares each country with itself over
# time, net of what all countries share in a year. The interaction with
# education asks how the gap between the bands moves with the rate. Under
# fixed effects that gap is compared across all cells, between countries as
# well as within them, because the country effects are the same for every
# band, so the interaction is not confined to within-country movement.
interaction <- purrr::imap(items, \(lab, it) {
  f_pooled <- as.formula(paste(it, "~ unemp_c * edu3 + age_c + female + ses + urban"))
  f_fe     <- as.formula(paste(it, "~ unemp_c * edu3 + age_c + female + ses + urban | cntry + year"))
  fits <- list(
    "pooled, clustered"            = fixest::feglm(f_pooled, data = d, family = binomial("logit"), vcov = ~cy),
    "country and year fixed effects" = fixest::feglm(f_fe, data = d, family = binomial("logit"), vcov = ~cy)
  )
  purrr::imap(fits, \(m, nm) {
    ct <- fixest::coeftable(m)
    keep <- grepl("unemp_c|edu3", rownames(ct))
    tibble::tibble(item = it, model = nm, term = rownames(ct)[keep],
           estimate = ct[keep, 1], se = ct[keep, 2], z = ct[keep, 3], p = ct[keep, 4])
  }) |> purrr::list_rbind()
}) |> purrr::list_rbind()
readr::write_csv(interaction, paste0(out, "items_glm_interaction.csv"))

# --- 5. a random intercept for the cluster ------------------------------------------
# One item, one model: respondents inside country-years inside countries, the
# structure of the published analysis, fitted as a logit with glmmTMB, which
# is far faster than glmer on four hundred thousand rows. The variance
# components say how much of the item's variation sits at each level.
m_re <- glmmTMB::glmmTMB(it9 ~ unemp_c * edu3 + age_c + female + ses + urban + (1 | cntry) + (1 | cy),
                data = d, family = binomial("logit"))
fx <- summary(m_re)$coefficients$cond
random <- dplyr::bind_rows(
  tibble::tibble(item = "it9", part = "fixed", term = rownames(fx),
         estimate = fx[, "Estimate"], se = fx[, "Std. Error"], z = fx[, "z value"],
         p = fx[, "Pr(>|z|)"]),
  tibble::tibble(item = "it9", part = "random", term = c("sd(country)", "sd(country-year)"),
         estimate = c(attr(glmmTMB::VarCorr(m_re)$cond$cntry, "stddev"),
                      attr(glmmTMB::VarCorr(m_re)$cond$cy, "stddev")),
         se = NA_real_, z = NA_real_, p = NA_real_)
)

readr::write_csv(random, paste0(out, "items_glm_random.csv"))
