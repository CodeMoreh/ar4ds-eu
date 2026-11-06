################################################################################
# Title:        Bayesian models of the EU-frames panel
# Purpose:      The fits behind the Bayesian estimation module. The slope of
#               the cosmopolitan share on unemployment in the 270
#               country-years, fitted with brms: crossed random intercepts
#               for countries and years under the default priors of brms;
#               then the two-way design under weakly informative priors,
#               first from its priors alone (prior predictive check), then
#               with one slope and independent errors, with errors that
#               persist from year to year within a country (AR(1)), with a
#               slope for each country (partial pooling), and with both.
#               Posterior predictive checks of the persistence of the
#               residuals and of the bounds of the share, prior sensitivity
#               by power-scaling (priorsense) and the computational checks;
#               only summaries and thinned draws leave the script.
# Reads:        cases/eu-frames/data/EUframes_cy.csv
# Writes:       companion/_model-outputs/bayes_panel_summary.csv
#               companion/_model-outputs/bayes_panel_prior_draws.csv
#               companion/_model-outputs/bayes_panel_ppc.csv
#               companion/_model-outputs/bayes_panel_ppc_draws.csv
#               companion/_model-outputs/bayes_panel_countries.csv
#               companion/_model-outputs/bayes_panel_diagnostics.csv
#               companion/_model-outputs/bayes_panel_sensitivity.csv
#               companion/_model-outputs/bayes_panel_versions.txt
#               (and the fitted models as fit_panel_*.rds, which git ignores)
# Author:       Claude for CM
# Last updated: 2026-11-06
################################################################################

set.seed(20261106)
out <- "companion/_model-outputs/"

# Unemployment as the departure of each year from the decade mean of its
# country (unemp_within), which leaves the slope of a model with country
# intercepts unchanged and keeps a country slope from standing in for a
# difference of level between countries
euframes <- readr::read_csv("cases/eu-frames/data/EUframes_cy.csv", show_col_types = FALSE) |>
  datawizard::demean(select = "unemp", by = "cntry") |>
  dplyr::mutate(year_f = factor(year))

# --- Crossed random intercepts under the default priors ---------------------- #
# The call and the seed of model 8 in fit_models.R, whose printed summary and
# priors the module displays from brms_panel.txt and brms_priors.txt; the
# same call returns the same draws, and this fit supplies the numbers that the
# page quotes from them
fit_ri <- brms::brm(mcosmo ~ unemp + (1 | cntry) + (1 | year), data = euframes,
                    chains = 4, cores = 4, iter = 4000, seed = 2026, backend = "cmdstanr",
                    refresh = 0, silent = 2, file = paste0(out, "fit_panel_ri"), file_refit = "on_change")

# --- The two-way design under weakly informative priors ---------------------- #
# Priors on the scales of a share and of a rate in points, set before the
# data. Each country has a level of its own (0 + cntry) under one common prior,
# its share in 2004, which a prior on differences from one reference country
# would not give; years depart from 2004 by up to about 0.2; the slope is up to
# about 0.02 per point; the residual sd is a few points of share; country
# slopes depart from the average by up to about 0.02; the persistence is
# centred on none. brms bounds the AR coefficient to (-1, 1) when cov = TRUE.
prior_twoway <- c(brms::prior(normal(0, 0.1), class = "b"),
                  brms::set_prior("normal(0.5, 0.15)", class = "b",
                                  coef = paste0("cntry", sort(unique(euframes$cntry)))),
                  brms::prior(normal(0, 0.01), class = "b", coef = "unemp_within"),
                  brms::prior(exponential(20), class = "sigma"))
prior_sd <- brms::prior(normal(0, 0.01), class = "sd")
prior_ar <- brms::prior(normal(0, 0.5), class = "ar")

# cov = TRUE models the AR(1) residuals through their covariance matrix within
# each country: the process is stationary, and posterior_predict() draws whole
# replicated series, which the check of persistence needs. The regression form
# (cov = FALSE) predicts each year from the observed year before.
formulas <- list(
  common = mcosmo ~ 0 + cntry + year_f + unemp_within,
  ar1 = mcosmo ~ 0 + cntry + year_f + unemp_within + ar(time = year, gr = cntry, p = 1, cov = TRUE),
  slopes = mcosmo ~ 0 + cntry + year_f + unemp_within + (0 + unemp_within | cntry),
  both = mcosmo ~ 0 + cntry + year_f + unemp_within + (0 + unemp_within | cntry) +
    ar(time = year, gr = cntry, p = 1, cov = TRUE)
)
priors <- list(common = prior_twoway, ar1 = c(prior_twoway, prior_ar),
               slopes = c(prior_twoway, prior_sd), both = c(prior_twoway, prior_sd, prior_ar))

fit_prior <- brms::brm(formulas$common, data = euframes, prior = prior_twoway, sample_prior = "only",
                       chains = 4, cores = 4, iter = 2000, seed = 20261106, backend = "cmdstanr",
                       refresh = 0, silent = 2, file = paste0(out, "fit_panel_prior"), file_refit = "on_change")

# adapt_delta = 0.95 for every two-way fit: the varying slopes put a
# hierarchical sd near zero, where the sampler needs smaller steps
twoway <- purrr::imap(formulas, \(f, name) {
  brms::brm(f, data = euframes, prior = priors[[name]],
            chains = 4, cores = 4, iter = 2000, seed = 20261106, backend = "cmdstanr",
            control = list(adapt_delta = 0.95), refresh = 0, silent = 2,
            file = paste0(out, "fit_panel_", name), file_refit = "on_change")
})
fits <- c(list(ri = fit_ri, prior = fit_prior), twoway)

# --- Posterior summaries ----------------------------------------------------- #
# Median, mean, sd and quantiles of each parameter; the probability of
# direction, and the probability of a value beyond 0.001 in the direction of
# the median, which for the slope is a fall of at least one point of share
# for a rise of ten points of unemployment (bayestestR)
params <- list(ri = c("b_unemp", "b_Intercept", "sd_cntry__Intercept", "sd_year__Intercept", "sigma"),
               prior = c("b_unemp_within", "sigma"),
               common = c("b_unemp_within", "sigma"),
               ar1 = c("b_unemp_within", "ar[1]", "sigma"),
               slopes = c("b_unemp_within", "sd_cntry__unemp_within", "sigma"),
               both = c("b_unemp_within", "sd_cntry__unemp_within", "ar[1]", "sigma"))
summary_tab <- purrr::imap(params, \(vars, name) {
  posterior::summarise_draws(
    brms::as_draws_df(fits[[name]], variable = vars),
    "median", "mean", "sd",
    \(x) posterior::quantile2(x, probs = c(0.025, 0.05, 0.95, 0.975)),
    # summarise_draws() passes each variable as an iterations-by-chains matrix,
    # which bayestestR would read as one parameter per chain
    pd = \(x) as.numeric(bayestestR::p_direction(as.vector(x))),
    p_beyond = \(x) as.numeric(bayestestR::p_significance(as.vector(x), threshold = 0.001))
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(model = name, .before = 1)
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signif(x, 6)))
readr::write_csv(summary_tab, paste0(out, "bayes_panel_summary.csv"))

# --- Prior predictive check -------------------------------------------------- #
# Cell means simulated from the priors alone (50 of the 4,000 draws, every
# cell), and the change in the share for a rise of ten points of unemployment,
# in points of share (every draw)
# Seeded here as well as at the top, because a fit loaded from its file draws
# no random numbers and would otherwise leave the stream elsewhere
set.seed(20261106)
yrep_prior <- brms::posterior_predict(fit_prior)
slope_prior <- brms::as_draws_df(fit_prior, variable = "b_unemp_within")$b_unemp_within
prior_draws <- dplyr::bind_rows(
  tibble::tibble(quantity = "cell_mean", draw = rep(seq(80, 4000, by = 80), each = ncol(yrep_prior)),
                 value = as.vector(t(yrep_prior[seq(80, 4000, by = 80), ]))),  # r-style: keep – a draws-by-cells matrix flattened row by row
  tibble::tibble(quantity = "rise_10", draw = seq_along(slope_prior), value = 10 * 100 * slope_prior)
) |>
  dplyr::mutate(value = round(value, 4))
readr::write_csv(prior_draws, paste0(out, "bayes_panel_prior_draws.csv"))

# --- Posterior predictive checks --------------------------------------------- #
# Persistence: the correlation within countries between the residual of a
# country-year and that of the year before, the residuals coming from the
# two-way least-squares fit, computed alike on the data and on every
# replicated dataset. Bounds: the smallest and the largest cell mean of each
# replicated dataset, and the share of replicated cell means outside 0 to 1.
pairs <- euframes |>
  dplyr::mutate(row = dplyr::row_number()) |>
  datawizard::data_arrange(c("cntry", "year")) |>
  dplyr::mutate(previous = dplyr::lag(row), .by = cntry) |>
  datawizard::data_filter(!is.na(previous))
resid_obs <- resid(lm(mcosmo ~ unemp + cntry + year_f, data = euframes))
observed <- c(persistence = cor(resid_obs[pairs$row], resid_obs[pairs$previous]),
              min = min(euframes$mcosmo), max = max(euframes$mcosmo), outside = 0)

# One lm() with the replicated datasets as the columns of its outcome fits the
# same two-way design to all of them at once
set.seed(20261107)
yrep <- purrr::map(twoway, brms::posterior_predict)
ppc_draws <- purrr::imap(yrep, \(y, name) {
  resid_rep <- resid(lm(t(y) ~ unemp + cntry + year_f, data = euframes))
  tibble::tibble(
    model = name, draw = seq_len(nrow(y)),
    persistence = purrr::map_dbl(seq_len(nrow(y)), \(s) cor(resid_rep[pairs$row, s], resid_rep[pairs$previous, s])),
    min = purrr::map_dbl(seq_len(nrow(y)), \(s) min(y[s, ])),
    max = purrr::map_dbl(seq_len(nrow(y)), \(s) max(y[s, ])),
    outside = purrr::map_dbl(seq_len(nrow(y)), \(s) mean(y[s, ] < 0 | y[s, ] > 1))
  )
}) |>
  purrr::list_rbind()

# The tail probability compares each replicated statistic with the observed
# one; for the share outside 0 to 1, whose observed value is 0, only the
# replicated values carry information
ppc <- ppc_draws |>
  datawizard::data_to_long(select = c("persistence", "min", "max", "outside"),
                           names_to = "statistic", values_to = "value") |>
  dplyr::mutate(observed = observed[statistic]) |>
  dplyr::summarise(observed = observed[1], rep_median = median(value), rep_q05 = quantile(value, 0.05),
                   rep_q95 = quantile(value, 0.95), rep_mean = mean(value), p_ppc = mean(value >= observed),
                   .by = c("model", "statistic")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signif(x, 6)))
readr::write_csv(ppc, paste0(out, "bayes_panel_ppc.csv"))

# One draw in four of the persistence statistic, for the figure of the check
ppc_draws |>
  datawizard::data_filter(draw %% 4 == 0) |>
  datawizard::data_select(c("model", "draw", "persistence")) |>
  dplyr::mutate(persistence = round(persistence, 4)) |>
  readr::write_csv(paste0(out, "bayes_panel_ppc_draws.csv"))

# --- The slopes of the countries --------------------------------------------- #
# No pooling: a slope for each country from its own ten years, year effects
# shared, by least squares with a 90% confidence interval. Partial pooling:
# the posterior median and 90% interval of each country slope (the average
# slope plus the departure of the country) in the two varying-slope models.
no_pooling <- lm(mcosmo ~ 0 + cntry + year_f + cntry:unemp_within, data = euframes) |>
  parameters::model_parameters(ci = 0.9) |>
  tibble::as_tibble() |>
  datawizard::data_filter(grepl(":unemp_within$", Parameter)) |>
  dplyr::transmute(cntry = sub("^cntry(.*):unemp_within$", "\\1", Parameter), model = "no_pooling",
                   estimate = Coefficient, low = CI_low, high = CI_high)
partial <- purrr::map(c("slopes", "both"), \(name) {
  coef(twoway[[name]], robust = TRUE, probs = c(0.05, 0.95))$cntry[, , "unemp_within"] |>
    tibble::as_tibble(rownames = "cntry") |>
    dplyr::transmute(cntry, model = name, estimate = Estimate, low = Q5, high = Q95)
}) |>
  purrr::list_rbind()
dplyr::bind_rows(no_pooling, partial) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signif(x, 6))) |>
  readr::write_csv(paste0(out, "bayes_panel_countries.csv"))

# --- Computation ------------------------------------------------------------- #
# The largest R-hat and the smallest bulk and tail effective sample sizes over
# every parameter, divergent transitions and the deepest tree of the sampler
diagnostics <- purrr::imap(fits, \(fit, name) {
  conv <- posterior::summarise_draws(
    posterior::subset_draws(brms::as_draws_df(fit), variable = c("lp__", "lprior"), exclude = TRUE),
    "rhat", "ess_bulk", "ess_tail")
  nuts <- brms::nuts_params(fit)
  tibble::tibble(model = name, draws = brms::ndraws(fit), parameters = nrow(conv),
                 max_rhat = max(conv$rhat, na.rm = TRUE),
                 min_ess_bulk = min(conv$ess_bulk, na.rm = TRUE), min_ess_tail = min(conv$ess_tail, na.rm = TRUE),
                 divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
                 max_treedepth = max(nuts$Value[nuts$Parameter == "treedepth__"]))
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signif(x, 6)))
readr::write_csv(diagnostics, paste0(out, "bayes_panel_diagnostics.csv"))

# --- Prior sensitivity by power-scaling (Kallioinen et al.) ------------------ #
# How far the posterior of each parameter moves when the prior or the
# likelihood is raised to a power near 1, without refitting. For the models
# with AR(1) residuals brms returns the log-likelihood of each country-year
# given the other years of its country, so the likelihood column of those
# models scales that conditional form; the prior column is exact for all.
sensitivity <- purrr::imap(params[c("common", "ar1", "slopes", "both")], \(vars, name) {
  priorsense::powerscale_sensitivity(twoway[[name]], variable = vars) |>
    tibble::as_tibble() |>
    dplyr::transmute(model = name, variable, prior = signif(prior, 4), likelihood = signif(likelihood, 4),
                     diagnosis)
}) |>
  purrr::list_rbind()
readr::write_csv(sensitivity, paste0(out, "bayes_panel_sensitivity.csv"))

writeLines(c(
  paste("Fitted:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste("R", getRversion(), "| CmdStan", cmdstanr::cmdstan_version()),
  paste("brms", packageVersion("brms"), "| cmdstanr", packageVersion("cmdstanr"),
        "| posterior", packageVersion("posterior"), "| priorsense", packageVersion("priorsense"),
        "| bayestestR", packageVersion("bayestestR"))
), paste0(out, "bayes_panel_versions.txt"))
