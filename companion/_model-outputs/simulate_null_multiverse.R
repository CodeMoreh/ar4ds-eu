################################################################################
# Title:        A null multiverse on the structure of the EU-frames twin
# Purpose:      The null world of the simulation module, declared and diagnosed
#               with DeclareDesign (Blair, Cooper, Coppock and Humphreys 2019,
#               American Political Science Review): respondents in the 270
#               country-years of the course panel, at a tenth of the respondents
#               of each cell as in the simulated twin, with a country effect, a
#               country-year effect and person-level scatter whose sizes come
#               from the panel and the twin, and no effect of unemployment at
#               all. Four estimators that the course discusses are fitted to
#               every draw, and the share of draws in which each rejects the true
#               value of zero at the 5% level is its rate of false positives.
# Reads:        cases/eu-frames/data/EUframes_cy.csv (the cells, their
#               unemployment and sizes, and the variances between countries and
#               between country-years), cases/eu-frames/data/EUframes_person_full.csv
#               (the mean and the scatter of the person-level score)
# Writes:       companion/_model-outputs/sim_null_multiverse.csv
# Author:       Claude for CM
# Last updated: 2026-10-27
################################################################################

set.seed(20261027)

panel <- readr::read_csv("cases/eu-frames/data/EUframes_cy.csv", show_col_types = FALSE) |>
  dplyr::arrange(cntry, year) |>
  dplyr::mutate(n_sim = pmax(1L, as.integer(round(n_cy * 0.10))))
twin <- readr::read_csv("cases/eu-frames/data/EUframes_person_full.csv", show_col_types = FALSE)

# The sizes of the three kinds of scatter: between countries and between the
# country-years of a country, from a random-intercept model of the 270 cell
# means; between the respondents of a cell, from the twin
cells_fit <- lmerTest::lmer(mcosmo ~ unemp + (1 | cntry), data = panel)
cells_sd <- parameters::model_parameters(cells_fit, effects = "random")
sd_country <- cells_sd$Coefficient[cells_sd$Group == "cntry"]
sd_cell <- cells_sd$Coefficient[cells_sd$Group == "Residual"]
sd_person <- twin |>
  dplyr::summarise(v = stats::var(cosmo), .by = c(cntry, year)) |>
  dplyr::summarise(sd = sqrt(mean(v))) |>
  dplyr::pull(sd)
mean_cosmo <- mean(twin$cosmo)

null_world <-
  DeclareDesign::declare_model(
    countries = fabricatr::add_level(N = dplyr::n_distinct(panel$cntry), cntry = unique(panel$cntry),
                                     u_c = stats::rnorm(N, 0, sd_country)),
    cells = fabricatr::add_level(N = 10, year = panel$year, unemp = panel$unemp, n_sim = panel$n_sim,
                                 u_cy = stats::rnorm(N, 0, sd_cell)),
    persons = fabricatr::add_level(N = n_sim,
                                   cosmo = pmin(pmax(mean_cosmo + 0 * unemp + u_c + u_cy +
                                                       stats::rnorm(N, 0, sd_person), 0), 1))
  ) +
  DeclareDesign::declare_inquiry(slope = 0) +
  DeclareDesign::declare_estimator(cosmo ~ unemp, .method = estimatr::lm_robust, se_type = "classical",
                                   inquiry = "slope", term = "unemp", label = "pooled") +
  # The two multilevel estimators report the slope of unemployment with its t on
  # Satterthwaite df (lmerTest, read with parameters); purrr::exec() writes the
  # data into the stored call, which lmerTest evaluates again for the df
  DeclareDesign::declare_estimator(
    handler = DeclareDesign::label_estimator(\(data) data |>
      purrr::exec(.fn = lmerTest::lmer, formula = cosmo ~ unemp + (1 | cntry)) |>
      parameters::model_parameters(effects = "fixed", ci_method = "satterthwaite", keep = "^unemp$") |>
      as.data.frame() |>  # r-style: keep – drops the print method of parameters before the columns are chosen
      dplyr::transmute(term = Parameter, estimate = Coefficient, std.error = SE, df = df_error,
                       p.value = p, conf.low = CI_low, conf.high = CI_high)),
    inquiry = "slope", label = "country_re") +
  DeclareDesign::declare_estimator(
    handler = DeclareDesign::label_estimator(\(data) data |>
      purrr::exec(.fn = lmerTest::lmer, formula = cosmo ~ unemp + (1 | cntry) + (1 | cntry:year)) |>
      parameters::model_parameters(effects = "fixed", ci_method = "satterthwaite", keep = "^unemp$") |>
      as.data.frame() |>  # r-style: keep – drops the print method of parameters before the columns are chosen
      dplyr::transmute(term = Parameter, estimate = Coefficient, std.error = SE, df = df_error,
                       p.value = p, conf.low = CI_low, conf.high = CI_high)),
    inquiry = "slope", label = "three_level") +
  DeclareDesign::declare_estimator(
    handler = DeclareDesign::label_estimator(\(data) data |>
      dplyr::summarise(mcosmo = mean(cosmo), unemp = dplyr::first(unemp), .by = c(cntry, year)) |>
      estimatr::lm_robust(formula = mcosmo ~ unemp, se_type = "classical") |>
      estimatr::tidy() |>
      dplyr::filter(term == "unemp")),
    inquiry = "slope", label = "cell_means")

# 500 draws; the bootstrap of DeclareDesign gives the simulation error of each
# rate (its "se(power)")
null_dx <- DeclareDesign::diagnose_design(null_world, sims = 500, bootstrap_sims = 200)

null_multiverse <- null_dx$diagnosands_df |>
  dplyr::select(estimator, n_sims, mean_estimate, sd_estimate, rejection_rate = power,
                rejection_rate_se = dplyr::all_of("se(power)"), coverage) |>  # r-style: keep – a column of DeclareDesign named se(power), not a call
  dplyr::mutate(sd_country = sd_country, sd_cell = sd_cell, sd_person = sd_person,
                mean_cosmo = mean_cosmo, persons = sum(panel$n_sim),
                dplyr::across(dplyr::where(is.double), \(x) signif(x, 6)))

# readr writes the bytes of the committed files; datawizard::data_write() does not
readr::write_csv(null_multiverse, "companion/_model-outputs/sim_null_multiverse.csv", na = "")
