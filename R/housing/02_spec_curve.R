################################################################################
# Title:        02_spec_curve.R
# Purpose:      The curve of the claim: every specification of the menu
#               fitted once, with a randomisation test of each as the check.
# Reads:        cases/housing/data/housing_audit.csv
# Writes:       cases/housing/data/spec_curve.csv
# Author:       Claude for CM
# Last updated: 2026-10-26
################################################################################

source("R/housing/_tools.R")

audit <- readr::read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE)

# The randomisation test reshuffles the names among the analysed emails and
# refits, so it asks how often an arbitrary relabelling gives a gap as large as
# the one observed. It is stored as a check beside the test of each model.
# ri2::conduct_ri() draws the reshuffles from the design of the experiment, a
# complete random assignment of the Hispanic names to as many emails as
# received them (Gerber and Green 2012, chapter 3; Coppock, ri2). The linear
# model is refitted by ri2 itself, without inverse probability weights, which
# would change the covariate-adjusted coefficient; the logit is refitted
# through a test function. statmod::permp() turns the count of reshuffles at
# least as extreme into a p-value that is never zero (Phipson and Smyth 2010,
# Statistical Applications in Genetics and Molecular Biology).
N_DRAWS <- 1999

curve <- tidyr::expand_grid(!!!SPEC_AXES) |>
  purrr::pmap(\(comparison, population, uncoded, model, adjust)
              fit_spec(audit, comparison, population, uncoded, model, adjust)) |>
  dplyr::bind_rows() |>
  dplyr::mutate(spec_id = dplyr::row_number(), .before = 1)

p_ri <- curve |>
  purrr::pmap_dbl(\(spec_id, comparison, population, uncoded, model, adjust, coef, ...) {
    set.seed(20261025 + spec_id)
    terms <- ADJUST_TERMS[[adjust]]
    frame <- spec_frame(audit, comparison, population, uncoded) |>
      dplyr::select("y", "hisp", dplyr::all_of(terms)) |>
      stats::na.omit()
    form <- stats::reformulate(c("hisp", terms), response = "y")
    design <- randomizr::declare_ra(N = nrow(frame), m = sum(frame$hisp))
    ri <- if (model == "lpm") {
      ri2::conduct_ri(form, assignment = "hisp", declaration = design, sharp_hypothesis = 0,
                      IPW = FALSE, data = frame, sims = N_DRAWS)
    } else {
      ri2::conduct_ri(test_function = \(d) stats::coef(stats::glm(form, family = stats::binomial(), data = d))[["hisp"]],
                      assignment = "hisp", declaration = design, sharp_hypothesis = 0,
                      data = frame, sims = N_DRAWS)
    }
    stopifnot(abs(ri$sims_df$est_obs[1] - coef) < 1e-9)
    extreme <- summary(ri)$two_tailed_p_value * N_DRAWS
    statmod::permp(round(extreme), nperm = N_DRAWS, n1 = sum(frame$hisp), n2 = sum(frame$hisp == 0))
  })

curve <- curve |>
  dplyr::mutate(
    p_ri = p_ri,
    supports = b_pp < 0,
    sig = p < 0.05,
    is_reference = comparison == "white" & population == "replies" & uncoded == "drop" &
      model == "lpm" & adjust == "none"
  ) |>
  # the 95% interval is printed by the helpers from a live fit, not stored
  dplyr::select(-"ci_low", -"ci_high")

stopifnot(nrow(curve) == 64, sum(curve$is_reference) == 1)

readr::write_csv(curve, "cases/housing/data/spec_curve.csv")  # r-style: keep – datawizard::data_write() writes other bytes
