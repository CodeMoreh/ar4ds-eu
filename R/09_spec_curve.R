##############################################################################
# Title:        09_spec_curve.R
# Purpose:      The curve of the claim on the menu that the audit of October
#               2026 keeps: 864 specifications of the three positive scales
#               (cosmopolitan, utilitarian and their composite), and the same
#               864 for the three negative scales, the other half of the
#               hypothesis, on a curve of their own. Each curve crosses, for
#               every outcome, unemployment in four full-coverage series and
#               two forms, or GDP growth in the World Bank series, with or
#               without the other economic measure; two-way or country fixed
#               effects; the zero or the drop rule for respondents who mention
#               nothing; all 13 items or the set without "unemployment"; and
#               cells as interviewed or under the GESIS weight: 256
#               unemployment and 32 growth specifications per outcome. Sample,
#               regression weight, scoring rule and model family are fixed
#               (all 270 cells, unweighted, shares, linear).
#               The vertical axis is the descriptive partial correlation of
#               the conventional fit, r = t / sqrt(t^2 + df), which equals the
#               correlation of the residualised outcome and predictor; every
#               point is classed by the restricted wild cluster bootstrap by
#               country (Rademacher signs, 9,999 replicates, the null
#               imposed), with CR2 and Satterthwaite df beside it as the
#               closed-form check and CR1 on G - 1 df kept for teaching.
#               Claim alignment: r_ca = r on the positive curve and -r on the
#               negative one, each times -1 again for growth, so that r_ca < 0
#               is the direction of the claim on both curves.
#               The file is keyed by the thirteen axis codes of the exercise
#               helpers, fixed axes included, so that a class submission can
#               be classed by looking its specification up.
# Reads:        data/teney_panel_measurement.csv, data/teney_panel_weights.csv,
#               data/macro/predictor_measures.csv, data/EUframes_cy.csv,
#               _planning/audit-2026-10/out/menu_audited_maximal.csv (the
#               check of the partial correlations)
# Writes:       data/spec_curve.csv (864 rows),
#               data/spec_curve_negative.csv (864 rows)
# Author:       Chris Moreh
# Last updated: 2026-10-25
##############################################################################

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tidyr) })
source("R/_curve_tools.R")

inputs <- curve_inputs()
signs  <- curve_signs()

stat_cols <- c("b", "se", "t", "df", "r", "r_ca", "p_conv", "t_cr1", "p_cr1", "t_cr2", "df_cr2",
               "p_cr2", "p_wcr", "p_wcr_webb", "supports", "sig", "n_cells", "n_countries",
               "is_reference")

build_curve <- function(outcomes) {
  menu <- curve_menu(outcomes)
  map(seq_len(nrow(menu)), \(i) fit_linear(menu[i, ], inputs, signs)) |>
    list_rbind() |>
    bind_cols(menu) |>
    mutate(r_ca = r * align_factor(outcome, predictor),
           supports = r_ca < 0,
           sig = p_wcr < 0.05,
           is_reference = outcome == "mcosmo" & predictor == "unemp" & measure == "wb_nat" &
             pred_form == "raw" & copredictor == "none" & estimator == "fe_twoway" &
             noop == "zero" & item_set == "all" & cell_weight == "unweighted") |>
    select(all_of(axis_codes), all_of(stat_cols))
}

curve_pos <- build_curve(positive_scales)
curve_neg <- build_curve(negative_scales)

datawizard::data_write(curve_pos, out_path("spec_curve.csv"), verbose = FALSE)
datawizard::data_write(curve_neg, out_path("spec_curve_negative.csv"), verbose = FALSE)

## ---- checks -------------------------------------------------------------------

# the reference specification is the reproduction target of the course
ref <- curve_pos |> filter(is_reference)
stopifnot("the curves do not have 864 rows each" = nrow(curve_pos) == 864 && nrow(curve_neg) == 864,
          "there is not exactly one reference specification" = nrow(ref) == 1,
          "the reference is not reproduced" =
            abs(ref$t + 3.804) < 5e-4 && ref$df == 233 && abs(ref$r + 0.2418) < 1e-4,
          "a specification lost cells" = all(c(curve_pos$n_cells, curve_neg$n_cells) == 270))

# the partial correlations must match the refit of the audit
audited <- datawizard::data_read("_planning/audit-2026-10/out/menu_audited_maximal.csv",
                                 verbose = FALSE)
match_keys <- c("outcome", "predictor", "measure", "pred_form", "copredictor", "estimator",
                "noop", "item_set", "cell_weight")
chk <- curve_pos |> inner_join(audited, by = match_keys, suffix = c("", "_audit"))
tibble(matched = nrow(chk), max_abs_diff_r = max(abs(chk$r - chk$r_audit)),
       max_abs_diff_p_cr2 = max(abs(chk$p_cr2 - chk$p_cr2_audit)),
       class_differs_from_audit = sum((chk$p_wcr < 0.05) != (chk$p_wcr_audit < 0.05)),
       median_abs_diff_p_wcr = median(abs(chk$p_wcr - chk$p_wcr_audit))) |>
  print()
stopifnot("the partial correlations differ from the audit" =
            nrow(chk) == 864 && max(abs(chk$r - chk$r_audit)) < 1e-12)

# the bootstrap at the reference cell and its country fixed-effects twin,
# against fwildclusterboot with 99,999 replicates of its own
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  map(c("fe_twoway", "fe_country"), \(e) {
    row <- curve_menu("mcosmo") |>
      filter(predictor == "unemp", measure == "wb_nat", pred_form == "raw", copredictor == "none",
             noop == "zero", item_set == "all", cell_weight == "unweighted", estimator == e)
    d <- spec_data(row, inputs)
    m <- lm(as.formula(paste("y ~ x + factor(cntry)", if (e == "fe_twoway") "+ factor(year)")),
            data = d)
    # the package draws from the dqrng stream, which is seeded here
    bt <- map(c("rademacher", "webb"), \(ty) {
      dqrng::dqset.seed(20261025)
      fwildclusterboot::boottest(m, param = "x", clustid = "cntry", B = 99999, type = ty,
                                 conf_int = FALSE)
    })
    ours <- curve_pos |> semi_join(row, by = axis_codes)
    tibble(estimator = e, p_wcr = ours$p_wcr, p_package = fwildclusterboot::pval(bt[[1]]),
           p_wcr_webb = ours$p_wcr_webb, p_package_webb = fwildclusterboot::pval(bt[[2]]))
  }) |>
    list_rbind() |>
    print()
}

# the shares that the pages quote, on both curves
bind_rows(curve_pos |> mutate(curve = "positive"), curve_neg |> mutate(curve = "negative")) |>
  summarise(specs = n(), below = 100 * mean(supports),
            conv = 100 * mean(supports & p_conv < 0.05), cr1 = 100 * mean(supports & p_cr1 < 0.05),
            cr2 = 100 * mean(supports & p_cr2 < 0.05), wcr = 100 * mean(supports & sig),
            webb = 100 * mean(supports & p_wcr_webb < 0.05), median_r_ca = median(r_ca),
            .by = curve) |>
  print()
