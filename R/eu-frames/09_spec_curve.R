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
#               point is classed by CR2 errors clustered by country with
#               Satterthwaite df (clubSandwich), with conventional errors and
#               CR1 on G - 1 df kept for teaching.
#               Claim alignment: r_ca = r on the positive curve and -r on the
#               negative one, each times -1 again for growth, so that r_ca < 0
#               is the direction of the claim on both curves.
#               The file is keyed by the thirteen axis codes of the exercise
#               helpers, fixed axes included, so that a class submission can
#               be classed by looking its specification up.
# Reads:        cases/eu-frames/data/teney_panel_measurement.csv,
#               cases/eu-frames/data/teney_panel_weights.csv,
#               cases/eu-frames/data/macro/predictor_measures.csv,
#               cases/eu-frames/data/EUframes_cy.csv,
#               _planning/audit-2026-10/out/menu_audited_maximal.csv or the
#               file named by COURSE_AUDIT_FILE (the check of the partial
#               correlations and of the CR2 p-values)
# Writes:       cases/eu-frames/data/spec_curve.csv (864 rows),
#               cases/eu-frames/data/spec_curve_negative.csv (864 rows)
# Author:       Chris Moreh
# Last updated: 2026-10-26
##############################################################################

source("R/eu-frames/_curve_tools.R")

inputs <- curve_inputs()

stat_cols <- c("b", "se", "t", "df", "r", "r_ca", "p_conv", "t_cr1", "p_cr1", "t_cr2", "df_cr2",
               "p_cr2", "supports", "sig", "n_cells", "n_countries", "is_reference")

build_curve <- function(outcomes) {
  menu <- curve_menu(outcomes)
  purrr::map(seq_len(nrow(menu)), \(i) fit_linear(menu[i, ], inputs)) |>
    purrr::list_rbind() |>
    dplyr::bind_cols(menu) |>
    dplyr::mutate(r_ca = r * align_factor(outcome, predictor),
                  supports = r_ca < 0,
                  sig = p_cr2 < 0.05,
                  is_reference = outcome == "mcosmo" & predictor == "unemp" & measure == "wb_nat" &
                    pred_form == "raw" & copredictor == "none" & estimator == "fe_twoway" &
                    noop == "zero" & item_set == "all" & cell_weight == "unweighted") |>
    datawizard::data_select(select = c(axis_codes, stat_cols))
}

curve_pos <- build_curve(positive_scales)
curve_neg <- build_curve(negative_scales)

datawizard::data_write(curve_pos, out_path("spec_curve.csv"), verbose = FALSE)
datawizard::data_write(curve_neg, out_path("spec_curve_negative.csv"), verbose = FALSE)

## ---- checks -------------------------------------------------------------------

# the reference specification is the reproduction target of the course
ref <- datawizard::data_filter(curve_pos, is_reference)
stopifnot("the curves do not have 864 rows each" = nrow(curve_pos) == 864 && nrow(curve_neg) == 864,
          "there is not exactly one reference specification" = nrow(ref) == 1,
          "the reference is not reproduced" =
            abs(ref$t + 3.804) < 5e-4 && ref$df == 233 && abs(ref$r + 0.2418) < 1e-4,
          "a specification lost cells" = all(c(curve_pos$n_cells, curve_neg$n_cells) == 270))

# the partial correlations and the CR2 p-values must match the refit of the
# audit, a local planning file that COURSE_AUDIT_FILE can point to; the check
# is skipped, with a message, where the file is absent
audit_file <- Sys.getenv("COURSE_AUDIT_FILE",
                         unset = "_planning/audit-2026-10/out/menu_audited_maximal.csv")
if (file.exists(audit_file)) {
  audited <- datawizard::data_read(audit_file, verbose = FALSE)
  match_keys <- c("outcome", "predictor", "measure", "pred_form", "copredictor", "estimator",
                  "noop", "item_set", "cell_weight")
  chk <- dplyr::inner_join(curve_pos, audited, by = match_keys, suffix = c("", "_audit"))
  tibble::tibble(matched = nrow(chk), max_abs_diff_r = max(abs(chk$r - chk$r_audit)),
                 max_abs_diff_p_cr2 = max(abs(chk$p_cr2 - chk$p_cr2_audit)),
                 class_differs_from_audit = sum((chk$p_cr2 < 0.05) != (chk$p_cr2_audit < 0.05))) |>
    print()
  stopifnot("the partial correlations differ from the audit" =
              nrow(chk) == 864 && max(abs(chk$r - chk$r_audit)) < 1e-12,
            "the CR2 p-values differ from the audit" = max(abs(chk$p_cr2 - chk$p_cr2_audit)) < 1e-12)
} else {
  message("The audit file ", audit_file, " is absent, so the curve is not checked against it.")
}

# the shares that the pages quote, on both curves
dplyr::bind_rows(positive = curve_pos, negative = curve_neg, .id = "curve") |>
  dplyr::summarise(specs = dplyr::n(), below = 100 * mean(supports),
                   conv = 100 * mean(supports & p_conv < 0.05),
                   cr1 = 100 * mean(supports & p_cr1 < 0.05),
                   cr2 = 100 * mean(supports & sig), median_r_ca = stats::median(r_ca),
                   median_df_cr2 = stats::median(df_cr2), .by = curve) |>
  print()
