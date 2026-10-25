# The grid pipeline

The scripts in this folder build the specification grids, the curve of the claim and the tables around it that the course pages, the browser lab and the class chart read from `data/`. They are numbered in the order in which they run, and each one reads only the course panel, the inputs listed below that no script builds, and the outputs of the scripts before it. Three files are sourced helpers, not pipeline stages: `_paths.R` names the four locations used throughout, `_wild_bootstrap.R` holds the significance rule of the curve, and `_curve_tools.R` holds the one definition of a specification and of its fit that stages 09 to 13 share.

## Order and dependencies

| Script | Reads | Writes |
|:--|:--|:--|
| `01_person_file.R` | the sixteen GESIS Eurobarometer files in `_planning_data/raw/`, `data/EUframes_cy.csv` | `_planning_data/person_level.rds` (respondent level, never committed) |
| `02_macro_series.R` | `data-raw/eurostat/*.tsv.gz`, `data/EUframes_cy.csv` | `data/macro/analyst_unemployment.csv`, `data/macro/predictor_measures.csv` |
| `07_measurement_panels.R` | `_planning_data/person_level.rds`, `data/macro/population.csv`, `data/EUframes_cy.csv` | `data/teney_panel_weights.csv`, `data/teney_panel_measurement.csv` |
| `08_mi_tier.R` | `_planning_data/person_level.rds`, `data/EUframes_cy.csv`, `data/teney_panel_weights.csv` | `data/spec_grid_mi.csv`, `data/mi_diagnostics.csv` |
| `09_spec_curve.R` | `data/teney_panel_measurement.csv`, `data/teney_panel_weights.csv`, `data/macro/predictor_measures.csv`, `data/EUframes_cy.csv` | `data/spec_curve.csv`, `data/spec_curve_negative.csv` (864 rows each) |
| `10_curve_forks.R` | the two curves | `data/spec_curve_forks.csv` |
| `11_menu_deviations.R` | the two curves, the inputs of `09`, `data/spec_grid_mi.csv` | `data/menu_deviations_rows.csv.gz`, `data/menu_deviations.csv` |
| `12_displays.R` | the inputs of `09`, `data/spec_curve.csv` | `data/display_between_within.csv`, `data/display_country_means.csv`, `data/display_periods.csv`, `data/display_loo.csv`, `data/display_bailout.csv`, `data/display_inference.csv` |
| `13_reference_marks.R` | `_template/data_private/eb_respondents.rds` through `companion/_model-outputs/_load_respondents.R`, `data/analysts5.csv`, `data/spec_curve.csv`, the inputs of `09` | `companion/_model-outputs/reference_refits.csv`, `data/reference_marks.csv` |
| `check_parity.R` | the committed files and a rebuild of them in another folder | nothing; prints the comparison |

The stages are numbered from 07 after the macro series because the numbers 03 to 06 belonged to the stages that built the grids of the menu before the audit of October 2026, which the curve of `09` and the comparisons of `11` supersede; they remain in the history of the repository. The curve of `09` is the menu that the audit keeps, and `10` to `13` are built on it. Three stages read licensed microdata: `01` builds the respondent file, `07` and `08` aggregate it, and `13` refits two respondent-level designs on the respondent file that the workspace builds. Everything else runs from committed aggregates.

The significance rule of the curve is the restricted wild cluster bootstrap by country in `_wild_bootstrap.R`. Its sign matrix is drawn once from a fixed seed and keyed by country code, so that a copy of the file reproduces any stored p-value exactly from the same cells; `09` checks it against fwildclusterboot at the reference cell.

## Locations

`_paths.R` sets four locations, each of which an environment variable can redirect: the destination of every file that the site reads (`COURSE_OUT_DIR`, by default `data`), a work folder for intermediate tiers that only later stages read (`COURSE_WORK_DIR`, by default `_grid_work`), the private folder for the licensed files (`COURSE_PRIVATE_DIR`, by default `_planning_data`), and the folder of the raw GESIS waves (`EB_RAW_DIR`, by default `_planning_data/raw`). A rebuild is first written to a folder outside `data/` and compared with the committed files by `check_parity.R`, which compares every file that both builds hold; the committed files are replaced only once the comparison has been read. Stages 09 to 13 read the outputs of `02` and `07` through the same setting, so a rebuild folder for them needs those files as well.

## Inputs that no script builds

- `data/EUframes_cy.csv` – the course panel, rebuilt from the raw GESIS files; its codebook is beside it.
- `data/analysts5.csv` – the Task 2 results of the five Multi100 analysts, from the public Multi100 data. The effect size of 018OL carries the sign of the Multi100 record (d = −0.121).
- `data/macro/population.csv` – total and adult population by country-year, World Bank `SP.POP.TOTL` less the share aged 0–14 (`SP.POP.0014.TO.ZS`), as retrieved in September 2026. It is an input of `07`.
- `data-raw/eurostat/` – the five Eurostat tables from which the analysts' unemployment series and the Eurostat growth series are cut, stored as retrieved; their provenance is in `data-raw/eurostat/README.md`.
- `_template/data_private/eb_respondents.rds` – the respondent file that the workspace builds with `prep/01_build_respondents.R` from the participant's own GESIS downloads. It carries the occupation, community type and education bands that the designs of `13` need and `person_level.rds` does not. It is licensed microdata and never committed.
- `_planning/audit-2026-10/out/menu_audited_maximal.csv` – the refit of the audit, against which `09` checks the partial correlations of the curve. It is a check, not an input of any output.

## Packages

dplyr, tidyr, purrr, readr, datawizard, haven, fixest, plm, betareg, glmmTMB, sandwich, lmtest, clubSandwich, lme4, lmerTest, mice, Amelia and effectsize; fwildclusterboot and dqrng for the check of the bootstrap in `09`.
