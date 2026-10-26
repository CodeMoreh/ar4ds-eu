# The grid pipeline

The scripts in this folder build the specification grids, the curve of the claim and the tables around it that the pages of the case, the browser lab and the class chart read from `cases/eu-frames/data/`. File names without a folder in the table and the lists below are in that folder; every other path is relative to the project root, from which the scripts run. They are numbered in the order in which they run, and each one reads only the course panel, the inputs listed below that no script builds, and the outputs of the scripts before it. Two files are sourced helpers, not pipeline stages: `_paths.R` names the four locations used throughout, and `_curve_tools.R` holds the one definition of a specification and of its fit that stages 09 to 13 share, the significance rule of the curve included. A third, `_chart_multiverse.R`, builds the interactive charts of the multiverse page and is sourced by that page alone.

## Order and dependencies

| Script | Reads | Writes |
|:--|:--|:--|
| `01_person_file.R` | the sixteen GESIS Eurobarometer files in `_planning_data/raw/`, `EUframes_cy.csv` | `_planning_data/person_level.rds` (respondent level, never committed) |
| `02_macro_series.R` | `data-raw/eu-frames/eurostat/*.tsv.gz`, `EUframes_cy.csv` | `macro/analyst_unemployment.csv`, `macro/predictor_measures.csv` |
| `07_measurement_panels.R` | `_planning_data/person_level.rds`, `macro/population.csv`, `EUframes_cy.csv` | `teney_panel_weights.csv`, `teney_panel_measurement.csv` |
| `08_mi_tier.R` | `_planning_data/person_level.rds`, `EUframes_cy.csv`, `teney_panel_weights.csv` | `spec_grid_mi.csv`, `mi_diagnostics.csv` |
| `09_spec_curve.R` | `teney_panel_measurement.csv`, `teney_panel_weights.csv`, `macro/predictor_measures.csv`, `EUframes_cy.csv`; for its check, the audit file named below | `spec_curve.csv`, `spec_curve_negative.csv` (864 rows each) |
| `10_curve_forks.R` | the two curves | `spec_curve_forks.csv` |
| `11_menu_deviations.R` | the two curves, the inputs of `09`, `spec_grid_mi.csv` | `menu_deviations_rows.csv.gz`, `menu_deviations.csv` |
| `12_displays.R` | the inputs of `09`, `spec_curve.csv` | `display_between_within.csv`, `display_country_means.csv`, `display_periods.csv`, `display_loo.csv`, `display_bailout.csv`, `display_inference.csv` |
| `13_reference_marks.R` | `_workspaces/eu-frames/data_private/eb_respondents.rds` through `companion/_model-outputs/_load_respondents.R`, `analysts5.csv`, `spec_curve.csv`, the inputs of `09` | `companion/_model-outputs/reference_refits.csv`, `reference_marks.csv` |
| `14_dag_answers.R` | nothing; the graphs are those of the graph cell of Task 2 in `cases/eu-frames/lab.qmd` | `dag_answers.csv` |
| `check_parity.R` | the committed files and a rebuild of them in another folder | nothing; prints the comparison |

The stages are numbered from 07 after the macro series because the numbers 03 to 06 belonged to the stages that built the grids of the menu before the audit of October 2026, which the curve of `09` and the comparisons of `11` supersede; they remain in the history of the repository. The curve of `09` is the menu that the audit keeps, and `10` to `13` are built on it. `14` reads no data: it writes the adjustment sets that dagitty gives for every graph that the graph cell of Task 2 of the browser lab can build, which the lab prints because dagitty cannot run there. Three stages read licensed microdata: `01` builds the respondent file, `07` and `08` aggregate it, and `13` refits two respondent-level designs on the respondent file that the workspace builds. Everything else runs from committed aggregates.

The significance rule of the curve is CR2 errors clustered by country with Satterthwaite degrees of freedom (Bell and McCaffrey 2002; Pustejovsky and Tipton 2018), computed by `clubSandwich::coef_test()` in `fit_linear()` of `_curve_tools.R`. It is computed in closed form, so any copy of the fit on the same cells reproduces a stored p-value exactly. Conventional errors and CR1 on G − 1 df, both from `fixest`, are stored beside it as the teaching contrasts. Every other statistic comes from a package as well: the partial correlation from `effectsize::t_to_r()`, which computes the conversion rule of Multi100; the pooling of the imputations from `mice::pool.scalar()`; the within–between split from `datawizard::demean()`; the clustered tests of the random-effects and beta fits from `plm`, `sandwich` and `lmtest`. Two lines carry the mark `# r-style: PENDING`: Kish's effective sample size in `07`, for which no installed package offers a function.

## Locations

`_paths.R` sets four locations, each of which an environment variable can redirect: the destination of every file that the site reads (`COURSE_OUT_DIR`, by default `cases/eu-frames/data`), a work folder for intermediate tiers that only later stages read (`COURSE_WORK_DIR`, by default `_grid_work`), the private folder for the licensed files (`COURSE_PRIVATE_DIR`, by default `_planning_data`), and the folder of the raw GESIS waves (`EB_RAW_DIR`, by default `_planning_data/raw`). A rebuild is first written to a folder outside `cases/eu-frames/data/` and compared with the committed files by `check_parity.R`, which compares every file that both builds hold; the committed files are replaced only once the comparison has been read. Stages 09 to 13 read the outputs of `02` and `07` through the same setting, so a rebuild folder for them needs those files as well.

## Inputs that no script builds

- `EUframes_cy.csv` – the course panel, rebuilt from the raw GESIS files; its codebook is beside it.
- `analysts5.csv` – the Task 2 results of the five Multi100 analysts, from the public Multi100 data. The effect size of 018OL carries the sign of the Multi100 record (d = −0.121).
- `macro/population.csv` – total and adult population by country-year, World Bank `SP.POP.TOTL` less the share aged 0–14 (`SP.POP.0014.TO.ZS`), as retrieved in September 2026. It is an input of `07`.
- `data-raw/eu-frames/eurostat/` – the five Eurostat tables from which the analysts' unemployment series and the Eurostat growth series are cut, stored as retrieved; their provenance is in `data-raw/eu-frames/eurostat/README.md`.
- `_workspaces/eu-frames/data_private/eb_respondents.rds` – the respondent file that the workspace builds with `prep/01_build_respondents.R` from the participant's own GESIS downloads. It carries the occupation, community type and education bands that the designs of `13` need and `person_level.rds` does not. It is licensed microdata and never committed.
- `_planning/audit-2026-10/out/menu_audited_maximal.csv` – the refit of the audit, against which `09` checks the partial correlations and the CR2 p-values of the curve. It is a check, not an input of any output; the environment variable `COURSE_AUDIT_FILE` can point to a copy elsewhere, and `09` skips the check with a message where the file is absent.

## Packages

datawizard, insight, dplyr, tidyr, purrr, readr, tibble, haven, fixest, clubSandwich, effectsize, plm, lmtest, sandwich, betareg, lme4, lmerTest, mice, Amelia and dagitty; stringr, jsonlite, htmltools and rlang for the chart helper. Every function is called with its namespace, and no script attaches a package. The datawizard verbs evaluate their expressions outside the calling function, so inside a function whose filters or new columns name its arguments the scripts use dplyr, and `dplyr::arrange()` sorts every written file, because it sorts in the C locale on every machine.
