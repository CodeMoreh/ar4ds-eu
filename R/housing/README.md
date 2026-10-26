# The pipeline of the housing-audit case

The scripts write every data file of the case into `cases/housing/data/` from three inputs: the public file of the original authors (Harvard Dataverse, doi:10.7910/DVN/1HOVTU, CC0) and the five-row extract of the Multi100 record, both in `data-raw/housing/`, and the de-identified data of the SCORE replication, which stage 06 fetches from the OSF. They run from the project root, in this order; `menu_options.csv` is written by hand and read by the helpers and the decision map.

| Stage | Reads | Writes |
|---|---|---|
| `01_audit_data.R` | `data-raw/housing/dataverse_1HOVTU/HousingAuditAJPSFinalPublic_Unidentified.dta` | `audit_original.csv` (the public file as CSV, original names), `housing_audit.csv` (the course file) |
| `02_spec_curve.R` | `housing_audit.csv` | `spec_curve.csv` (the 64 specifications, each with a randomisation test of 1,999 reshuffles; the slowest stage, several minutes) |
| `03_reference_marks.R` | `housing_audit.csv`, `data-raw/housing/multi100/multi100_einstein_extract.csv` | `analysts5.csv` (the five analyses refitted and checked against their recorded values), `reference_marks.csv` |
| `04_menu_deviations.R` | `spec_curve.csv` | `menu_deviations.csv` |
| `05_displays.R` | `housing_audit.csv` | `display_names.csv`, `display_outcomes.csv`, `display_subgroups.csv`, `display_ri.csv`, `display_power.csv` |
| `06_replication.R` (after 03) | the de-identified data of the SCORE replication, fetched from osf.io/xseqg into the gitignored `data-raw/housing/score_replication/` (the licence is unstated, so the file is never committed), `reference_marks.csv` | `display_replication.csv` (aggregates only), and the SCORE row of `reference_marks.csv` |
| `check_parity.R` | `housing_audit.csv`, `spec_curve.csv` | nothing; stops if the estimator disagrees with estimatr or marginaleffects, or the committed curve with a fresh fit |

`_tools.R` holds the estimator that every stage sources: `fit_spec()` and its parts (`spec_frame()`, `vcov_hc2()`, `ame_logit()`, `r_multi100()`, `check_spec()`, `spec_line()`). It uses stats and dplyr only, because the downloadable workspace and the browser lab carry the same functions verbatim; a change here must be copied into `_workspaces/housing/R/spec_tools.R` and into the helper chunk of `cases/housing/lab.qmd`, and checked again with `check_parity.R`.

Dependencies: tidyverse and haven for the stages; estimatr and marginaleffects for the parity check only.
