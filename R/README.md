# The grid pipeline

The scripts in this folder build the specification grids that the course pages, the browser lab and the class chart read from `data/`. They are numbered in the order in which they run, and each one reads only the course panel, the public inputs listed below, and the outputs of the scripts before it. `_paths.R` is a sourced helper that names the four locations used throughout; it is not a pipeline stage.

## Order and dependencies

| Script | Reads | Writes |
|:--|:--|:--|
| `01_person_file.R` | the sixteen GESIS Eurobarometer files in `_planning_data/raw/`, `data/EUframes_cy.csv` | `_planning_data/person_level.rds` (respondent level, never committed) |
| `02_macro_series.R` | `data-raw/eurostat/*.tsv.gz`, `data/EUframes_cy.csv` | `data/macro/analyst_unemployment.csv`, `data/macro/predictor_measures.csv` |
| `03_spec_grid.R` | `data/EUframes_cy.csv` | `data/spec_grid.csv` (the canonical 840) |
| `04_family_grid.R` | `data/EUframes_cy.csv`, `data/spec_grid.csv` | `data/spec_grid_family.csv` (the family universe of 1,680) |
| `05_full_grid.R` | `data/EUframes_cy.csv`, `data/spec_grid_family.csv` | `data/spec_grid_full.csv` (the full universe of 2,520) |
| `06_fork_importance.R` | the three grids above, `data/analysts5.csv` | `data/fork_importance.csv` and five sibling decompositions |
| `check_parity.R` | the committed files and a rebuild of them in another folder | nothing; prints the comparison |

The three nested grids form a chain: `04` imports the 840 rows of `03` unchanged and adds their beta twins, and `05` imports the 1,680 rows of `04` unchanged and adds the growth predictor. Only `01` reads the licensed microdata, and nothing downstream of it in this table depends on it: the core grids run from the committed panel alone. The respondent file feeds the weighting, measurement and imputation tiers listed below.

## Locations

`_paths.R` sets four locations, each of which an environment variable can redirect: the destination of every file that the site reads (`COURSE_OUT_DIR`, by default `data`), a work folder for intermediate tiers that only later stages read (`COURSE_WORK_DIR`, by default `_grid_work`), the private folder for the licensed files (`COURSE_PRIVATE_DIR`, by default `_planning_data`), and the folder of the raw GESIS waves (`EB_RAW_DIR`, by default `_planning_data/raw`). A rebuild is first written to a folder outside `data/` and compared with the committed files by `check_parity.R`; the committed files are replaced only once the comparison has been read.

## Inputs that no script builds

- `data/EUframes_cy.csv` – the course panel, rebuilt from the raw GESIS files; its codebook is beside it.
- `data/analysts5.csv` – the Task 2 results of the five Multi100 analysts, from the public Multi100 data.
- `data/macro/population.csv` – total and adult population by country-year, World Bank `SP.POP.TOTL` less the share aged 0–14 (`SP.POP.0014.TO.ZS`), as retrieved in September 2026. It is an input of the weighting tier.
- `data-raw/eurostat/` – the five Eurostat tables from which the analysts' unemployment series and the Eurostat growth series are cut, stored as retrieved; their provenance is in `data-raw/eurostat/README.md`.

## Packages

dplyr, tidyr, purrr, readr, datawizard, haven, fixest, plm, betareg, glmmTMB, sandwich, lmtest and effectsize.

## Still to port

The scripts behind the remaining files that the course reads from `data/` are not yet in this folder. In pipeline order they are the measure grid (`data/spec_grid_measure.csv`), the weighting and measurement panels (`data/teney_panel_weights.csv`, `data/teney_panel_measurement.csv`), the weighting grid (`data/spec_grid_weights.csv`), the maximal tier of the linear family and its beta and multiple-imputation tiers (`data/spec_grid_mi.csv`, `data/mi_diagnostics.csv`), the decision-level summaries (`data/decision_levels_maximal.csv`, `data/decision_classes_maximal.csv`, `data/axis_level_summary_maximal.csv`) and the per-cell files under `data/grid/`. Until they are ported, those files are the committed copies and are not rebuilt here.
