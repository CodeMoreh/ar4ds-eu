# AR4DS – EU frames

The site of the EU-frames case of Applied Replication for Data Skills (AR4DS), a course in reproduction, robustness and replication in R taught on published claims. Facilitator: [Chris Moreh](https://chrismoreh.com). The site deploys to GitHub Pages at <https://codemoreh.github.io/ar4ds-eu/>; the other cases and the pages that every case shares are on the [AR4DS site](https://codemoreh.github.io/ar4ds/).

The case is a claim from a study of how Europeans frame the European Union (Teney, 2016, *European Sociological Review*): poor national economic performance lowers the positive framing of the EU. Five analysts reanalysed the claim for the Multi100 project, published in *Nature* (Aczel et al., 2026) as part of SCORE (Alipourfard et al., 2021), and a preregistered replication tested it on later Eurobarometer waves. The data are Eurobarometer microdata from GESIS, which require a registration and may not be redistributed, so the course aggregates them to a panel of 270 country-years.

## Formats

| Format | Folder | Taught in |
|---|---|---|
| One-day workshop, in four parts | `tracks/eu-frames-one-day/` | the browser lab, the workspace as an option |
| Extended course, three days built around the workshop | `tracks/eu-frames-extended/` | the workspace, with the browser lab on the day of the workshop |
| Self-study | `tracks/self-study/` | the workspace |

Each format has a docked sidebar (`_sidebar.yml`), a page for each of its parts, and a slides page that embeds its deck (`slides/eu-frames-*.qmd`, assembled from `slides/_segments/eu-frames/`). The run sheet of the facilitator, with the timings, is `facilitator/eu-frames-guide.qmd`.

## Layout

| Path | What it is |
|---|---|
| `index.qmd` | The home page |
| `cases/eu-frames/` | The case study, the exercise overview, the browser lab, the specification menu, the decision map, the multiverse page with the class chart, the cheatsheet, the reading on the specification curve, and `data/` with a codebook for every file |
| `tracks/` | The formats |
| `slides/` | The decks and their segments |
| `companion/` | The modules of the companion, written on the data of the case; `_model-outputs/` holds the scripts and aggregate outputs behind them |
| `R/eu-frames/` | The pipeline that builds the data files of the case; `R/eu-frames/README.md` gives the order |
| `data-raw/eu-frames/` | The archived Eurostat tables that the pipeline reads |
| `data/` | The published estimates of the 29 teams of Silberzahn et al. (2018), with their codebook |
| `facilitator/` | The run sheet and the notes on the formats, rendered but not linked; the setup note of the class-results pipeline and the code of its Cloudflare Worker, which the sites of all the cases use |
| `resources.qmd` | Readings, records and tools |
| `_variables.yml` | Site variables and the next-delivery block |

The downloadable workspace of the case is the repository [`CodeMoreh/replication-lab`](https://github.com/CodeMoreh/replication-lab), and the browser lab carries the same helper functions. Participants who build the panel from the raw files download the Eurobarometer waves from GESIS themselves, because the licence forbids redistribution.

## The data

The case keeps its data in `cases/eu-frames/data/`, each file with a codebook beside it.

| File | What it is |
|---|---|
| `EUframes_cy.csv` | The EU-frames panel of 270 country-years, rebuilt from the raw GESIS microdata; `EUframes_cy_edu.csv` splits it by education band |
| `teney_panel_measurement.csv`, `teney_panel_weights.csv`, `macro/` | The panel under every measurement variant, the weighting panel, and the unemployment, growth and population series |
| `rep_data.csv` | The file that analyst C6HJR submitted to Multi100, kept for provenance and for the exercise on the case page |
| `EUframes_person_full.csv` | A simulated person-level twin of the panel (41,660 rows), generated from committed aggregate tables; it contains no real respondent |

The files around the curve of the claim are `spec_curve.csv` (one row for each specification of the curve, with its estimate on the scale of the claim, its test and the partial correlation of Multi100), `menu_options.csv` (every option of the menu, on the curve or off it, with the reason for its place and the page that teaches it; written by hand, and read by the menu, the decision map and the helpers that refuse an option off the curve), `menu_deviations.csv` (each option set beside its twins on the curve), `reference_marks.csv` (the published result and the five reanalyses placed on the scale of the curve), `analysts5.csv` (the five Multi100 analyses as recorded, with the course refit of each), the `display_*.csv` files of the teaching displays, and `class_results.csv` (the fallback of the live class chart when its feed cannot be reached).

**OSF nodes of the case.** The course fetches from [osf.io/6zqct](https://osf.io/6zqct), the facilitator's personal extended fork of the official archival Multi100 component for analyst C6HJR, [osf.io/8rtwe](https://osf.io/8rtwe). The full SCORE dossier on the paper is [osf.io/h7432](https://osf.io/h7432).

**Eurobarometer microdata are not redistributed.** The raw Eurobarometer microdata are licensed by GESIS and are not committed or redistributed here. Obtain them from GESIS (<https://search.gesis.org/>) under their usage terms to reproduce the data pipeline of the case, or to build models that rely on individual-level data. Everything in the repository that was derived from them is an aggregate: country-year cell means, the same means split by education band, and tables of model results and summary statistics.

## Licence

The derived aggregate data, the code and the rendered website content are released under [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE). The underlying Eurobarometer microdata remain subject to the GESIS terms. The growth and unemployment rates in the panel come from World Bank Open Data (CC BY 4.0), and the alternative series from Eurostat; the bailout indicator is coded from European Commission records of EU financial assistance programmes.
