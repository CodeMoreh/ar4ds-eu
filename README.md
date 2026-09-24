# Applied Replication for Data Skills

A hands-on course in applied replication for data skills, deliverable from one day to a semester, with a one-day workshop at its centre. Facilitator: [Chris Moreh](https://chrismoreh.com). This repository holds the course website, the reveal.js slide decks and a zero-install browser lab. Most recently delivered online at the Făgăraș Summer School, 3 August 2026.

Course participants will complete a guided robustness reanalysis of an article that the facilitator reanalysed as part of a large replication exercise, recently published in *Nature* (Aczel et al., 2026) in a special section alongside two other papers from the same multi-year meta-project, *Systematizing Confidence in Open Research and Evidence* (SCORE; Alipourfard et al., 2021). Participants will import data directly from an OSF repository and reproduce a constrained model, then choose one analytical deviation, make a reasoned argument for their choices supported by a directed acyclic graph (DAG), preregister that choice in a shortened but real preregistration template completed before anything is run, and submit the result to a live class specification curve.

## Website

The site deploys to GitHub Pages at <https://codemoreh.github.io/applied-replication/>.


## Course outline

The session covers four topics. The first is the three Rs – reproducibility, robustness and replicability – grounded in three *Nature* (2026) reports that measured these outcomes across the SCORE sample of published social and behavioural science articles. The second traces those three Rs through the EU-frames case, a 2016 study by the original author (OA) of the correlates of EU framing in Eurobarometer data. The third is estimands and DAGs: distinguishing theoretical, empirical and estimation targets, and reading confounders, mediators and colliders off causal graphs. The fourth is the specification menu and the multiverse, which show what each analytical choice does to the estimate, and what a curve of results across a whole set of defensible analyses can and cannot establish.

## How the work is done

Participants install R and an IDE (Positron, RStudio, VS Code, etc.) and download the workspace as a zip from the companion repository [`CodeMoreh/replication-lab`](https://github.com/CodeMoreh/replication-lab), unzip it, and open the folder in their IDE. None of this involves git or an account. The folder holds the working script, the helper functions, the data and a report skeleton containing Tasks 1–5 and the preregistration block. Models are fitted live.

The [browser lab](exercise/browser-lab.qmd) holds the same Tasks 1–5 on webR and quarto-live, with the data pre-loaded and nothing installed, reading its results from the pre-computed grid. It is the page that the facilitator uses on a shared screen, a place for anyone to try the exercise without an installation, and the fallback when a machine fails. Both submit results through the same one-click links, `submit_exploration()` for the exploratory result of Task 4 and `report_result()` for the preregistered result of Task 5, and each adds a point to the live [Multiverse](results.qmd) chart.

## Repository layout

| Path | What it is |
|------------------------|-----------------------------------------------|
| `index.qmd` | Home: welcome, overview, how the work is done, schedule |
| `setup.qmd` | Pre-workshop preparation: the installation, the browser lab, what you do not need, optional readings |
| `tracks/` | Six tracks through the same material, one folder each with a docked sidebar: three one-day formats (open science and replication; social data science tools; the statistical methods), three days, a semester, self-study |
| `slides/` | The reveal.js decks: `index.qmd` is the full deck, the variants (`one-day-*.qmd`, `three-days.qmd`) include the same segment files under `_segments/` in their own order; the slides page of each track embeds the deck that the track uses |
| `exercise/index.qmd` | Full task description: the claim, the five analysts, Tasks 1–5 |
| `exercise/browser-lab.qmd` | Tasks 1–5 running in the browser (webR / quarto-live), for demonstration and practice |
| `exercise/spec-menu.qmd` | Specification menu for the chosen deviation: fourteen axes, grouped into estimand, measurement and estimation decisions |
| `exercise/estimand-dial.qmd` | Estimand choices (OJS): set the fourteen choices, read the question that the seven Level 1 choices ask, and watch how far the measurement and estimation options of that one question spread |
| `exercise/cheatsheet.qmd` | Wallet card: URLs, the steps of the two submissions to the chart, fallbacks |
| `companion/` | Sixteen self-study modules in three strands, which hold most of the teaching in the course |
| `results.qmd` | Multiverse: specification curve chart and five-analyst comparison |
| `resources.qmd` | Readings, estimand theory, case-study references, tool docs |
| `data/` | Committed course datasets and codebooks (see below) |
| `.github/workflows/publish.yml` | CI: render and deploy to GitHub Pages |

## The data

The `data/` directory holds the committed course datasets and their codebooks, served verbatim so the browser lab can fetch them.

| File | What it is |
|------------------------|-----------------------------------------------|
| `rep_data.csv` | The as-submitted Multi100 artefact (270 country-years), kept for provenance; nothing on the site reads it |
| `EUframes_cy.csv` | The EU-frames panel, rebuilt from the raw GESIS microdata |
| `EUframes_cy_codebook.md` | Codebook for `EUframes_cy.csv` (sources, scales, variables, licence) |
| `EUframes_cy_edu.csv` | The same panel split by education band (810 rows: 27 countries × 10 years × 3 bands), for the individual-level extension, with its own codebook |
| `spec_grid.csv` | 840 pre-computed specifications across the task menu |
| `spec_grid_family.csv` | The family universe of 1,680: the 840 above plus a beta twin of each |
| `spec_grid_full.csv` | 2,520 specifications, adding GDP growth as a predictor on which the claim is tested |
| `grid/` | The maximal fourteen-axis grid, one gzipped file for each combination of six of the seven Level 1 choices (the seventh, the scoring rule, selects rows within the file), with `manifest.csv`, `axes.csv`, `canonical_rca.csv` and `cells_summary.csv` alongside; the browser lab and the estimand choices page read these |
| `spec_grid_mi.csv` | Pooled multiple-imputation results for the no-opinion axis (mice and Amelia, ten imputations each), with `mi_diagnostics.csv` |
| `spec_grid_measure.csv`, `spec_grid_weights.csv` | The measure axis and the weighting axes fitted around the grid of 840 |
| `teney_panel_measurement.csv` | The country-year panel under every measurement variant: outcome construction, item set, no-opinion rule, cell weighting |
| `teney_panel_weights.csv` | Per-cell respondent counts, effective sample sizes, no-opinion counts and adult population |
| `macro/predictor_measures.csv` | The published unemployment and growth series that the measure axis chooses between |
| `decision_levels_maximal.csv`, `decision_classes_maximal.csv`, `axis_level_summary_maximal.csv` | Every axis of the maximal grid classified as an estimand, measurement or estimation decision, with the share of variance and of p = .05 crossings attributable to it, the nested share by level, and the claim and significance shares for every level |
| `fork_importance.csv` | Share of specification variance attributable to each menu axis |
| `analysts5.csv` | The five Multi100 analysts' results for the constrained claim |
| `silberzahn29.csv` | The published estimates of the 29 teams in the red-card study of Silberzahn et al. (2018), with its own codebook |
| `class_results.csv` | Committed fallback for the live class chart when the results feed is unreachable |
| `EUframes_person_full.csv` | Simulated person-level twin of the panel (41,660 rows), with its own codebook alongside |

**OSF nodes.** The course fetches from [osf.io/6zqct](https://osf.io/6zqct), the facilitator's personal extended fork of the official archival Multi100 component for analyst C6HJR [osf.io/8rtwe](https://osf.io/8rtwe). The full SCORE dossier on the reanalysed paper is [osf.io/h7432](https://osf.io/h7432).

**Microdata are not redistributed.** The raw Eurobarometer microdata are GESIS-licensed and are not committed or redistributed here. Obtain them from GESIS (<https://search.gesis.org/>) under their usage terms if you want to reproduce the whole data pipeline, or to build alternative models that rely on individual-level data. The repository holds no respondent-level survey data. Everything in it that was derived from the microdata is an aggregate: country-year cell means, the same means split by education band, and tables of model results and summary statistics. It also holds a simulated person-level twin, which was generated from committed aggregate tables and contains no real respondent.

## Sources, credits and licensing

The growth and unemployment rates in the panel come from World Bank Open Data (CC BY 4.0), and the alternative series on the measure axis come from Eurostat. The bailout indicator is coded from European Commission records of EU financial assistance programmes. The underlying Eurobarometer microdata remain subject to GESIS terms. The derived aggregate data and the rendered website content are released under [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE).