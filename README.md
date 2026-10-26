# Applied Replication for Data Skills

A hands-on course in applied replication for data skills, taught on four published claims from the social sciences and deliverable as anything from a short session of two to three hours to a semester. Facilitator: [Chris Moreh](https://chrismoreh.com). This repository holds the course website, the reveal.js slide decks, a zero-install browser lab for each case, the teaching modules of the companion curriculum, and the pipelines that build the data files of the cases.

Each case is a claim that five independent analysts reanalysed for the Multi100 project, published in *Nature* (Aczel et al., 2026) in a special section alongside two other papers from the same multi-year meta-project, *Systematizing Confidence in Open Research and Evidence* (SCORE; Alipourfard et al., 2021). In every case participants follow the same five tasks: they check their setup, describe the data and write the estimand behind the claim with a causal graph, reproduce the value that Multi100 recorded for one of the five analysts, explore a menu of defensible analyses one decision at a time, preregister one of them in a shortened template before running it, and submit their results to a live class chart drawn over the curve of specifications that the course fits.

## Website

The site deploys to GitHub Pages at <https://codemoreh.github.io/applied-replication/>.

## The four cases

The cases are ordered by the statistical and data-handling difficulty that they carry, and the opening page of the site presents them in that order.

| Case | Claim | Data | Methods it carries |
|---|---|---|---|
| The housing-audit case (`housing`) | Einstein and Glick (2017, *American Journal of Political Science*): emails signed with Hispanic names were greeted by name less often | One public file of 1,014 emails (Harvard Dataverse, CC0) | Proportions, the chi-square test, linear probability against logit, conditioning on a post-treatment variable, randomisation inference, power |
| The immigration-and-trust case (`trust`) | McLaren (2012, *World Politics*): concern about immigration lowers trust in politics | European Social Survey, rounds 1–4, as an extract shipped with the course (CC BY-NC-SA 4.0) | Attitude scales and their reliability, regression with controls, survey weights, country fixed effects against random intercepts, few-cluster inference |
| The cohabitation case (`cohabitation`) | Kuo and Raley (2016, *Demography*): cohabitors became less likely to marry | National Survey of Family Growth, 1995 and 2006–2010 (public domain) | Person-period data, discrete-time event-history models, competing risks, comparisons across periods, design-based errors |
| The EU-frames case (`eu-frames`) | Teney (2016, *European Sociological Review*): poor national economic performance lowers the positive framing of the EU | Eurobarometer microdata from GESIS (registration required, no redistribution), aggregated to a country-year panel | Aggregation of repeated cross-sections, panel fixed and random effects, the within–between model, few-cluster inference, a multiverse at scale |

## How the work is done

Participants install R and an IDE (Positron, RStudio, VS Code, etc.) and download the workspace of their case as a zip from its companion repository, unzip it, and open the folder in their IDE. None of this involves git or an account. Each workspace holds the report with Tasks 1–5 and the preregistration block, a working script with the same steps, the helper functions, and the data with their codebooks; in the EU-frames case participants download the Eurobarometer files from GESIS themselves, because the licence forbids redistribution. Models are fitted live.

| Case | Workspace repository |
|---|---|
| housing | `CodeMoreh/replication-lab-housing` |
| trust | `CodeMoreh/replication-lab-trust` |
| cohabitation | `CodeMoreh/replication-lab-cohabitation` |
| eu-frames | [`CodeMoreh/replication-lab`](https://github.com/CodeMoreh/replication-lab) |

The browser lab of each case (`cases/<case>/lab.qmd`) holds the same Tasks 1–5 on webR and quarto-live, with the data loaded in the page and nothing installed. It is the page that the facilitator uses on a shared screen, a place for anyone to try the exercise without an installation, and the fallback when a machine fails. Both routes submit results through pre-filled links to one results Form, `submit_exploration()` for the exploratory result of Task 4 and `report_result()` for the preregistered result of Task 5, and each adds a mark to the live class chart on the multiverse page of the case. The labels of the housing, trust and cohabitation cases carry the name of the case after the tag of the participant, and each chart reads only the rows of its own case.

## Repository layout

Everything that belongs to a case carries its name in its path (`cases/<case>/`, `R/<case>/`, `data-raw/<case>/`, `tracks/<case>-<format>/`, `slides/<case>-<format>.qmd`, `_workspaces/<case>/`).

| Path | What it is |
|---|---|
| `index.qmd` | The opening page: the four cases, the tracks grouped by case, the five tasks, the course, getting started, the facilitator |
| `setup.qmd`, `help.qmd` | Installing R, Quarto and an IDE, downloading and checking a workspace; known setup problems and their fixes |
| `resources.qmd` | The glossary, the papers, the data and the public records of the cases, the readings and the tool documentation |
| `cases/<case>/index.qmd` | The case study: the original study, SCORE and Multi100, the five reanalyses with an assessment of their choices, their standardised results, and the data; every track of the case starts here |
| `cases/<case>/exercise.qmd` | The exercise overview: the claim, the five analysts, Tasks 1–5 |
| `cases/<case>/lab.qmd` | The browser lab: Tasks 1–5 in the browser (webR, quarto-live) |
| `cases/<case>/menu.qmd` | The specification menu: the decisions of an analysis of the claim at three levels, the curve of specifications that the defensible options make, and the reason for every option on and off the curve |
| `cases/<case>/map.qmd` | The decision map (OJS): every decision in the order in which an analyst meets it, with the estimand statement that the choices write and what each option does to the results |
| `cases/<case>/multiverse.qmd` | The curve of the claim with the reference marks, the displays beside it, and the live class chart |
| `cases/<case>/cheatsheet.qmd` | A page to print: the addresses, the helpers, the options, the two submissions and the fallbacks |
| `cases/eu-frames/reading-inference.qmd` | The reading on why the specifications of a curve are not independent tests |
| `cases/<case>/data/` | The committed data of the case and their codebooks, served with the site so that webR can read them (see below) |
| `tracks/` | The tracks, one folder each with a docked sidebar in `_sidebar.yml`, named by the case and the format: `housing-short` and `housing-one-day`; `trust-one-day`; `cohabitation-one-day`; the six tracks of the EU-frames case (`eu-frames-one-day-replication`, `eu-frames-one-day-tools`, `eu-frames-one-day-methods`, `eu-frames-three-days`, `eu-frames-semester`, `eu-frames-self-study`) |
| `slides/` | The reveal.js decks, named by the case and the format, each assembled from segments in `_segments/<case>/` and the case-neutral `_segments/shared/`; the slides page of each track embeds the deck that the track uses |
| `companion/` | Eighteen teaching modules in three strands (computational tools; data management and sharing; statistical methods), shared by the cases, each naming at its top the cases on which it works; `companion/_model-outputs/` holds the scripts and aggregate outputs behind the EU-frames sections |
| `facilitator/` | The run sheet of each case (`<case>-guide.qmd`) and the notes on the formats (`tracks.qmd`), rendered but not linked; the setup note for the class-results Form and the code of its Cloudflare Worker |
| `R/<case>/` | The pipeline of each case, with a README that gives the order of the scripts and what each one reads and writes, the estimator of the case in `_tools.R` (copied verbatim into the workspace and the browser lab), and a parity check against established packages |
| `data-raw/<case>/` | The committed raw inputs of each case and their provenance; person-level files that may not be committed stay in a gitignored `cache/` |
| `data/` | Data that more than one case uses: the published estimates of the 29 teams of Silberzahn et al. (2018), with their codebook |
| `_includes/`, `_variables.yml` | The next-delivery callout of the opening page and its script, the navbar item that shows the chosen track; site-wide facts, the addresses of each case, and the block that schedules a delivery |
| `style/`, `assets/` | The site and slide themes, the filter that prints the case label on every case page and companion module, and the chart code of the multiverse pages |
| `.github/workflows/publish.yml` | CI: render and deploy to GitHub Pages. Every executable page comes from the committed `_freeze/` except the opening page, so the workflow installs R with only the packages that its chunk and the render need (knitr, rmarkdown, yaml, readr and datawizard) |

## The data

Each case keeps its data in `cases/<case>/data/`, each file with a codebook beside it. Every case has the same files around its curve: `spec_curve.csv` (one row for each specification of the curve, with its estimate on the scale of the claim, its test and the partial correlation of Multi100), `menu_options.csv` (every option of the menu, on the curve or off it, with the reason for its place and the page that teaches it; written by hand, and read by the menu, the decision map and the helpers that refuse an option off the curve), `menu_deviations.csv` (each option set beside its twins on the curve), `reference_marks.csv` (the published result and the five reanalyses placed on the scale of the curve), `analysts5.csv` (the five Multi100 analyses as recorded, with the course refit of each), the `display_*.csv` files of the teaching displays, and `class_results.csv` (the fallback of the live class chart when its feed cannot be reached).

The data of the analyses themselves:

| Case | File | What it is |
|---|---|---|
| housing | `audit_original.csv` | The public file of the original authors as CSV, with their column names: 1,014 emails; the reproduction of Task 3 runs on it |
| housing | `housing_audit.csv` | The same emails with readable names and derived columns; every other exercise runs on it |
| trust | `trust_ess.csv` | The analysis extract of rounds 1–4 of the ESS for the 16 countries of the paper: 120,080 respondents, with the trust and immigration items, the controls of the paper and the weights |
| trust | `trust_macro.csv` | The country and country-round variables of the paper, for the multilevel models that the course fits as checks |
| cohabitation | `cohab_unions.csv` | Every first premarital cohabitation of the women of five cycles of the NSFG, one row per union |
| cohabitation | `cohab_months.csv` | The person-month file built from it for the two surveys of the paper |
| cohabitation | `score_person_months.csv` | The person-month file that SCORE built for its replication and Multi100 gave the five analysts, from the OSF; the reproduction of Task 3 runs on it |
| eu-frames | `EUframes_cy.csv` | The EU-frames panel of 270 country-years, rebuilt from the raw GESIS microdata; `EUframes_cy_edu.csv` splits it by education band |
| eu-frames | `teney_panel_measurement.csv`, `teney_panel_weights.csv`, `macro/` | The panel under every measurement variant, the weighting panel, and the unemployment, growth and population series |
| eu-frames | `rep_data.csv` | The file that analyst C6HJR submitted to Multi100, kept for provenance and for the exercise on the case page |
| eu-frames | `EUframes_person_full.csv` | A simulated person-level twin of the panel (41,660 rows), generated from committed aggregate tables; it contains no real respondent |

**OSF nodes of the EU-frames case.** The course fetches from [osf.io/6zqct](https://osf.io/6zqct), the facilitator's personal extended fork of the official archival Multi100 component for analyst C6HJR [osf.io/8rtwe](https://osf.io/8rtwe). The full SCORE dossier on the paper is [osf.io/h7432](https://osf.io/h7432). The OSF records of the other cases are listed on their case pages and in `resources.qmd`.

**Eurobarometer microdata are not redistributed.** The raw Eurobarometer microdata are licensed by GESIS and are not committed or redistributed here. Obtain them from GESIS (<https://search.gesis.org/>) under their usage terms to reproduce the data pipeline of the EU-frames case, or to build models that rely on individual-level data. Everything in the repository that was derived from them is an aggregate: country-year cell means, the same means split by education band, and tables of model results and summary statistics.

## Sources, credits and licensing

The data of the cases come under four different sets of terms, and each codebook states the terms of its file.

- **Housing-audit data.** The replication package of Einstein and Glick (2017) is in the public domain (CC0, Harvard Dataverse, doi:10.7910/DVN/1HOVTU), so the original file and every course file built from it may be committed and served.
- **ESS extract.** Adapted from ESS Rounds 1–4 (ESS ERIC, ess.sikt.no; the editions are listed in `cases/trust/data/trust_ess_codebook.md`). ESS data are licensed under CC BY-NC-SA 4.0, and so is `trust_ess.csv`: it may be shared and adapted for non-commercial purposes, with attribution, under the same licence. Anyone who wants the full files should download them from the ESS Data Portal. The country-level series of the case come from the World Bank (CC BY 4.0), Eurostat, ParlGov and Party Facts, as listed in `data-raw/trust/README.md`.
- **NSFG data.** The public-use files of the National Survey of Family Growth (National Center for Health Statistics) are in the public domain, and their use is bound by the NCHS data use restrictions: statistical reporting and analysis only, and no attempt to identify a respondent. The SCORE person-month file comes from an OSF project of the SCORE dossier (osf.io/b6p3j), which states that "All materials on this OSF project may be shared publicly".
- **Eurobarometer data.** The underlying microdata remain subject to GESIS terms. The growth and unemployment rates in the EU-frames panel come from World Bank Open Data (CC BY 4.0), and the alternative series from Eurostat; the bailout indicator is coded from European Commission records of EU financial assistance programmes.

The derived aggregate data, the code and the rendered website content are released under [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE), except `cases/trust/data/trust_ess.csv`, which carries the CC BY-NC-SA 4.0 licence of the ESS.
