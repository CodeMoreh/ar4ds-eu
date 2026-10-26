# Codebook: the tables of the browser lab

The [browser lab](../lab.qmd) of the immigration-and-trust case is a demonstration lab: it fits no model and does not download the ESS extract. Every number that its Tasks 2 and 3 print comes from the four small files below, each written by the course pipeline with the code that the downloadable workspace runs for the same task, so that the lab and the workspace give the same numbers. The results of Tasks 4 and 5 are the rows of `spec_curve.csv` ([codebook](spec_curve_codebook.md)), Cronbach's alpha of Task 2 is the pooled row of `display_reliability.csv` ([codebook](displays_codebook.md)), and the answers of the causal-graph task are in `dag_answers.csv` ([codebook](dag_answers_codebook.md)). The first three files are written by `R/trust/06_displays.R` from `trust_ess.csv` ([codebook](trust_ess_codebook.md)), and `lab_reproduction.csv` by `R/trust/04_reference_marks.R`.

## `lab_counts.csv`

One row with the counts of the first cell of Task 2, as `datawizard::data_summary()` gives them on the extract: `respondents` (120,080), `countries` (16), `country_rounds` (60) and `paper_sample` (110,732, the citizens who do not belong to an ethnic minority, the sample of the paper).

## `lab_distribution.csv`

The distributions of the three measures of distrust and of concern about immigration in the sample of the paper, as `datawizard::describe_distribution()` gives them, with concern as the mean of the three immigration items for the respondents who answered all three. One row per variable (`distrust_parl`, `distrust_pol`, `distrust_legal`, `concern`), with the columns `Variable`, `Mean`, `SD`, `IQR`, `Min`, `Max`, `Skewness`, `Kurtosis`, `n` (the respondents with a value) and `n_Missing`. The mean distrust is 5.08 for parliament, 6.06 for politicians and 4.45 for the legal system, and the mean concern 4.85, all on scales from 0 to 10.

## `lab_countries.csv`

The 16 countries of the sample of the paper, one row each, as `datawizard::data_summary()` gives them by country: `cntry`, `r_within` (the correlation of concern with distrust of parliament among the respondents of the country who answered both), `concern` and `distrust_parl` (the unweighted means). The lab computes from these rows the correlation of the 16 means, 0.68, and draws them; the correlation within a country runs from 0.17 (Italy) to 0.38 (Norway).

## `lab_reproduction.csv`

The results that Task 3 shows in place of the reproduction, which a page in a browser cannot run, because the OSF does not let a page on another website read the file of the analysts. Three rows:

- `6U3LL`: the model of analyst 6U3LL on the file that Multi100 gave the analysts (osf.io/e48gd), a three-level model of Table 3 of the paper without unemployment, with random intercepts for country and country-round, fitted by lme4 (as lmerTest fits it, the lme4 fit wrapped) and read with the defaults of `parameters::model_parameters()`, as the workspace reads it. Its coefficient is 0.191 with *t* = 46.625 on 72,733 respondents in 57 country-rounds of 16 countries, the value that Multi100 recorded. This row is kept to six significant digits, the precision at which lme4 gives the same values from one run to the next; its degrees of freedom are the residual degrees of freedom that parameters reports for an lme4 fit, while Multi100 recorded the 72,700 Satterthwaite degrees of freedom of lmerTest.
- `6U3LL_fe`: the same individual-level model with fixed effects for country-round in place of the random intercepts and of the country-level variables, fitted by fixest with conventional errors: 0.191, *t* = 46.60.
- `course_no_it2`: the reference specification of the curve on the course extract without the Italian round 2, which the integrated ESS files of the SCORE reproduction osf.io/jp7tr lacked: 0.1772 on the 69,522 respondents of that reproduction, with errors clustered by country (*t* = 18.67 on 15 df) and a conventional *t* of 41.37.

The columns are `id`, `model` (a description of the row), `b` (points of distrust per point of concern), `se`, `ci_low` and `ci_high` (the 95% interval), `t`, `df`, `p`, `n`, `country_rounds`, `countries` (empty where the fit has no countries as groups or clusters) and `t_conv` (the conventional *t*, given only where the test of the row is another).
