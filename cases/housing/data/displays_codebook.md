# Codebook: the displays beside the curve

The curve of the housing-audit case (`spec_curve.csv`, [codebook](spec_curve_codebook.md)) keeps only the specifications that answer the claim as the course reads it. Several findings stay in the course as displays of their own, because each describes the experiment or answers a related but different question. The five files below hold the numbers behind those displays. They are written by `R/housing/05_displays.R` from the course file `housing_audit.csv` ([codebook](housing_audit_codebook.md)).

Every comparison in these files is a difference in proportions, the coefficient of a linear model of a 0/1 outcome on a 0/1 indicator of the group, in percentage points, with HC2 standard errors (parameters, with sandwich) and a two-sided t test on the residual degrees of freedom, the estimator of the reference specification of the curve. A negative difference means that the first group did less well than the white names. The shares are of the emails that each comparison counts, and the bounced emails are never counted.

## `display_names.csv` – the six names

Nine rows: one for each of the six names (`level` = `name`) and one for each of the three groups (`level` = `group`), for a table and a chart of what came back to each name.

| Column | Meaning |
|:--|:--|
| `level` | `name` or `group` |
| `name` | The name, or the group for a group row |
| `group` | `white`, `black` or `hispanic` |
| `name_sex` | `male` or `female`; empty for a group row |
| `sent` | The emails sent |
| `bounced`, `delivered` | The emails that bounced and those that were delivered |
| `replies` | The delivered emails that drew a reply |
| `coded` | The replies with a greeting code |
| `named` | The replies that opened with the name of the sender |
| `reply_pct`, `reply_lo`, `reply_hi` | The share of delivered emails that drew a reply, in per cent, with its 95% Wilson interval, which stays inside 0–100 for small groups: the score interval of `prop.test()` without the continuity correction |
| `named_pct`, `named_lo`, `named_hi` | The share of coded replies that opened with the name, with its 95% Wilson interval |
| `named_all_pct` | The named replies as a share of all delivered emails |

By group, the white names drew replies to 57.5% of 308 delivered emails and a named greeting in 61.1% of 175 coded replies, the Black names 60.7% of 326 and 56.1% of 196, and the Hispanic names 52.7% of 357 and 41.2% of 187. By name, the share of coded replies that opened with the name runs from 37.1% for Santiago to 62.0% for Emily.

## `display_outcomes.csv` – the four outcomes

Eight rows: four outcomes, each compared for the Hispanic names and for the Black names with the white names.

| Column | Meaning |
|:--|:--|
| `outcome` | `reply`, any reply, among the delivered emails; `reply24`, a reply within 24 hours, among the replies that carry the code; `named`, a reply that opens with the name, among the coded replies; `named_any`, a reply that opens with the name, among the delivered emails, an email without a reply counting as 0 and the uncoded replies left out |
| `contrast` | `hispanic` or `black`, compared with the white names |
| `b_pp`, `se_pp` | The difference in percentage points and its HC2 standard error |
| `t`, `df`, `p` | The t, its residual degrees of freedom and its two-sided p-value |
| `n` | The emails compared |
| `ci_low`, `ci_high` | The 95% interval of the difference in percentage points, from the same t on the residual degrees of freedom |

The Hispanic names drew a reply 4.8 points less often than the white names (p = 0.214, 665 emails), and the Black names 3.3 points more often (p = 0.404). Among replies, a reply within 24 hours came 2.1 points more often to Hispanic names (p = 0.642) and 4.5 points more often to Black names (p = 0.297). The named greeting is 20.0 points less common for Hispanic names among the coded replies (p = 0.0001, 362 replies) and 13.3 points less common among all delivered emails (p = 0.0001, 662 emails), against 5.0 points (p = 0.328) and 1.0 point (p = 0.789) for the Black names. The paper reports the gap in replies with p = .11, which is the one-tailed value of the same test.

## `display_subgroups.csv` – subgroups of the replies

Eight rows: the named greeting among the coded replies, Hispanic against white names, in subgroups that the curve leaves out because each answers a narrower question.

| Column | Meaning |
|:--|:--|
| `subgroup` | `all names`, the reference comparison; `male names (Santiago against Brett)`; `female names (Gabriela against Emily)`; `replies from officials with Hispanic names` and `replies from other officials`; and the communities in the lower, middle and top third of percent Hispanic |
| `outcome`, `contrast` | `named` and `hispanic` throughout |
| `b_pp` … `ci_high` | As in `display_outcomes.csv` |

The gap is −23.3 points between Santiago and Brett (p = 0.0009, 201 replies) and −15.7 points between Gabriela and Emily (p = 0.046, 161). Among the 28 replies from officials with Hispanic names it is −25.7 points (p = 0.184), and among the 334 replies from other officials −19.8 (p = 0.0003). By the Hispanic third of the community it is −20.7 points in the lower third (p = 0.031, 109 replies), −14.8 in the middle third (p = 0.096, 127) and −22.5 in the top third (p = 0.011, 121). The subgroup of officials is selected after the email arrived, and the thirds lack 5 of the 362 replies.

## `display_ri.csv` – the randomisation distribution

4,999 rows, one per reshuffle: the names of the 362 coded replies to Hispanic and white names, the rows of the reference specification, reshuffled among those replies by complete random assignment of the 187 Hispanic names (ri2), and the difference in the share of named greetings recomputed each time. The seed is 20261025.

| Column | Meaning |
|:--|:--|
| `draw` | The reshuffle, 1 to 4,999 |
| `b_pp` | The difference in percentage points, Hispanic minus white, after the reshuffle |

The central 95% of the reshuffled differences lies between −10.0 and +9.9 points, and the whole distribution between −17.8 and +23.2. One of the 4,999 reshuffles gives a difference at least as large in absolute value as the observed −19.97 points. The display shows what differences chance alone produces when the names are assigned at random to these replies.

## `display_power.csv` – the power of the two designs

48 rows: for two designs, the power of a two-sided test at the 5% level to detect a gap from 2 to 25 points in the share of named greetings, below a share of 61% for the white names, computed with `power.prop.test()`.

| Column | Meaning |
|:--|:--|
| `design` | `original (175 and 187 replies)`, the comparison of Hispanic and white names among the coded replies of the paper, or `replication (369 and 369 replies)`, the 738 replies of the SCORE replication split equally |
| `gap_pp` | The gap in percentage points, 2 to 25 |
| `n_per_group` | The replies per group: 181, the mean of 175 and 187, or 369 |
| `power` | The probability that the test rejects the null hypothesis of no gap when the gap is `gap_pp` |

The original comparison has a power of 0.48 for a gap of 10 points, 0.70 for 13 points and 0.97 for 20 points; the replication has 0.78, 0.95 and 1.00. The share of 61% for the white names in both designs is an assumption of the display, taken from the original data.

## `display_replication.csv` – the replication on the scale of the course

4 rows, written by `R/housing/06_replication.R` from the de-identified data of the SCORE replication, which the script fetches from the OSF (osf.io/xseqg) at run time; the licence of those data is unstated, so the course keeps only these aggregates. The replication emailed 3,050 authorities in 2020 with four male aliases, two meant to indicate a Hispanic and two a white sender, and its de-identified file holds 2,717 of those emails. Each row is the gap in greetings by name between the Hispanic and the white aliases, as a difference in proportions with HC2 errors, the analogue of the reference specification of the curve.

| Column | Meaning |
|:--|:--|
| `population` | `replies` (the emails that drew a reply) or `all` (every delivered email, no reply counting as no greeting) |
| `authorities` | `all`, or `sub-county`: the authorities that are not run by a county, which the replication kept to match Table 1 of the paper |
| `b_pp`, `se_pp` | The gap in percentage points and its HC2 standard error |
| `stat_type`, `stat`, `df`, `p` | The t statistic, its residual degrees of freedom and its two-sided p-value |
| `n` | The emails in the comparison |
| `rate_hisp`, `rate_comp` | The share, in per cent, that drew a reply opening with the name of the sender, for the Hispanic and the white aliases |

Among all 1,484 replies the gap is +0.0 points (52.4% against 52.4%), and among all 2,717 emails −1.6 points; none of the four is significant. The logit of the report with covariates in thirds, on the 1,243 sub-county replies that carry every covariate (one of the 1,244 lacks the poverty third), b = 0.051 and z = 0.45, is reproduced by the same data.
