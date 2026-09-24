---
title: "silberzahn29.csv – codebook"
---

The results of the 29 teams in Silberzahn et al. (2018), *Many Analysts, One
Data Set: Making Transparent How Variations in Analytic Choices Affect
Results*, **Advances in Methods and Practices in Psychological Science** 1(3):
337–356. One row per team. These are the numbers behind Figure 3 of the paper.

**Provenance.** Downloaded from the authors' public OSF project,
[osf.io/gvm2z](https://osf.io/gvm2z), component "Results: Data", file
`Crowdsourcing Effects in OR with Subgroups.csv`. Rebuilt by
`companion/_model-outputs/build_silberzahn29.R`, which fetches that file over
the network, renames the columns, and refuses to write unless the summary figures
reported in the paper still hold: 29 teams, median odds ratio 1.310, range 0.888 to 2.931,
20 significant.

Nothing here is licensed or restricted – these are published aggregate results
about 29 analyses, not the football data that the teams analysed.

## The question that the teams were given {#the-question-the-teams-were-given}

Are football referees more likely to give red cards to dark-skinned players
than to light-skinned ones? Every team received the same dataset – 2,053
players, 146,028 player–referee dyads – and answered in whatever way they
judged correct.

## Variables

| Column | Type | Meaning |
|---|---|---|
| `team` | integer | The identifier given to the team in the published figure. Runs 1–32 with gaps: three teams that began did not deliver a usable estimate, so there are 29 rows |
| `approach` | character | The team's own description of its method, verbatim from the published table |
| `distribution` | character | The response distribution that the team assumed: `Linear`, `Logistic`, `Poisson`, or `Other`. This is the grouping used in the paper, where `Other` is the `misc` category, holding the two teams (Dirichlet-process clustering, Tobit) that fit none of the three |
| `clustering` | character | How the team handled the non-independence of repeated players and referees: `random`, `clustered`, `fixed`, or `none` |
| `or` | double | The estimate made by the team, as an odds ratio. Above 1 means dark-skinned players are red-carded more often |
| `or_lo`, `or_hi` | double | The 95% interval, on the same scale |
| `significant` | logical | Whether that interval excludes 1. True for twenty teams and false for nine |

## Conversions and truncation {#two-conversions-worth-knowing-about}

Not every team reported an odds ratio. Four reported a correlation or a
standardised mean difference, and the script published with the paper converts
these to the odds-ratio scale so that all 29 can be plotted on one axis. The
conversion was made by Silberzahn and colleagues, in `1_meta_plot.R` in the same
OSF project, and it is the reason that the interval for team 21 is so wide: that
team reported a Tobit coefficient, which was converted to a correlation and then
to an odds ratio, and its uncertainty was converted along with it.

Teams 21 and 27 have intervals that run far past the right edge of any readable
plot (up to 11.5 and 78.7). The published figure truncates both at 5 and marks
them with an asterisk; the version in the deck does the same and notes the truncation.

## What the deck uses it for

- **Part 1**, before SCORE: Figure 3 of the paper, rebuilt with every team and
  its interval, grouped by distribution family. Silberzahn and colleagues ran the first many-analysts
  study, and SCORE later applied the same design at scale.
- **Part 2**, on "Two studies to set beside our five": the same 29 estimates as
  a single-row strip, a reminder before Auspurg and Brüderl's reanalysis.

The median of 1.310 is not marked on either figure. It is left for the later
discussion of how a multi-analyst study combines estimates at all, where
Silberzahn's median is set beside the Multi100 approach as one option among
several.
