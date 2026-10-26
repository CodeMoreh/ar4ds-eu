# Multi100 record of the housing-audit claim

`multi100_einstein_extract.csv` holds the five rows of `multi100_processed_data.csv` for the paper `Einstein_AmJourPoliSci_2017_mxyQ`, one per analyst, with every Task 1 and Task 2 field and the cleaned effect sizes. The columns that carry analysts' names, sign-up details and timestamps are left out. The full file is public at github.com/marton-balazs-kovacs/multi100 (mirrored at osf.io/q5h2c); the extract was taken on 2026-10-25 from the copy fetched on 2026-07-18.

The project is reported in Aczel et al. (2026), "Investigating the analytical robustness of the social and behavioural sciences", *Nature*, doi:10.1038/s41586-025-09844-9. Its conversion rules, which `R/housing/_tools.R` follows in `r_multi100()`: a t statistic gives r = t / sqrt(t² + df); a z statistic gives r = tanh(|z| × sqrt(1 / (N − 3))) × sign(z), the sample size standing in for the missing degrees of freedom; a χ² is treated as the square of a z, r = tanh(sqrt(χ²) × sqrt(1 / (N − 3))).

The analysts' own scripts are on OSF, in the Multi100 component of the paper (osf.io/38rsh): 8PTRZ osf.io/j8wvh, UYV3F osf.io/3672s (a review document only), EWFMI osf.io/jhvca, 25N5D osf.io/v8zah, P9W8Y osf.io/pzwbm.
