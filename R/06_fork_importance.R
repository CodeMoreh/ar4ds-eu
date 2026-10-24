##############################################################################
# Title:        06_fork_importance.R
# Purpose:      Report of the summaries of the three nested universes that the
#               course quotes, and the variance decompositions behind its fork
#               chart: the canonical 840 (linear family, unemployment
#               predictor, the resolution of Task 2 of Multi100), the family
#               universe of 1,680, and the full universe of 2,520. Each
#               decomposition runs on two scales, as estimated and
#               claim-aligned, because the raw scale confounds the polarity of
#               the framings and of the two economic measures with
#               disagreement about the estimate. Shares are non-partial
#               eta-squared from a sequential main-effects fit; the residual
#               is the complement, which is the interaction structure between
#               axes. The printed tables are the output of the script.
# Reads:        data/spec_grid.csv, data/spec_grid_family.csv,
#               data/spec_grid_full.csv, data/analysts5.csv
# Writes:       data/fork_importance.csv, fork_importance_aligned.csv,
#               fork_importance_family.csv, fork_importance_family_aligned.csv,
#               fork_importance_full.csv, fork_importance_full_aligned.csv
# Author:       Chris Moreh
# Last updated: 2026-10-24
##############################################################################

suppressPackageStartupMessages({ library(dplyr); library(purrr) })
source("R/_paths.R")

sp   <- datawizard::data_read(out_path("spec_grid.csv"), verbose = FALSE)
fam  <- datawizard::data_read(out_path("spec_grid_family.csv"), verbose = FALSE)
full <- datawizard::data_read(out_path("spec_grid_full.csv"), verbose = FALSE)
an   <- datawizard::data_read("data/analysts5.csv", verbose = FALSE)

# claim alignment: reverse-code the negative framings as 1 - y, and flip the
# growth predictor, whose claim-expected sign is the opposite of that of
# unemployment, so that r_ca < 0 marks support for the claim everywhere.
# |r_ca| = |r| exactly, so every magnitude summary is invariant to it.
align <- function(d) {
  pol <- if_else(d$positive_outcome, 1, -1)
  gro <- if ("predictor" %in% names(d)) if_else(d$predictor == "growth", -1, 1) else 1
  mutate(d, r_ca = r * pol * gro)
}
sp   <- align(sp)
fam  <- align(fam)
full <- align(full)

eta_shares <- function(data, resp, axes) {
  fml <- as.formula(paste(resp, "~", paste(axes, collapse = " + ")))
  e <- effectsize::eta_squared(aov(fml, data = data), partial = FALSE, verbose = FALSE)
  tibble(term = c(e$Parameter, "Residuals"),
         share = round(100 * c(e$Eta2, 1 - sum(e$Eta2)), 1)) |>
    arrange(desc(share))
}

axes6 <- c("outcome", "pred_form", "copredictor", "estimator", "sample", "weights")
axes7 <- c(axes6, "family")
axes8 <- c("outcome", "predictor", "pred_form", "copredictor", "estimator",
           "sample", "weights", "family")

describe <- function(d, label) {
  tibble(universe = label, specs = nrow(d),
         claim_dir = round(100 * mean(d$supports_claim), 1),
         significant = round(100 * mean(d$sig), 1),
         both = round(100 * mean(d$sig & d$supports_claim), 1),
         min_r_ca = round(min(d$r_ca), 3), max_r_ca = round(max(d$r_ca), 3),
         median_r_ca = round(median(d$r_ca), 3),
         median_abs_r = round(median(abs(d$r)), 3))
}

# the three nested universes
bind_rows(describe(sp, "canonical 840"), describe(fam, "family 1,680"),
          describe(full, "full 2,520")) |>
  as.data.frame() |>
  print(row.names = FALSE)

# sub-universes of the full grid, claim-aligned
full |>
  summarise(n = n(),
            claim  = round(100 * mean(supports_claim), 1),
            signif = round(100 * mean(sig), 1),
            lo = round(min(r_ca), 3), hi = round(max(r_ca), 3),
            med = round(median(r_ca), 3),
            .by = c(predictor, family)) |>
  as.data.frame() |>
  print(row.names = FALSE)

# the constrained universe (cosmopolitan scale only) and its wrong-sign anatomy
sub <- sp |> filter(outcome == "mcosmo")
tibble(specs = nrow(sub), negative = round(100 * mean(sub$estimate < 0), 1),
       significant = round(100 * mean(sub$sig), 1),
       both = round(100 * mean(sub$sig & sub$estimate < 0), 1)) |>
  as.data.frame() |>
  print(row.names = FALSE)
sub |> filter(!supports_claim) |> count(estimator) |> as.data.frame() |> print(row.names = FALSE)

# mean claim-aligned r by estimator (canonical 840)
sp |>
  summarise(n = n(), mean_r_ca = round(mean(r_ca), 3),
            median_r_ca = round(median(r_ca), 3), .by = estimator) |>
  as.data.frame() |>
  print(row.names = FALSE)

# the five recorded analysts on the canonical curve, with their statistics
# also converted on the 233 residual df of the two-way design (a teaching
# device for the conversion lesson, not the convention of the chart)
df_design <- 233
an |>
  transmute(analyst_id, statistic, recorded_r = r,
            r_on_233 = statistic / sqrt(statistic^2 + df_design),
            pct_more_supporting = map_dbl(r, \(x) 100 * mean(sp$r_ca < x))) |>
  mutate(across(where(is.double), \(x) round(x, 3))) |>
  as.data.frame() |>
  print(row.names = FALSE)

sp |>
  filter(baseline) |>
  transmute(b = signif(estimate, 3), t = round(statistic, 3), r = round(r, 3), df) |>
  as.data.frame() |>
  print(row.names = FALSE)

## ---- the six decompositions --------------------------------------------------

decomps <- list(
  list(file = "fork_importance.csv",                d = sp,   resp = "r",    axes = axes6),
  list(file = "fork_importance_aligned.csv",        d = sp,   resp = "r_ca", axes = axes6),
  list(file = "fork_importance_family.csv",         d = fam,  resp = "r",    axes = axes7),
  list(file = "fork_importance_family_aligned.csv", d = fam,  resp = "r_ca", axes = axes7),
  list(file = "fork_importance_full.csv",           d = full, resp = "r",    axes = axes8),
  list(file = "fork_importance_full_aligned.csv",   d = full, resp = "r_ca", axes = axes8)
)

walk(decomps, \(x) {
  tab <- eta_shares(x$d, x$resp, x$axes)
  print(as.data.frame(tab |> mutate(decomposition = x$file)), row.names = FALSE)
  datawizard::data_write(tab, out_path(x$file), verbose = FALSE)
})
