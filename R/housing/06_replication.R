################################################################################
# Title:        06_replication.R
# Purpose:      The SCORE replication on the scale of the course: the gap in
#               greetings by name between Hispanic and white aliases in the new
#               audit, under the choices of the menu that its data allow, and
#               its mark on the curve of the claim.
# Reads:        the de-identified replication data, osf.io/xseqg (downloaded at
#               run time into data-raw/housing/score_replication/, gitignored),
#               cases/housing/data/reference_marks.csv
# Writes:       cases/housing/data/display_replication.csv,
#               cases/housing/data/reference_marks.csv (the SCORE row)
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
library(haven)
source("R/housing/_tools.R")

# The OSF component states no licence, so the file is fetched rather than
# committed, and only aggregate results leave this script. It holds 2,717
# delivered emails from four male aliases (two Hispanic, two white), no Black
# aliases; County = 1 marks authorities run by counties, which the replication
# left out to match Table 1 of the original.
RAW <- "data-raw/housing/score_replication/Audit_Deidentified.dta"
if (!file.exists(RAW)) {
  dir.create(dirname(RAW), recursive = TRUE, showWarnings = FALSE)
  download.file("https://osf.io/download/mka4e/", RAW, mode = "wb", quiet = TRUE)
}

repl <- read_dta(RAW) |>
  zap_labels() |>
  transmute(hisp = HispanicName, reply = Reply, named = Friendly, county = County)

stopifnot(nrow(repl) == 2717, sum(repl$reply) == 1484, all(repl$named <= repl$reply))

# The two populations of the menu; there are no uncoded replies and no Black
# aliases, so the comparison is white names under every option.
gap <- function(data, population, authorities) {
  frame <- if (population == "replies") filter(data, reply == 1) else data
  fit <- lm(named ~ hisp, data = frame)
  b <- unname(coef(fit)["hisp"])
  se <- sqrt(vcov_hc2(fit)["hisp", "hisp"])
  tibble(population = population, authorities = authorities,
         b_pp = 100 * b, se_pp = 100 * se, stat_type = "t", stat = b / se,
         df = fit$df.residual, p = 2 * pt(-abs(b / se), fit$df.residual), n = nrow(frame),
         rate_hisp = 100 * mean(frame$named[frame$hisp == 1]),
         rate_comp = 100 * mean(frame$named[frame$hisp == 0]))
}

replication <- expand_grid(population = c("replies", "all"), authorities = c("all", "sub-county")) |>
  pmap(\(population, authorities) gap(if (authorities == "all") repl else filter(repl, county == 0),
                                      population, authorities)) |>
  bind_rows()
write_csv(replication, "cases/housing/data/display_replication.csv")

# The mark: the raw gap among all replies, the analogue of the reference
# specification of the curve (white names, replies, no covariates)
ref <- filter(replication, population == "replies", authorities == "all")
marks <- read_csv("cases/housing/data/reference_marks.csv", show_col_types = FALSE) |>
  filter(id != "SCORE") |>
  bind_rows(tibble(
    id = "SCORE", label = "SCORE replication (new audit, 2020)", statistic_type = "t",
    statistic = ref$stat, n = ref$n, b_pp = ref$b_pp, on_chart = TRUE,
    note = "New data: 3,050 authorities emailed by four male aliases, two Hispanic and two white. Placed by the raw gap among all replies, computed by the course from the de-identified data on the OSF (osf.io/xseqg)."
  ))
write_csv(marks, "cases/housing/data/reference_marks.csv", na = "")
