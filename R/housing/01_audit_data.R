################################################################################
# Title:        01_audit_data.R
# Purpose:      The two data files of the housing-audit case: the public file of
#               the original authors as plain CSV with its own column names, and
#               the course file with readable names and the derived columns.
# Reads:        data-raw/housing/dataverse_1HOVTU/HousingAuditAJPSFinalPublic_Unidentified.dta
# Writes:       cases/housing/data/audit_original.csv, cases/housing/data/housing_audit.csv
# Author:       Claude for CM
# Last updated: 2026-10-25
################################################################################

library(tidyverse)
library(haven)

# CC0 replication package, Harvard Dataverse doi:10.7910/DVN/1HOVTU. The
# community variables are thirds of the 2012 ACS distributions (1 lower,
# 2 middle, 3 top) because the continuous values would identify the housing
# authorities; 14 authorities have none. The source labels HispanicEmailer
# "Hard to find email address", a copy error: it records whether the official
# who replied had a distinctively Hispanic name.
raw <- read_dta("data-raw/housing/dataverse_1HOVTU/HousingAuditAJPSFinalPublic_Unidentified.dta")

original <- raw |>
  mutate(across(where(is.labelled), \(x) as.numeric(x))) |>
  zap_label() |>
  zap_formats()
write_csv(original, "cases/housing/data/audit_original.csv", na = "")

names_key <- tibble(
  NumberAssignment = 1:6,
  name = c("Tyrone", "Shanice", "Santiago", "Gabriela", "Brett", "Emily"),
  group = c("black", "black", "hispanic", "hispanic", "white", "white"),
  name_sex = c("male", "female", "male", "female", "male", "female")
)

audit <- original |>
  mutate(email_id = row_number()) |>
  left_join(names_key, by = "NumberAssignment") |>
  transmute(
    email_id, name, group, name_sex,
    hard_email = HardEmail,
    reply = Response_Dichotomous,          # missing = the email bounced
    reply24 = Respond24,                   # replies only
    named = ProperName,                    # replies only; 5 replies uncoded
    hispanic_official = HispanicEmailer,   # replies only
    waitlist = na_if(WaitListFinal, ""),
    white_third = white_pop_third,
    black_third = black_pop_third,
    hispanic_third = hispanic_pop_third,
    pop_third = total_pop_third,
    poverty_third = poverty_pop_third,
    assessment_third = assesment_third
  ) |>
  mutate(across(ends_with("_third"), \(x) factor(x, levels = 1:3, labels = c("lower", "middle", "top"))))

stopifnot(
  nrow(audit) == 1014,
  sum(is.na(audit$reply)) == 23,
  sum(audit$reply == 1, na.rm = TRUE) == 563,
  sum(audit$reply == 1 & is.na(audit$named), na.rm = TRUE) == 5,
  all(is.na(audit$named[audit$reply %in% 0]))
)

write_csv(audit, "cases/housing/data/housing_audit.csv", na = "")
