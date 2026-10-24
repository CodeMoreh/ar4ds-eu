##############################################################################
# Title:        _paths.R – where the grid pipeline reads and writes
# Purpose:      One place for the four locations that the scripts in R/ use.
#               The course data folder is the default destination of every
#               file that the site reads; the work folder holds the large
#               intermediate tiers that only later stages read; the private
#               folder holds the GESIS-licensed waves and the respondent file
#               built from them, which never enter git. Each location can be
#               redirected by an environment variable, so that a rebuild can
#               be written beside the committed files and compared with them
#               before anything is overwritten. Paths are relative to the
#               course root.
# Author:       Chris Moreh
# Last updated: 2026-10-24
# Sourced by:   every numbered script in R/, and check_parity.R
##############################################################################

out_dir     <- Sys.getenv("COURSE_OUT_DIR", unset = "data")
work_dir    <- Sys.getenv("COURSE_WORK_DIR", unset = "_grid_work")
private_dir <- Sys.getenv("COURSE_PRIVATE_DIR", unset = "_planning_data")
# the single raw seam: the sixteen GESIS scientific-use files
raw_dir     <- Sys.getenv("EB_RAW_DIR", unset = file.path(private_dir, "raw"))

out_path  <- function(...) file.path(out_dir, ...)
work_path <- function(...) file.path(work_dir, ...)

# the respondent file is GESIS-derived microdata: it stays in the private folder
person_file <- file.path(private_dir, "person_level.rds")
