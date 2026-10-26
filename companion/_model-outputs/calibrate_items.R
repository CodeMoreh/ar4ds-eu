###############################################################################
# Title:        Item structure conditional on the covariates
# Purpose:      Give the framing instrument a mean structure. The first version
#               of this calibration estimated the correlations among items and
#               covariates and stopped there, which leaves the items
#               conditionally independent of anything not in that matrix. That
#               is why the twin's occupation contrasts failed: occupation
#               beyond the unemployed indicator was drawn from a model that
#               knew about demographics and nothing about framing, so a
#               regression containing occupation AND education split the
#               association between them differently from the real data.
#
#               What is estimated here is a multivariate probit. Each of the
#               thirteen items has a latent propensity that is linear in the
#               covariates, absorbs a country-year effect, and carries a
#               residual correlated with the other twelve. Fitting that gives
#               the coefficient block; the residual correlation follows by
#               subtraction, because the total latent correlation between two
#               items is what their shared covariates induce plus what is left.
#
# Reads:        the respondent file, through _load_respondents.R (licensed, local only)
#               companion/_model-outputs/joint_corr.csv
# Writes:       companion/_model-outputs/item_probit_coef.csv
#               companion/_model-outputs/item_residual_corr.csv
#               Coefficient and correlation tables: aggregate, and committed.
# Author:       Chris Moreh
# Last updated: 2026-07-30
###############################################################################
source("companion/_model-outputs/_load_respondents.R")
items <- paste0("it", c(1:7, 9:14))

# Left-right is missing for four of the sixteen waves. Dropping those
# respondents would throw away a quarter of the sample to estimate one
# coefficient, so it enters centred with a missing indicator beside it: the
# indicator carries whatever those waves differ by, and the slope is estimated
# where the variable exists.
RILE_MEAN <- mean(ind$rile, na.rm = TRUE)
dat <- ind |>
  datawizard::data_modify(cy = paste(cntry, year, sep = "_"),
         rile_c = dplyr::if_else(is.na(rile), 0, rile - RILE_MEAN),
         rile_missing = as.integer(is.na(rile)),
         age_c = age - mean(age, na.rm = TRUE),
         age_c2 = (age - mean(age, na.rm = TRUE))^2,
         edu_c = edu - mean(edu, na.rm = TRUE),
         edu3 = factor(dplyr::case_when(edu < 16 ~ "low", edu <= 19 ~ "mid",
                                 TRUE ~ "high"),
                       levels = c("low", "mid", "high")),
         ses   = factor(ses, levels = c("manual", "self_employed", "manager",
                                        "white_collar", "house_person",
                                        "unemployed", "retired", "student")),
         urban = factor(urban, levels = c("large_town", "rural", "small_town"))) |>
  datawizard::data_filter(!is.na(ses), !is.na(urban), !is.na(age_c), !is.na(edu_c),
         !is.na(edu3))

# Squared age is a column rather than an inline I(): fixest names an inline
# term I(I(age_c^2)), which then has to be matched character for character by
# anything reading the coefficient file back.
# Education enters twice, as years and as the three published bands. Years
# alone is a straight line through a relationship the case reads in bands, and
# a straight line fitted to a curve reproduces about half of each band
# contrast - which is exactly what the twin did before this term was added.
# Together they are a broken-stick: the bands carry the jumps, the linear term
# the gradient inside each.
RHS <- "female + age_c + age_c2 + edu_c + edu3 + ses + urban + rile_c + rile_missing"

# The country-year effect is absorbed rather than estimated: the twin solves
# its own cell shifts against the public panel, so what is needed here is the
# WITHIN-cell covariate structure. Leaving cells in the linear predictor would
# hand the generator two sets of cell effects fighting each other.
# one probit per item, with fixest (Bergé, Butts and McDermott 2026,
# arXiv:2601.21749)
fits <- purrr::map(purrr::set_names(items), \(v) {
  f <- as.formula(paste(v, "~", RHS, "| cy"))
  fixest::feglm(f, data = dat, family = binomial("probit"))
})

coefs <- purrr::imap(fits, \(m, v) tibble::tibble(item = v, term = names(coef(m)),
                                   estimate = as.numeric(coef(m)))) |>
  purrr::list_rbind()
readr::write_csv(coefs |> datawizard::data_modify(estimate = signif(estimate, 6)),
                 "companion/_model-outputs/item_probit_coef.csv")

# --- residual correlation by subtraction ------------------------------------
# r-style: PENDING – the residual correlations of a multivariate probit, by
# subtraction from the latent correlations; a joint fit of the thirteen items
# (mvProbit, bayesm) would give other correlations, and so another twin than
# the committed one, whose md5 the course fixes
# On the standardised latent scale the total correlation between two items is
#   rho_jk = [ cov(eta_j, eta_k) + rho_eps_jk ] / sqrt((var eta_j + 1)(var eta_k + 1))
# because a probit fixes the residual variance at 1. The linear predictors are
# computed on the real covariate distribution, so cov(eta) is a fact about the
# data rather than an assumption, and the residual correlation is what is left
# once it is removed.
eta <- purrr::map_dfc(fits, \(m) as.numeric(predict(m, type = "link")))
names(eta) <- items
V <- cov(eta)
v_eta <- diag(V)

corr <- readr::read_csv("companion/_model-outputs/joint_corr.csv", show_col_types = FALSE)
lat <- function(a, b) {
  if (a == b) return(1)
  r <- corr$r[(corr$var1 == a & corr$var2 == b) | (corr$var1 == b & corr$var2 == a)]
  if (length(r) == 0) NA_real_ else r[1]
}
Rt <- outer(items, items, Vectorize(lat))
dimnames(Rt) <- list(items, items)

Reps <- Rt * sqrt(outer(v_eta + 1, v_eta + 1)) - V
diag(Reps) <- 1
Reps <- cov2cor(Reps)

# The subtraction can push the matrix off the space of correlation matrices,
# and a Cholesky in the generator would then fail with nothing to read. The
# nearest positive-definite version is used instead, and how far it had to move
# is reported: a large change would mean the decomposition is not holding and
# the assumption behind it needs revisiting.
# The nearest correlation matrix is that of Higham (2002, IMA Journal of
# Numerical Analysis), from Matrix::nearPD(); on the committed calibration it is
# not needed, since the smallest eigenvalue is well above the threshold.
if (min(eigen(Reps, symmetric = TRUE, only.values = TRUE)$values) < 1e-6) {
  fixed <- as.matrix(Matrix::nearPD(Reps, corr = TRUE)$mat)
  message("residual matrix moved to the nearest correlation matrix; largest change ",
          signif(max(abs(fixed - Reps)), 3))
  Reps <- fixed
}

res_long <- as.data.frame(as.table(Reps)) |>  # r-style: keep – the matrix in long form, in the order of the committed file
  setNames(c("item1", "item2", "r")) |>
  datawizard::data_filter(as.character(item1) < as.character(item2)) |>
  datawizard::data_modify(r = signif(r, 6))
readr::write_csv(res_long, "companion/_model-outputs/item_residual_corr.csv")

# How much of the item correlation the covariates explain: the median total
# and residual correlation between two items, and the range of the variance of
# the linear predictor
tibble::tibble(median_total_r = signif(median(Rt[lower.tri(Rt)]), 3),
               median_residual_r = signif(median(Reps[lower.tri(Reps)]), 3),
               var_eta_min = signif(min(v_eta), 3), var_eta_max = signif(max(v_eta), 3)) |>
  print()

# The largest covariate effects on any item
coefs |>
  datawizard::data_filter(!grepl("rile_missing", term)) |>
  dplyr::slice_max(abs(estimate), n = 10) |>
  datawizard::data_modify(estimate = signif(estimate, 3)) |>
  print()
