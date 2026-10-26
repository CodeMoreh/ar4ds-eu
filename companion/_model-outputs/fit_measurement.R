###############################################################################
# Title:        The thirteen EU-meaning items: dimensionality and the scales
# Purpose:      Everything the measurement module displays about the framing
#               instrument, computed once on the licensed respondent file and
#               written as aggregate tables. Item prevalences; the tetrachoric
#               correlations among the thirteen items; the eigenvalues of that
#               matrix; exploratory two- and four-factor solutions (minres,
#               oblimin) set against the published assignment of items to
#               scales; a confirmatory comparison of the published four-scale
#               model, the data-driven four-factor model and a two-factor
#               valence model, fitted on the binary items with the
#               diagonally weighted least squares estimator; and the
#               reliability of each scale, so that a two-item scale can be
#               read for what it is. The committed correlation matrix is
#               enough for a reader to repeat the exploratory analyses.
#
# Reads:        the respondent file, through _load_respondents.R (GESIS licensed, local only)
# Writes:       companion/_model-outputs/items_prevalence.csv
#               companion/_model-outputs/items_tetrachoric.csv
#               companion/_model-outputs/efa_eigen.csv
#               companion/_model-outputs/efa_loadings.csv
#               companion/_model-outputs/efa_factor_corr.csv
#               companion/_model-outputs/cfa_fit.csv
#               companion/_model-outputs/cfa_loadings.csv
#               companion/_model-outputs/scale_reliability.csv
#               All aggregate; nothing at the respondent level leaves the
#               directory.
# Author:       Chris Moreh
# Last updated: 2026-09-07
###############################################################################
out <- "companion/_model-outputs"

source("companion/_model-outputs/_load_respondents.R")
p <- ind

# the fourteen response options of the battery, the Euro (position 8) excluded
# by the original paper; positions follow the questionnaire
items <- tibble::tribble(
  ~var,  ~item,                         ~scale,
  "it1",  "Peace",                       "cosmopolitan",
  "it2",  "Economic prosperity",         "utilitarian",
  "it3",  "Democracy",                   "cosmopolitan",
  "it4",  "Social protection",           "utilitarian",
  "it5",  "Freedom to travel",           "cosmopolitan",
  "it6",  "Cultural diversity",          "cosmopolitan",
  "it7",  "Stronger say in the world",   "cosmopolitan",
  "it9",  "Unemployment",                "communitarian",
  "it10", "Bureaucracy",                 "libertarian",
  "it11", "Waste of money",              "libertarian",
  "it12", "Loss of cultural identity",   "communitarian",
  "it13", "More crime",                  "communitarian",
  "it14", "Not enough border control",   "communitarian"
) |>
  datawizard::data_modify(valence = dplyr::if_else(scale %in% c("cosmopolitan", "utilitarian"), "positive", "negative"))

X <- as.matrix(p[, items$var])
storage.mode(X) <- "numeric"
n <- nrow(X)

## ---- prevalence -------------------------------------------------------------------

prevalence <- items |>
  datawizard::data_modify(share_mentioning = colMeans(X)[var],  # r-style: keep – the 0/1 items as a matrix
         n_respondents = n)
datawizard::data_write(prevalence, file.path(out, "items_prevalence.csv"), verbose = FALSE)

## ---- tetrachoric correlations and their eigenvalues ------------------------------

tet <- psych::tetrachoric(X, correct = 0.5)$rho
dimnames(tet) <- list(items$item, items$item)
tet_long <- as.data.frame(tet) |>  # r-style: keep – the matrix as a frame, before it is made long
  datawizard::data_modify(item = rownames(tet)) |>
  tidyr::pivot_longer(-item, names_to = "with", values_to = "tetrachoric")
datawizard::data_write(tet_long, file.path(out, "items_tetrachoric.csv"), verbose = FALSE)

ev <- eigen(tet)$values
set.seed(20260907)
pa <- psych::fa.parallel(tet, n.obs = n, fm = "minres", fa = "fa", plot = FALSE, n.iter = 20)
datawizard::data_write(tibble::tibble(factor = seq_along(ev), eigenvalue = ev,
                              parallel_mean = pa$fa.sim, n_respondents = n),
                       file.path(out, "efa_eigen.csv"), verbose = FALSE)

## ---- exploratory solutions ----------------------------------------------------------

efa <- purrr::map(c(2, 4), \(k) {
  f <- psych::fa(tet, nfactors = k, n.obs = n, fm = "minres", rotate = "oblimin")
  L <- unclass(f$loadings)
  list(loadings = tibble::tibble(solution = paste0(k, "-factor"), item = items$item, scale = items$scale,
                         valence = items$valence, communality = f$communality) |>
         dplyr::bind_cols(tibble::as_tibble(L) |> setNames(paste0("F", seq_len(k)))),
       phi = tibble::as_tibble(f$Phi, .name_repair = "minimal") |>
         setNames(paste0("F", seq_len(k))) |>
         dplyr::mutate(solution = paste0(k, "-factor"), factor = paste0("F", seq_len(k)), .before = 1),
       varacc = 100 * sum(f$Vaccounted["Proportion Var", ]))
})
datawizard::data_write(dplyr::bind_rows(purrr::map(efa, "loadings")), file.path(out, "efa_loadings.csv"), verbose = FALSE)
datawizard::data_write(dplyr::bind_rows(purrr::map(efa, "phi")), file.path(out, "efa_factor_corr.csv"), verbose = FALSE)

## ---- confirmatory comparison ----------------------------------------------------------

# the published assignment, the data-driven assignment read off the
# four-factor solution, and a two-factor valence model
models <- list(
  published = "
    cosmopolitan  =~ it1 + it3 + it5 + it6 + it7
    utilitarian   =~ it2 + it4
    communitarian =~ it9 + it12 + it13 + it14
    libertarian   =~ it10 + it11",
  data_driven = "
    benefits      =~ it1 + it2 + it3 + it4 + it7
    mobility      =~ it5 + it6 + it7
    communitarian =~ it9 + it12 + it13 + it14
    libertarian   =~ it10 + it11",
  valence = "
    positive =~ it1 + it2 + it3 + it4 + it5 + it6 + it7
    negative =~ it9 + it10 + it11 + it12 + it13 + it14"
)
d_items <- as.data.frame(X)  # r-style: keep – lavaan reads the items from a plain data frame
fits <- purrr::imap(models, \(m, nm) {
  fit <- lavaan::cfa(m, data = d_items, ordered = names(d_items), estimator = "WLSMV", std.lv = TRUE)
  fm <- lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                            "rmsea.scaled", "srmr"))
  list(fit = tibble::tibble(model = nm, chisq = fm[["chisq.scaled"]], df = fm[["df.scaled"]],
                    cfi = fm[["cfi.scaled"]], tli = fm[["tli.scaled"]],
                    rmsea = fm[["rmsea.scaled"]], srmr = fm[["srmr"]], n_respondents = n),
       loadings = lavaan::standardizedSolution(fit) |>
         dplyr::filter(op == "=~") |>
         dplyr::transmute(model = nm, factor = lhs, var = rhs, loading = est.std) |>
         dplyr::left_join(items |> dplyr::select(var, item), by = "var"),
       corr = lavaan::standardizedSolution(fit) |>
         dplyr::filter(op == "~~", lhs != rhs) |>
         dplyr::transmute(model = nm, factor = lhs, with = rhs, correlation = est.std))
})
datawizard::data_write(dplyr::bind_rows(purrr::map(fits, "fit")), file.path(out, "cfa_fit.csv"), verbose = FALSE)
datawizard::data_write(dplyr::bind_rows(c(purrr::map(fits, "loadings"), purrr::map(fits, "corr"))) |>
                         datawizard::data_select(select = c("model", "factor", "var", "item", "loading", "with", "correlation")),
                       file.path(out, "cfa_loadings.csv"), verbose = FALSE)

## ---- reliability of each scale, as the items stand ------------------------------------

scales <- list(cosmopolitan = c("it1", "it3", "it5", "it6", "it7"), utilitarian = c("it2", "it4"),
               communitarian = c("it9", "it12", "it13", "it14"), libertarian = c("it10", "it11"),
               positive = c("it1", "it2", "it3", "it4", "it5", "it6", "it7"),
               negative = c("it9", "it10", "it11", "it12", "it13", "it14"))
reliability <- purrr::imap(scales, \(v, nm) {
  r <- tet[items$item[match(v, items$var)], items$item[match(v, items$var)]]
  # ordinal alpha: Cronbach's alpha on the tetrachoric matrix (Zumbo,
  # Gadermann and Zeisser 2007, Journal of Modern Applied Statistical Methods),
  # from psych::alpha(), which reads a correlation matrix as one
  tibble::tibble(scale = nm, items = length(v), mean_tetrachoric = mean(r[lower.tri(r)]),
                 ordinal_alpha = psych::alpha(r, warnings = FALSE)$total$std.alpha,
                 mean_prevalence = mean(colMeans(X[, v])))  # r-style: keep – the 0/1 items as a matrix
}) |> purrr::list_rbind()
datawizard::data_write(reliability, file.path(out, "scale_reliability.csv"), verbose = FALSE)

## ---- computed facts reported for verification ------------------------------------------

# the respondents, the eigenvalues, the number of factors that the parallel
# analysis suggests (a formality at this n), and the variance that the two- and
# four-factor solutions account for
print(n)
print(round(ev, 2))
print(pa$nfact)
print(round(purrr::map_dbl(efa, "varacc"), 1))
dplyr::bind_rows(purrr::map(fits, "fit")) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3))) |>
  print()
reliability |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3))) |>
  print()
