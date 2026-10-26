###############################################################################
# Title:        The exhibit models of the statistics and weights modules
# Purpose:      Fit the person-level and Bayesian exhibit models whose printed
#               summaries companion/stats-methods.qmd and
#               companion/weighting.qmd display verbatim, and capture each
#               summary in a text file of this folder. These are illustrative
#               single fits for teaching.
# Reads:        the respondent file, through _load_respondents.R (the
#               GESIS-licensed person-level rebuild of 416,698 respondents,
#               held only on the machine of the facilitator and never in a
#               repository)
#               cases/eu-frames/data/EUframes_cy.csv (the committed panel)
# Writes:       companion/_model-outputs/*.txt (captured model summaries:
#               aggregate statistical output only, no microdata)
#               companion/_model-outputs/cosmo_dist.csv (counts of the
#               cosmopolitan score)
#               _planning_data/brms_panel.rds, _planning_data/ordbetareg_sub.rds
#               (the fitted Bayesian objects, kept locally so that the
#               summaries can be printed again without sampling; gitignored,
#               since they embed data)
# Author:       Chris Moreh
# Last updated: 2026-10-26
###############################################################################

options(width = 88)
outdir <- "companion/_model-outputs"

source("companion/_model-outputs/_load_respondents.R")
person <- ind |>
  as.data.frame()  # r-style: keep – WeMix::mix() takes a plain data frame
euframes <- readr::read_csv("cases/eu-frames/data/EUframes_cy.csv", show_col_types = FALSE)

# --- 1. logistic GLM – mentions at least one cosmopolitan meaning ----------- #
m_logit <- glm(I(cosmo > 0) ~ unemp, family = binomial(), data = person)
writeLines(capture.output(summary(m_logit)), file.path(outdir, "glm_logit.txt"))
cat("logit  b_unemp =", round(coef(m_logit)["unemp"], 5), "\n")

# --- 2. design-weighted GLM via the survey package --------------------------- #
des <- survey::svydesign(ids = ~1, weights = ~w1, data = person)
m_svy <- survey::svyglm(cosmo ~ unemp, design = des)
writeLines(capture.output(summary(m_svy)), file.path(outdir, "svyglm.txt"))
cat("svyglm b_unemp =", round(coef(m_svy)["unemp"], 5), "\n")

# --- 3. two-level random intercepts – respondents in countries --------------- #
m_l2 <- lmerTest::lmer(cosmo ~ unemp + (1 | cntry), data = person)
writeLines(capture.output(summary(m_l2)), file.path(outdir, "lmer_2lvl.txt"))
cat("lmer2  b_unemp =", round(lme4::fixef(m_l2)["unemp"], 5), "\n")

# --- 4. three-level random intercepts – respondents in country-years in
#        countries (the structure used in the original author's (OA) published
#        model) --------------------------------------------------------------- #
m_l3 <- lmerTest::lmer(cosmo ~ unemp + (1 | cntry) + (1 | cntry:year), data = person)
writeLines(capture.output(summary(m_l3)), file.path(outdir, "lmer_3lvl.txt"))
cat("lmer3  b_unemp =", round(lme4::fixef(m_l3)["unemp"], 5), "\n")

# --- 5. random slopes – the unemployment effect varies by country ------------ #
m_rs <- tryCatch(
  lmerTest::lmer(cosmo ~ unemp + (1 + unemp | cntry) + (1 | cntry:year), data = person),
  error = function(e) e
)
if (inherits(m_rs, "error")) {
  writeLines(paste("random-slope fit failed:", conditionMessage(m_rs)), file.path(outdir, "lmer_slopes.txt"))
} else {
  writeLines(capture.output(summary(m_rs)), file.path(outdir, "lmer_slopes.txt"))
  cat("lmerRS b_unemp =", round(lme4::fixef(m_rs)["unemp"], 5), "\n")
}

# --- 6. weighted multilevel via WeMix (level-1 nation weight W1) ------------- #
person <- person |>
  datawizard::data_modify(w_lvl2 = 1)   # unit weight at the country level – weighting at level 1 only
m_wemix <- WeMix::mix(cosmo ~ unemp + (1 | cntry), data = person,
               weights = c("w1", "w_lvl2"))
writeLines(capture.output(summary(m_wemix)), file.path(outdir, "wemix_2lvl.txt"))
cat("wemix  b_unemp =", round(m_wemix$coef["unemp"], 5), "\n")

# --- 7. frequentist ordered beta regression on the full microdata ------------ #
m_tmb <- glmmTMB::glmmTMB(cosmo ~ unemp + (1 | cntry) + (1 | cntry:year),
                 family = glmmTMB::ordbeta(), data = person)
writeLines(capture.output(summary(m_tmb)), file.path(outdir, "glmmtmb_ordbeta.txt"))
cat("glmmTMB(ordbeta) b_unemp =", round(glmmTMB::fixef(m_tmb)$cond["unemp"], 5), "\n")

# --- 8. Bayesian hierarchical model on the committed aggregate panel --------- #
m_brm <- brms::brm(mcosmo ~ unemp + (1 | cntry) + (1 | year), data = euframes,
             backend = "cmdstanr", chains = 4, cores = 4, iter = 4000,
             seed = 2026, refresh = 0, silent = 2)
writeLines(capture.output(print(summary(m_brm), digits = 4)), file.path(outdir, "brms_panel.txt"))
writeLines(capture.output(print(brms::prior_summary(m_brm))), file.path(outdir, "brms_priors.txt"))
saveRDS(m_brm, "_planning_data/brms_panel.rds")
cat("brms   b_unemp =", round(brms::fixef(m_brm)["unemp", "Estimate"], 5), "\n")

# --- 9. Bayesian ordered beta regression (ordbetareg wraps brms) -------------
#     Fitted on a 20,000-respondent random subsample: the ordered-beta
#     likelihood on all 416,698 rows would sample for many hours, and the
#     exhibit's purpose is the shape of the output, not a production estimate.
set.seed(2026)
person_sub <- dplyr::slice_sample(person, n = 20000)
m_ord <- ordbetareg::ordbetareg(cosmo ~ unemp + (1 | cntry), data = person_sub,
                    true_bounds = c(0, 1), backend = "cmdstanr",
                    chains = 4, cores = 4, iter = 2000, seed = 2026,
                    refresh = 0, silent = 2)
writeLines(capture.output(print(summary(m_ord), digits = 4)), file.path(outdir, "ordbetareg_sub.txt"))
saveRDS(m_ord, "_planning_data/ordbetareg_sub.rds")
cat("ordbetareg b_unemp =",
    round(brms::fixef(m_ord)["unemp", "Estimate"], 5), "\n")

# --- 10. person-level outcome distribution (aggregate counts only) ----------- #
person |>
  dplyr::count(cosmo, name = "n_respondents") |>
  readr::write_csv(file.path(outdir, "cosmo_dist.csv"))

# --- provenance manifest ------------------------------------------------------ #
writeLines(c(
  paste("Fitted:", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste("R", getRversion()),
  paste("person-level rows:", nrow(person)),
  paste("ordbetareg subsample rows:", nrow(person_sub), "(set.seed 2026)"),
  paste("lme4", packageVersion("lme4"), "| lmerTest", packageVersion("lmerTest"),
        "| survey", packageVersion("survey"), "| WeMix", packageVersion("WeMix")),
  paste("glmmTMB", packageVersion("glmmTMB"), "| brms", packageVersion("brms"),
        "| cmdstanr", packageVersion("cmdstanr"), "| ordbetareg",
        packageVersion("ordbetareg"))
), file.path(outdir, "manifest.txt"))
