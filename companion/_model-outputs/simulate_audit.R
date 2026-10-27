################################################################################
# Title:        Simulated audits of the housing-audit case
# Purpose:      The simulations of the simulation module, declared and diagnosed
#               with DeclareDesign (Blair, Cooper, Coppock and Humphreys 2019,
#               American Political Science Review): an audit whose recipe plants
#               the gap in named greetings that the original study found, and
#               the comparisons of the case diagnosed against it over many
#               draws. Three sets of designs: the recovery of the planted gap
#               when the name changes only the greeting; the comparison among
#               replies when the name also changes who replies, under three
#               assumptions about the officials who reply only to white names;
#               and the power of the comparison among replies at the size of the
#               original study and of the replication, for gaps from 0 to 25
#               points, beside the analytic power of display_power.csv.
#               The simulation module declares the first design on the page with
#               the same code and reads the diagnoses from the files below.
# Reads:        cases/housing/data/housing_audit.csv (the rates that the recipe
#               plants)
# Writes:       companion/_model-outputs/sim_audit_diagnosis.csv
#               companion/_model-outputs/sim_audit_draws.csv
# Author:       Claude for CM
# Last updated: 2026-10-27
################################################################################

set.seed(20261027)

# The recipe takes its numbers from the delivered emails to white and Hispanic
# names: how many there were, how often each group drew a reply, and how often
# a coded reply opened with the name of the sender
audit <- readr::read_csv("cases/housing/data/housing_audit.csv", show_col_types = FALSE) |>
  datawizard::data_filter(group %in% c("white", "hispanic"), !is.na(reply))
n_delivered <- nrow(audit)
n_hispanic <- sum(audit$group == "hispanic")
reply_white <- mean(audit$reply[audit$group == "white"])
reply_hisp <- mean(audit$reply[audit$group == "hispanic"])
reply_all <- mean(audit$reply)
coded <- datawizard::data_filter(audit, reply == 1, !is.na(named))
greet_white <- mean(coded$named[coded$group == "white"])
greet_hisp <- mean(coded$named[coded$group == "hispanic"])

# 1. Recovery. Every authority would reply or not whatever the name (v), and
# among the replies the Hispanic name lowers the chance of a named greeting from
# greet_white to greet_hisp (u: the same official greets by name under the
# Hispanic name only if it would under the white name). Z = 1 is a Hispanic
# name, assigned to n_hispanic emails by complete random assignment as in the
# experiment; HC2 errors, as the curve of the case uses for the linear model.
recovery <-
  DeclareDesign::declare_model(
    N = n_delivered,
    v = runif(N), u = runif(N),
    R = as.integer(v < reply_all),
    G_Z_0 = as.integer(u < greet_white), G_Z_1 = as.integer(u < greet_hisp)
  ) +
  DeclareDesign::declare_inquiry(gap = mean(G_Z_1[R == 1] - G_Z_0[R == 1])) +
  DeclareDesign::declare_assignment(Z = randomizr::complete_ra(N, m = n_hispanic)) +
  DeclareDesign::declare_measurement(G = fabricatr::reveal_outcomes(G ~ Z)) +
  DeclareDesign::declare_estimator(G ~ Z, .method = estimatr::lm_robust, se_type = "HC2",
                                   subset = R == 1, inquiry = "gap", term = "Z",
                                   label = "among replies")

recovery_dx <- DeclareDesign::diagnose_design(recovery, sims = 2000, bootstrap_sims = FALSE)

# 2. Conditioning on a reply. In this recipe the Hispanic name also lowers the
# chance of a reply from reply_white to reply_hisp: the authorities with v below reply_hisp
# reply to either name, and those between reply_hisp and reply_white reply only
# to a white name. How these marginal repliers greet a white name is not in the
# data, so the recipe sets it three ways: never by name, as often as the other
# officials, always by name. Two questions: the effect of the name on the
# greeting among the authorities that reply to either name, and the effect on
# receiving a named greeting at all, among every delivered email (an email
# without a reply counting as one without a named greeting).
greet_marginal <- greet_white
reply_design <-
  DeclareDesign::declare_model(
    N = n_delivered,
    v = runif(N), u = runif(N),
    always = v < reply_hisp,
    R_Z_0 = as.integer(v < reply_white), R_Z_1 = as.integer(v < reply_hisp),
    G_Z_0 = dplyr::if_else(always, as.integer(u < greet_white), as.integer(u < greet_marginal)),
    G_Z_1 = as.integer(u < greet_hisp),
    A_Z_0 = R_Z_0 * G_Z_0, A_Z_1 = R_Z_1 * G_Z_1
  ) +
  DeclareDesign::declare_inquiry(gap_always = mean(G_Z_1[always] - G_Z_0[always]),
                                 gap_all = mean(A_Z_1 - A_Z_0)) +
  DeclareDesign::declare_assignment(Z = randomizr::complete_ra(N, m = n_hispanic)) +
  DeclareDesign::declare_measurement(R = fabricatr::reveal_outcomes(R ~ Z),
                                     G = fabricatr::reveal_outcomes(G ~ Z),
                                     A = fabricatr::reveal_outcomes(A ~ Z)) +
  DeclareDesign::declare_estimator(G ~ Z, .method = estimatr::lm_robust, se_type = "HC2",
                                   subset = R == 1, inquiry = "gap_always", term = "Z",
                                   label = "among replies") +
  DeclareDesign::declare_estimator(A ~ Z, .method = estimatr::lm_robust, se_type = "HC2",
                                   inquiry = "gap_all", term = "Z", label = "all delivered")

reply_dx <- DeclareDesign::redesign(reply_design, greet_marginal = c(0, greet_white, 1)) |>
  DeclareDesign::diagnose_design(sims = 2000, bootstrap_sims = FALSE)

# 3. Power. The comparison among replies with fixed numbers of replies, as the
# analytic power of the experiments module assumes (stats::power.prop.test()
# in R/housing/05_displays.R): 181 replies a group at the size of the original
# comparison and 369 at that of the replication, a white rate of 61%, and
# gaps of 0 to 25 points. At a gap of 0 the share of significant draws is the
# rate of false positives.
power_white <- 0.61
gap <- 0.10
n_per_group <- 181
power_design <-
  DeclareDesign::declare_model(
    N = 2 * n_per_group,
    u = runif(N),
    G_Z_0 = as.integer(u < power_white), G_Z_1 = as.integer(u < power_white - gap)
  ) +
  DeclareDesign::declare_inquiry(gap = mean(G_Z_1 - G_Z_0)) +
  DeclareDesign::declare_assignment(Z = randomizr::complete_ra(N, m = n_per_group)) +
  DeclareDesign::declare_measurement(G = fabricatr::reveal_outcomes(G ~ Z)) +
  DeclareDesign::declare_estimator(G ~ Z, .method = estimatr::lm_robust, se_type = "HC2",
                                   inquiry = "gap", term = "Z", label = "among replies")

power_dx <- DeclareDesign::redesign(power_design, n_per_group = c(181, 369),
                                    gap = c(0, 0.05, 0.10, 0.15, 0.20, 0.25)) |>
  DeclareDesign::diagnose_design(sims = 1000, bootstrap_sims = FALSE)

# The diagnoses, one row per design and estimator, with the parameters of each
# design and, from the draws themselves, the mean of the estimates that reached
# p < 0.05 (the significant draws of a design whose power is modest overstate
# its gap)
significant_means <- dplyr::bind_rows(recovery_dx$simulations_df, reply_dx$simulations_df,
                                      power_dx$simulations_df) |>
  dplyr::summarise(mean_estimate_significant = mean(estimate[p.value < 0.05]),
                   .by = c(design, estimator))

diagnosis <- dplyr::bind_rows(
  recovery_dx$diagnosands_df |> dplyr::mutate(set = "recovery"),
  reply_dx$diagnosands_df |> dplyr::mutate(set = "reply"),
  power_dx$diagnosands_df |> dplyr::mutate(set = "power")
) |>
  dplyr::left_join(significant_means, by = c("design", "estimator")) |>
  dplyr::mutate(design = paste(set, design, sep = "_")) |>
  dplyr::select(set, design, dplyr::any_of(c("greet_marginal", "n_per_group", "gap")),
                inquiry, estimator, n_sims, mean_estimand, mean_estimate, bias, sd_estimate,
                rmse, power, coverage, mean_estimate_significant) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signif(x, 6)))

# The estimates of the recovery design, draw by draw, for the histogram of the
# module
draws <- recovery_dx$simulations_df |>
  dplyr::transmute(sim_ID, estimand, estimate = signif(estimate, 6), std.error = signif(std.error, 6),
                   p.value = signif(p.value, 6), conf.low = signif(conf.low, 6),
                   conf.high = signif(conf.high, 6))

# readr writes the bytes of the committed files; datawizard::data_write() does not
readr::write_csv(diagnosis, "companion/_model-outputs/sim_audit_diagnosis.csv", na = "")
readr::write_csv(draws, "companion/_model-outputs/sim_audit_draws.csv", na = "")
