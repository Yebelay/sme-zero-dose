# =============================================================================
# Design-based DIRECT estimates of zero-dose prevalence: national and by DHS
# region, per survey round, using the actual PSU/strata/weight design.
#
# IMPORTANT: v021 (psu) and v023 (strata) are cluster-style codes that are
# REUSED across rounds (psu 5 in 2011 and psu 5 in 2019 are unrelated sample
# points from independent frames). Each round therefore needs its own design
# identifiers; survey_year is prefixed onto both so a single combined
# svydesign object cannot accidentally link PSUs across rounds. This is
# mathematically identical to running three separate per-round designs, but
# lets one svyby() call return all three rounds at once.
#
# NOTE ON DOMAIN: "region" here is the DHS ADM1NAME attribute carried on the
# GPS point file, standardized for spelling across rounds. It is NOT the
# woreda; direct estimates below are the finest domain identifiable without
# the still-outstanding admin-3 boundary file (see README_STATUS).
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

child <- readRDS("data/clean/edhs_zero_dose_child_prewored_2026-07-02.rds") %>%
  filter(!is.na(zero_dose)) %>%
  mutate(
    psu_id    = paste(survey_year, psu),
    strata_id = paste(survey_year, strata)
  )

dir.create("data/clean/tables", showWarnings = FALSE, recursive = TRUE)

des <- svydesign(ids = ~psu_id, strata = ~strata_id, weights = ~wt,
                  data = child, nest = TRUE)

# ---- 1. National direct estimate, by round ----------------------------------
national <- svyby(~zero_dose, ~survey_year, des, svymean, vartype = c("se", "ci"),
                   na.rm = TRUE) %>%
  as_tibble() %>%
  transmute(survey_year,
            prevalence_pct = round(zero_dose * 100, 1),
            se_pct         = round(se * 100, 2),
            ci_low         = round(ci_l * 100, 1),
            ci_high        = round(ci_u * 100, 1))

# ---- 2. Direct estimate by region, by round ----------------------------------
by_region <- svyby(~zero_dose, ~survey_year + dhs_region, des, svymean,
                    vartype = c("se", "ci"), na.rm = TRUE) %>%
  as_tibble() %>%
  transmute(survey_year, dhs_region,
            prevalence_pct = round(zero_dose * 100, 1),
            se_pct         = round(se * 100, 2),
            ci_low         = round(pmax(ci_l, 0) * 100, 1),
            ci_high        = round(pmin(ci_u, 1) * 100, 1)) %>%
  left_join(child %>% count(survey_year, dhs_region, name = "n_children"),
            by = c("survey_year", "dhs_region")) %>%
  left_join(child %>% distinct(survey_year, dhs_region, psu_id) %>%
              count(survey_year, dhs_region, name = "n_clusters"),
            by = c("survey_year", "dhs_region")) %>%
  arrange(dhs_region, survey_year)

write_csv(national,  "data/clean/tables/direct_est_national_by_round.csv")
write_csv(by_region, "data/clean/tables/direct_est_region_by_round.csv")

cat("==== National direct estimate, zero-dose prevalence, by round ====\n")
print(national)

cat("\n==== Regional direct estimate, 2019, sorted by prevalence ====\n")
by_region %>% filter(survey_year == 2019) %>% arrange(desc(prevalence_pct)) %>%
  select(dhs_region, n_clusters, n_children, prevalence_pct, se_pct, ci_low, ci_high) %>%
  print(n = Inf)

cat("\n==== Regional direct estimate, all rounds ====\n")
by_region %>% select(survey_year, dhs_region, n_clusters, n_children,
                      prevalence_pct, se_pct, ci_low, ci_high) %>%
  arrange(dhs_region, survey_year) %>% print(n = Inf)

# ---- 3. Flag regions with small effective samples (unstable CIs) -----------
cat("\n==== Region-rounds with fewer than 15 clusters (wide/unstable CIs) ====\n")
by_region %>% filter(n_clusters < 15) %>%
  select(survey_year, dhs_region, n_clusters, n_children, prevalence_pct, ci_low, ci_high) %>%
  print(n = Inf)
