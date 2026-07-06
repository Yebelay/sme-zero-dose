# =============================================================================
# Descriptive statistics: zero-dose EDHS 2011/2016/2019, child + cluster level
# =============================================================================
suppressMessages({library(tidyverse)})

child   <- readRDS("processed/edhs_zero_dose_child_prewored_2026-07-02.rds")
cluster <- readRDS("processed/edhs_zero_dose_cluster_prewored_2026-07-02.rds")

dir.create("processed/tables", showWarnings = FALSE, recursive = TRUE)

wtd_mean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)

# ---- 1. Sample sizes and prevalence by survey round -------------------------
tab_round <- child %>%
  filter(!is.na(zero_dose)) %>%
  group_by(survey_year) %>%
  summarise(
    n_clusters      = n_distinct(cluster),
    n_children      = n(),
    zero_dose_unwtd = mean(zero_dose) * 100,
    zero_dose_wtd   = wtd_mean(zero_dose, wt) * 100,
    .groups = "drop"
  )

# ---- 2. Prevalence by residence, wealth quintile, mother's education --------
by_group <- function(data, group_var) {
  data %>%
    filter(!is.na(zero_dose), !is.na(.data[[group_var]])) %>%
    group_by(survey_year, .data[[group_var]]) %>%
    summarise(n_children = n(),
              zero_dose_wtd = wtd_mean(zero_dose, wt) * 100,
              .groups = "drop") %>%
    rename(group = 2) %>%
    mutate(group = as.character(group), variable = group_var, .before = 1)
}

tab_subgroup <- bind_rows(
  by_group(child, "residence"),
  by_group(child, "wealth_q"),
  by_group(child, "mother_educ"),
  by_group(child, "child_female")
)

# ---- 3. Prevalence by DHS-assigned region (ADM1NAME, attribute only) --------
tab_region <- cluster %>%
  filter(n_children > 0, !is.na(dhs_region)) %>%
  group_by(survey_year, dhs_region) %>%
  summarise(
    n_clusters  = n(),
    n_children  = sum(n_children),
    n_zero_dose = sum(n_zero_dose),
    prevalence  = n_zero_dose / n_children * 100,
    .groups = "drop"
  ) %>%
  arrange(dhs_region, survey_year)

# ---- 4. Cluster-level summary: geolocation coverage, urban/rural split ------
tab_cluster_summary <- cluster %>%
  group_by(survey_year) %>%
  summarise(
    n_clusters       = n(),
    n_urban          = sum(urban_rural == "urban", na.rm = TRUE),
    n_rural          = sum(urban_rural == "rural", na.rm = TRUE),
    median_children  = median(n_children),
    clusters_zero_n  = sum(n_children == 0),
    .groups = "drop"
  )

write_csv(tab_round,            "processed/tables/tab1_round_summary.csv")
write_csv(tab_subgroup,         "processed/tables/tab2_subgroup_prevalence.csv")
write_csv(tab_region,           "processed/tables/tab3_region_prevalence.csv")
write_csv(tab_cluster_summary,  "processed/tables/tab4_cluster_summary.csv")

cat("\n==== Table 1: sample size and zero-dose prevalence by round ====\n")
print(tab_round, n = Inf)

cat("\n==== Table 4: cluster geolocation / urban-rural summary ====\n")
print(tab_cluster_summary, n = Inf)

cat("\n==== Table 3: zero-dose prevalence by DHS region (2019) ====\n")
tab_region %>% filter(survey_year == 2019) %>% arrange(desc(prevalence)) %>% print(n = Inf)

cat("\n==== Table 2: zero-dose prevalence by subgroup (2019) ====\n")
tab_subgroup %>% filter(survey_year == 2019) %>% print(n = Inf)
