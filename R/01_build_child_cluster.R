# =============================================================================
# Ethiopia zero-dose SAE: Build analysis-ready dataset from EDHS 2011, 2016, 2019
# STAGE 1 of 2: children's recode + GPS + geographic covariates -> child and
# cluster (EA) level files. The admin-3 (woreda) spatial join is STAGE 2,
# in 02_woreda_join.R, and requires an admin-3 boundary shapefile that has not
# yet been supplied (see README_STATUS.md).
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(haven)
  library(sf)
  library(labelled)
})

sf_use_s2(FALSE)
raw_dir <- "data/raw"
out_dir <- "processed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Children's recode: read, tag round, stack ---------------------------
# 2011 (ETKR61FL) has no b19 (current age in months); age is recovered from
# v008 - b3 (CMC interview date minus CMC birth date) for that round only.
kr_vars <- c("v001","v002","v003","v005","v021","v022","v023","v024","v025",
             "v008","b3","b5","b19","b4","bord","v106","v130","v190",
             "h0","h2","h3","h4","h5","h7","h9","h10")

kr_2011 <- read_dta(file.path(raw_dir, "ETKR61FL.DTA"), col_select = any_of(kr_vars)) %>%
  mutate(survey_year = 2011L)
kr_2016 <- read_dta(file.path(raw_dir, "ETKR71FL.DTA"), col_select = any_of(kr_vars)) %>%
  mutate(survey_year = 2016L)
kr_2019 <- read_dta(file.path(raw_dir, "ETKR81FL.DTA"), col_select = any_of(kr_vars)) %>%
  mutate(survey_year = 2019L)

kr_raw <- bind_rows(kr_2011, kr_2016, kr_2019) %>%
  zap_labels() %>%
  mutate(across(-survey_year, as.numeric))

# ---- 2. Derive outcome and predictors; keep living children 12-23 months ----
# DHS h3 coding: 0 = no, 1/2/3 = vaccinated (card date / recall / marked),
# 8 = don't know. "Zero dose" = no DTP1/Penta1. h3 == 8 is set to NA (excluded),
# consistent with standard zero-dose indicator practice; see NOTES at bottom.
child_all <- kr_raw %>%
  mutate(
    cluster      = as.integer(v001),
    household    = as.integer(v002),
    wt           = v005 / 1e6,
    psu          = v021,
    strata       = v023,
    age_months   = as.integer(coalesce(b19, v008 - b3)),
    alive        = b5 == 1,
    dtp1_recd    = case_when(
      h3 %in% c(1, 2, 3) ~ 1L,
      h3 == 0            ~ 0L,
      TRUE               ~ NA_integer_),
    zero_dose    = case_when(
      dtp1_recd == 1L ~ 0L,
      dtp1_recd == 0L ~ 1L,
      TRUE            ~ NA_integer_),
    child_female = as.integer(b4 == 2),
    birth_order  = as.integer(bord),
    mother_educ  = factor(v106, levels = 0:3,
                           labels = c("none", "primary", "secondary", "higher")),
    wealth_q     = factor(v190, levels = 1:5,
                           labels = c("poorest", "poorer", "middle", "richer", "richest")),
    residence    = factor(v025, levels = c(1, 2), labels = c("urban", "rural")),
    region_code  = as.integer(v024)
  ) %>%
  filter(alive, age_months >= 12, age_months <= 23) %>%
  select(survey_year, cluster, household, wt, psu, strata,
         age_months, child_female, birth_order,
         mother_educ, wealth_q, residence, region_code,
         dtp1_recd, zero_dose)

message("Zero-dose cohort (living, 12-23 mo), rows: ", nrow(child_all))

# ---- 3. GPS cluster points ---------------------------------------------------
# One point per EA (cluster). DHS displaces coordinates for confidentiality
# (urban up to 2 km, rural up to 5 km, 1% of rural up to 10 km) and stores
# clusters it could not place at all as (0,0); those are dropped.
# DHS spells/formats region names differently across rounds (e.g. "Oromiya
# Region" in 2011/2016 vs "Oromia" in 2019; "Gambella" vs "Gambela"; a
# trailing " Region" suffix in 2011/2016 only). Standardize to one canonical
# name per region so pooled tables/charts don't silently split or drop rows.
standardize_region <- function(x) {
  x <- str_trim(x)
  x <- str_remove(x, "\\s*Region$")
  recode(x,
         "Gambella"           = "Gambela",
         "Oromiya"            = "Oromia",
         "Benishangul Gumuz"  = "Benishangul-Gumuz",
         .default = x)
}

read_gps <- function(path, year) {
  read_sf(path) %>%
    st_drop_geometry() %>%
    transmute(
      survey_year = year,
      cluster     = as.integer(DHSCLUST),
      latnum      = as.numeric(LATNUM),
      longnum     = as.numeric(LONGNUM),
      urban_rural = if_else(URBAN_RURA == "U", "urban", "rural"),
      dhs_region  = standardize_region(as.character(ADM1NAME)))
}

gps_tab <- bind_rows(
  read_gps(file.path(raw_dir, "ETGE61FL", "ETGE61FL.shp"), 2011L),
  read_gps(file.path(raw_dir, "ETGE71FL", "ETGE71FL.shp"), 2016L),
  read_gps(file.path(raw_dir, "ETGE81FL", "ETGE81FL.shp"), 2019L)
) %>%
  filter(!(latnum == 0 & longnum == 0))

message("GPS clusters, all rounds (unplaced dropped): ", nrow(gps_tab))

# ---- 4. Geographic covariates: pick vintage nearest the survey year ---------
gc_2011 <- read_csv(file.path(raw_dir, "ETGC62FL.csv"), show_col_types = FALSE) %>%
  transmute(survey_year = 2011L, cluster = as.integer(DHSCLUST),
            travel_times           = Travel_Times_2015,
            all_population_count   = All_Population_Count_2010,
            u5_population          = U5_Population_2010,
            un_population_density  = UN_Population_Density_2010,
            nightlights_composite  = Nightlights_Composite,
            aridity                = Aridity_2010,
            evi                    = Enhanced_Vegetation_Index_2010,
            itn_coverage           = ITN_Coverage_2010,
            malaria_incidence      = Malaria_Incidence_2010,
            rainfall               = Rainfall_2010,
            mean_temperature       = Mean_Temperature_2010,
            global_human_footprint = Global_Human_Footprint,
            growing_season_length  = Growing_Season_Length,
            livestock_cattle       = Livestock_Cattle,
            drought_episodes       = Drought_Episodes,
            irrigation             = Irrigation)

gc_2016 <- read_csv(file.path(raw_dir, "ETGC72FL.csv"), show_col_types = FALSE) %>%
  transmute(survey_year = 2016L, cluster = as.integer(DHSCLUST),
            travel_times           = Travel_Times_2015,
            all_population_count   = All_Population_Count_2015,
            u5_population          = U5_Population_2015,
            un_population_density  = UN_Population_Density_2015,
            nightlights_composite  = Nightlights_Composite,
            aridity                = Aridity_2015,
            evi                    = Enhanced_Vegetation_Index_2015,
            itn_coverage           = ITN_Coverage_2015,
            malaria_incidence      = Malaria_Incidence_2015,
            rainfall               = Rainfall_2015,
            mean_temperature       = Mean_Temperature_2015,
            global_human_footprint = Global_Human_Footprint,
            growing_season_length  = Growing_Season_Length,
            livestock_cattle       = Livestock_Cattle,
            drought_episodes       = Drought_Episodes,
            irrigation             = Irrigation)

gc_2019 <- read_csv(file.path(raw_dir, "ETGC81FL.csv"), show_col_types = FALSE) %>%
  transmute(survey_year = 2019L, cluster = as.integer(DHSCLUST),
            travel_times           = Travel_Times,
            all_population_count   = All_Population_Count_2020,
            u5_population          = U5_Population_2020,
            un_population_density  = UN_Population_Density_2020,
            nightlights_composite  = Nightlights_Composite,
            aridity                = Aridity_2020,
            evi                    = Enhanced_Vegetation_Index_2020,
            itn_coverage           = ITN_Coverage_2020,
            malaria_incidence      = Malaria_Incidence_2020,
            rainfall               = Rainfall_2020,
            mean_temperature       = Mean_Temperature_2020,
            global_human_footprint = Global_Human_Footprint,
            growing_season_length  = Growing_Season_Length,
            livestock_cattle       = Livestock_Cattle,
            drought_episodes       = Drought_Episodes,
            irrigation             = Irrigation,
            elevation              = Elevation)

gc_all <- bind_rows(gc_2011, gc_2016, gc_2019)

covariate_cols <- c("travel_times","all_population_count","u5_population",
                     "un_population_density","nightlights_composite","aridity",
                     "evi","itn_coverage","malaria_incidence","rainfall",
                     "mean_temperature","global_human_footprint",
                     "growing_season_length","livestock_cattle",
                     "drought_episodes","irrigation","elevation")

# ---- 5. Merge child + GPS + covariates (NO woreda yet) ----------------------
child_merged <- child_all %>%
  left_join(gps_tab, by = c("survey_year", "cluster")) %>%
  left_join(gc_all,  by = c("survey_year", "cluster")) %>%
  filter(!is.na(latnum), !is.na(longnum))

message("Pooled child rows (12-23 mo, geolocated, pre-woreda): ", nrow(child_merged))

# ---- 6. Aggregate to cluster (EA) level -------------------------------------
cluster_level <- child_merged %>%
  group_by(survey_year, cluster) %>%
  summarise(
    n_children  = sum(!is.na(zero_dose)),
    n_zero_dose = sum(zero_dose, na.rm = TRUE),
    prevalence  = if_else(n_children > 0, n_zero_dose / n_children, NA_real_),
    latnum      = first(latnum),
    longnum     = first(longnum),
    urban_rural = first(urban_rural),
    dhs_region  = first(dhs_region),
    across(all_of(covariate_cols), first),
    .groups = "drop"
  )

message("Cluster (EA) rows, pre-woreda: ", nrow(cluster_level))

# ---- 7. Save stage-1 outputs -------------------------------------------------
write_csv(child_merged,  file.path(out_dir, "edhs_zero_dose_child_prewored_2026-07-02.csv"))
write_csv(cluster_level, file.path(out_dir, "edhs_zero_dose_cluster_prewored_2026-07-02.csv"))
saveRDS(child_merged,    file.path(out_dir, "edhs_zero_dose_child_prewored_2026-07-02.rds"))
saveRDS(cluster_level,   file.path(out_dir, "edhs_zero_dose_cluster_prewored_2026-07-02.rds"))

message("Stage 1 done. Files written to processed/. Run 02_woreda_join.R once the",
        " admin-3 shapefile is supplied.")

# -----------------------------------------------------------------------------
# NOTES
# - Zero-dose = no DTP1 (h3). h3 == 8 (don't know) is NA and excluded; to count
#   DK as unvaccinated, change the dtp1_recd TRUE branch to 0L.
# - wt, psu, strata are retained for design-based direct estimates (survey pkg).
# - No woreda/region/zone fields are attached in this stage: DHS's own GPS files
#   only carry ADM1NAME (region, as coded at the time of that survey), not a
#   woreda identifier, and there is no common ID with any admin-3 shapefile.
#   Assigning woreda requires a point-in-polygon SPATIAL join against an actual
#   admin-3 boundary file (see 02_woreda_join.R) rather than a key-based merge.
# -----------------------------------------------------------------------------
