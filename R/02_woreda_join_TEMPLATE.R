# =============================================================================
# STAGE 2 (template, not yet run): assign each DHS cluster to a woreda
# (admin-3) by a SPATIAL join, then re-aggregate to cluster level with woreda
# attached. Run this once an admin-3 boundary shapefile is available; see
# README_STATUS.md for what is needed and where such a file usually comes
# from.
#
# WHY A SPATIAL JOIN AND NOT A KEY-BASED MERGE
# The DHS cluster files (ETGE61FL/ETGE71FL/ETGE81FL) carry a point geometry
# (latnum, longnum) and only a coarse ADM1NAME (region) attribute; they do not
# carry a woreda code, and no admin-3 shapefile carries a DHS cluster ID. There
# is therefore no shared column to left_join() or merge() on. The only way to
# connect a cluster to a woreda is geometric: test whether the cluster's point
# falls inside a given woreda polygon (point-in-polygon), which is what
# sf::st_join(..., join = st_within) does below. This is a spatial predicate,
# not an attribute key, which is why the two files do not need any variable
# in common.
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(sf)
})

sf_use_s2(FALSE)
raw_dir <- "data/raw"
out_dir <- "processed"

# ---- 0. EDIT THESE TWO PATHS once the shapefiles are supplied --------------
admin3_path <- file.path(raw_dir, "Ethadmin3_2023", "ET_Admin3_2023.shp")  # woreda polygons
admin1_path <- file.path(raw_dir, "Ethadmin1_2023", "ET_Admin1_2023.shp")  # region polygons (for maps)

stopifnot(
  "admin3_path not found - supply the woreda boundary shapefile first" = file.exists(admin3_path)
)

cluster_level <- readRDS(file.path(out_dir, "edhs_zero_dose_cluster_prewored_2026-07-02.rds"))
child_merged  <- readRDS(file.path(out_dir, "edhs_zero_dose_child_prewored_2026-07-02.rds"))

# ---- 1. Read admin-3 polygons; adjust field names to match your shapefile --
# CSA/OCHA admin-3 layers typically use ADMIN1/ADMIN2/ADMIN3 or NAME_1/NAME_2/
# NAME_3; inspect with names(st_read(admin3_path)) and edit the transmute()
# below to match. FNID (or an equivalent unique code) is the woreda's own ID,
# independent of the DHS cluster ID; it is retained because zone/woreda names
# alone are not always unique or spelled consistently across sources.
admin3 <- read_sf(admin3_path) %>%
  st_make_valid() %>%
  transmute(region = ADMIN1, zone = ADMIN2, woreda = ADMIN3, woreda_id = FNID)

# ---- 2. Build the cluster point layer, matched to admin3's CRS -------------
cluster_sf <- cluster_level %>%
  st_as_sf(coords = c("longnum", "latnum"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(admin3))

# ---- 3. Point-in-polygon join: each cluster gets the woreda it falls in ----
cluster_woreda <- st_join(cluster_sf, admin3, join = st_within, left = TRUE)

n_unmatched <- sum(is.na(cluster_woreda$woreda))
message("Clusters not matched to a woreda: ", n_unmatched, " / ", nrow(cluster_woreda))

# ---- 4. Distance to the nearest INTERNAL woreda boundary, in a metric CRS --
# DHS displaces coordinates for confidentiality (urban up to 2 km, rural up to
# 5 km, 1% of rural up to 10 km). A cluster that sits within its round's
# displacement radius of a boundary line may have been nudged across it, so
# its woreda assignment should be treated as uncertain rather than exact.
# st_boundary() keeps internal lines (unioning the polygons first would leave
# only the national border, which is not useful here); UTM 37N (EPSG:32637)
# covers Ethiopia and gives distances in metres.
utm_crs   <- 32637
cluster_m <- st_transform(cluster_woreda, utm_crs)
wbound_m  <- admin3 %>% st_transform(utm_crs) %>% st_boundary() %>% st_union()

cluster_woreda <- cluster_woreda %>%
  mutate(
    dist_bound_km = as.numeric(st_distance(cluster_m, wbound_m)) / 1000,
    displace_km   = if_else(urban_rural == "urban", 2, 5),
    boundary_flag = if_else(dist_bound_km < displace_km, 1L, 0L)
  )

message("Clusters within their own displacement radius of a woreda boundary: ",
        sum(cluster_woreda$boundary_flag, na.rm = TRUE))

# ---- 5. Attach woreda (+ boundary flag) back onto cluster and child files --
woreda_lookup <- cluster_woreda %>%
  st_drop_geometry() %>%
  select(survey_year, cluster, region, zone, woreda, woreda_id,
         dist_bound_km, boundary_flag)

cluster_final <- cluster_level %>%
  left_join(woreda_lookup, by = c("survey_year", "cluster"))

child_final <- child_merged %>%
  left_join(woreda_lookup, by = c("survey_year", "cluster"))

# ---- 6. Save final outputs ---------------------------------------------------
write_csv(child_final,   file.path(out_dir, "edhs_zero_dose_child_final_2026-07-02.csv"))
write_csv(cluster_final, file.path(out_dir, "edhs_zero_dose_cluster_final_2026-07-02.csv"))
saveRDS(child_final,     file.path(out_dir, "edhs_zero_dose_child_final_2026-07-02.rds"))
saveRDS(cluster_final,   file.path(out_dir, "edhs_zero_dose_cluster_final_2026-07-02.rds"))

cluster_final %>%
  filter(!is.na(latnum), !is.na(longnum)) %>%
  st_as_sf(coords = c("longnum", "latnum"), crs = 4326, remove = FALSE) %>%
  st_write(file.path(out_dir, "edhs_zero_dose_cluster_final_2026-07-02.gpkg"),
           delete_dsn = TRUE, quiet = TRUE)

message("Stage 2 done. Final child- and cluster-level files (with woreda) written to processed/.")

# -----------------------------------------------------------------------------
# NEXT STEPS FOR SAE
# - Bring in woreda-level covariates for woredas with no sampled cluster
#   (join on woreda_id, the admin-3 shapefile's own unique code, not on any
#   DHS field), so unsampled areas can receive model-based predictions.
# - Standardize covariates using the mean and SD from sampled clusters only,
#   then apply the same transformation to the unsampled-woreda covariate set.
# - Build the INLA-SPDE mesh from cluster_final's coordinates; fit the
#   binomial model (n_zero_dose / n_children) with the standardized covariates
#   as fixed effects and a spatial random field; predict to all woreda
#   centroids or areal units and aggregate for woreda-level estimates.
# - Cross-check clusters with boundary_flag == 1 against a second admin-3
#   source if one is available, since these are the assignments most exposed
#   to DHS's coordinate displacement.
# -----------------------------------------------------------------------------
