suppressMessages({
  library(tidyverse)
  library(sf)
  library(rnaturalearthdata)
})
sf_use_s2(FALSE)
dir.create("data/clean/maps", showWarnings = FALSE, recursive = TRUE)

cluster <- readRDS("data/clean/edhs_zero_dose_cluster_final_2026-07-02.rds")
data(countries110, package = "rnaturalearthdata")
eth_outline <- countries110[countries110$admin == "Ethiopia", ] %>% st_geometry()

cluster_sf <- cluster %>%
  filter(!is.na(latnum), !is.na(longnum)) %>%
  st_as_sf(coords = c("longnum", "latnum"), crs = 4326, remove = FALSE) %>%
  mutate(match_status = case_when(
    is.na(woreda) ~ "Unmatched (outside all woredas)",
    boundary_flag == 1 ~ "Matched, near a woreda boundary",
    TRUE ~ "Matched, away from any boundary"
  ))

# ---- Map A: match status / boundary-flag map --------------------------------
pA <- ggplot() +
  geom_sf(data = eth_outline, fill = "grey97", colour = "grey40", linewidth = 0.3) +
  geom_sf(data = cluster_sf %>% filter(match_status != "Unmatched (outside all woredas)"),
          aes(colour = match_status), size = 0.85, alpha = 0.75) +
  geom_sf(data = cluster_sf %>% filter(match_status == "Unmatched (outside all woredas)"),
          colour = "red", size = 3, shape = 4, stroke = 1.4) +
  scale_colour_manual(name = NULL, values = c(
    "Matched, near a woreda boundary" = "#e6550d",
    "Matched, away from any boundary" = "#3182bd")) +
  labs(title = "Cluster-to-woreda join: match status",
       subtitle = "Red X = the one unmatched cluster (2011). Orange = within its own round's DHS\ndisplacement radius of a woreda line, so the assignment is plausible but not certain.",
       caption = "Woreda boundary: ET_Admin3_2023 (supplied). Country outline: Natural Earth 1:110m (context only).") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("data/clean/maps/woreda_join_match_status.png", pA, width = 8.5, height = 8.5, dpi = 210, bg = "white")

# ---- Map B: the 6 anomalous cross-region clusters, labelled -----------------
anomalies <- cluster_sf %>%
  filter(!is.na(woreda),
         (dhs_region == "Afar" & region == "Tigray") |
         (dhs_region == "Somali" & region == "Oromia") |
         (dhs_region == "Tigray" & region == "Amhara") |
         (dhs_region == "SNNPR" & region == "Oromia")) %>%
  mutate(lbl = paste0(dhs_region, " to ", region, "\n(", round(dist_bound_km, 2), " km)"))

pB <- ggplot() +
  geom_sf(data = eth_outline, fill = "grey97", colour = "grey40", linewidth = 0.3) +
  geom_sf(data = cluster_sf, colour = "grey80", size = 0.5, alpha = 0.5) +
  geom_sf(data = anomalies, colour = "#d7301f", size = 2.6) +
  ggrepel::geom_label_repel(data = anomalies, aes(label = lbl, geometry = geometry),
                             stat = "sf_coordinates", size = 2.9, seed = 1,
                             label.padding = 0.2, min.segment.length = 0) +
  labs(title = "Clusters whose DHS-era region does not match the 2023 polygon region",
       subtitle = "Excludes the expected SNNPR to South/Central/Sidama/South West split; these six are\nnot explained by any known boundary reorganisation",
       caption = "Distance shown is to the nearest internal woreda boundary line (2023 vintage).") +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12))

ggsave("data/clean/maps/woreda_join_region_anomalies.png", pB, width = 8.5, height = 8.5, dpi = 210, bg = "white")

message("Maps A and B done.")
