suppressMessages(library(tidyverse))
cluster <- readRDS("data/clean/edhs_zero_dose_cluster_final_2026-07-02.rds")

# ---- Chart C: coverage histogram (clusters per covered woreda) -------------
cov <- cluster %>% filter(!is.na(woreda_id)) %>% count(woreda_id, name = "n_clusters")
cov_binned <- cov %>%
  mutate(bin = case_when(
    n_clusters == 1 ~ "1",
    n_clusters == 2 ~ "2",
    n_clusters == 3 ~ "3",
    n_clusters %in% 4:5 ~ "4 to 5",
    n_clusters %in% 6:10 ~ "6 to 10",
    n_clusters > 10 ~ "11+"
  )) %>%
  mutate(bin = factor(bin, levels = c("1","2","3","4 to 5","6 to 10","11+"))) %>%
  count(bin, name = "n_woredas")

n_total_woreda_2024 <- 1067
n_covered <- nrow(cov)
n_uncovered_approx <- n_total_woreda_2024 - n_covered

pC <- ggplot(cov_binned, aes(x = bin, y = n_woredas)) +
  geom_col(fill = "#3182bd", width = 0.65) +
  geom_text(aes(label = n_woredas), vjust = -0.4, size = 4) +
  labs(title = "How many clusters land in the same woreda? (pooled, 2011 to 2019)",
       subtitle = str_wrap(paste0(n_covered, " distinct woredas hit by at least one cluster, out of roughly ",
                          n_total_woreda_2024, " nationally (approx. 2024 count): ",
                          round(n_covered / n_total_woreda_2024 * 100, 1),
                          "% covered, ", n_uncovered_approx, " likely uncovered"), width = 85),
       x = "Clusters sharing the same woreda", y = "Number of woredas") +
  ylim(0, max(cov_binned$n_woredas) * 1.15) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("data/clean/maps/woreda_coverage_histogram.png", pC, width = 9, height = 6.5, dpi = 200, bg = "white")

# ---- Chart D: boundary-flag rate by region, sorted --------------------------
flag_by_region <- cluster %>% filter(!is.na(woreda)) %>%
  group_by(region) %>%
  summarise(n = n(), pct_flagged = mean(boundary_flag) * 100) %>%
  arrange(pct_flagged)

pD <- flag_by_region %>%
  mutate(region_label = paste0(region, " (n=", n, ")"),
         region_label = factor(region_label, levels = region_label)) %>%
  ggplot(aes(x = region_label, y = pct_flagged)) +
  geom_col(fill = "#e6550d", width = 0.65) +
  geom_text(aes(label = paste0(round(pct_flagged, 0), "%")), hjust = -0.15, size = 3.6) +
  coord_flip() +
  ylim(0, 105) +
  labs(title = "Share of clusters near a woreda boundary, by region",
       subtitle = "Higher = more clusters whose woreda assignment is sensitive to\nDHS coordinate displacement (2023 polygon vintage)",
       x = NULL, y = "Per cent of clusters flagged") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("data/clean/maps/woreda_boundary_flag_by_region.png", pD, width = 9.5, height = 6.5, dpi = 200, bg = "white")

message("Charts C and D done. n_covered=", n_covered, " n_uncovered_approx=", n_uncovered_approx)
