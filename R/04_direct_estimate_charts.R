suppressMessages(library(tidyverse))
national <- read_csv("data/clean/tables/direct_est_national_by_round.csv", show_col_types = FALSE)
by_region <- read_csv("data/clean/tables/direct_est_region_by_round.csv", show_col_types = FALSE)

# ---- Chart 1: national trend with 95% CI -----------------------------------
p1 <- ggplot(national, aes(x = factor(survey_year), y = prevalence_pct)) +
  geom_col(fill = "#2b8cbe", width = 0.55) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 0.6) +
  geom_text(aes(label = paste0(prevalence_pct, "%")), vjust = -2.6, size = 4.2, fontface = "bold") +
  labs(title = "Zero-dose (no DTP1) prevalence, Ethiopia",
       subtitle = "Design-based direct estimate; children 12-23 months\nError bars are 95% confidence intervals (survey-weighted, PSU/strata design)",
       x = "Survey round", y = "Zero-dose prevalence (%)") +
  ylim(0, max(national$ci_high) + 5) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("data/clean/maps/direct_est_national_trend.png", p1, width = 8, height = 6, dpi = 200, bg = "white")

# ---- Chart 2: 2019 regional direct estimates with CI, sorted ---------------
region_tab_2019 <- by_region %>%
  filter(survey_year == 2019) %>%
  mutate(region_label = paste0(dhs_region, " (", n_clusters, " clusters)")) %>%
  arrange(prevalence_pct)

p2 <- region_tab_2019 %>%
  mutate(region_label = factor(region_label, levels = region_label)) %>%
  ggplot(aes(x = region_label, y = prevalence_pct)) +
  geom_col(fill = "#fd8d3c", width = 0.65) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.25) +
  coord_flip() +
  labs(title = "Zero-dose prevalence by region, EDHS 2019",
       subtitle = "Direct estimate with 95% CI; cluster count shown per region",
       x = NULL, y = "Zero-dose prevalence (%)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave("data/clean/maps/direct_est_region_2019_ci.png", p2, width = 10, height = 6.5, dpi = 200, bg = "white")
message("charts done")
