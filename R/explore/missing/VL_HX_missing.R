# plot missing VL_HX values

rm(list = ls())
library(tidyverse)
library(patchwork)

## STUDIES WITH NO VL HISTORY

# VSGPDL: not excluded
# VLZUKHR: not excluded, relook completed - not in SA/RS/IN domains
# VYDSGR: not excluded
# VIZGFA: not excluded
# VFEFCS: not excluded (explicitly included), relook completed - not in SA/RS domains, IN not present
# VLAULV: not excluded
# VWPJRM: not excluded
# VEZMZD: not excluded
# VGKSTG: [patients with no previous exposure to SSG]
# VIVXJN: not excluded: relook; 1 pt only with IN evidence of SAG. Not in SA/RS domains. 
# VFETIZ: not excluded: relook: no SA or IN domains. Not in RS. 
# VDXALE: [patients with no previous exposure to SSG]: relook; information in IN domain, retrieved, but only 3 relapses in final dataset ads_clean                                                     
# VLEALTT: not excluded
# VVNGOE: [no previous treatment with antimony or paromomycin unless the treatment terminated two months prior to the study]: relook - not in RS/IN/SA
# VQKRHN: [excluded if they had previously failed treatment with amphotericin B for VL]
# VLNAZSK: [no previous  treatment with antimony or paromomycin unless the treatment terminated two months prior to the study]: relook - confirmed no patients had history of VL (updated)
# VFFFOP: not excluded. relook: not evidence of VL relapse in SA/RS/IN domains.

# merge datasets
source("Analysis/merge.R")   

# prepare missing data tables
source("Analysis/prepare_missing.R")

# Create plot
vl_plot <- ads_missing_pivot2 %>% 
  filter(str_detect(var, "VL_HISTORY|USUBJID")) %>% 
  mutate(
    var = as.character(var),
    var = ifelse(var == "USUBJID", "VL_HX_TOTAL", var))

levels <- vl_plot %>% 
  group_by(var) %>% 
  summarise(n = sum(NUM)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(var)

vl_plot <- vl_plot %>% 
  mutate(var = factor(var, levels = levels))

# baseline labs
shapes <- c("all_missing" = 20, "no_missing" = 1)
b1 <- vl_plot %>% 
  ggplot(aes(x = var, y = STUDYID, fill = PCT)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "VL History Missingness",
       x = "Variable",
       y = "Study ID",
       fill = "% available") +
  theme_minimal() +  # Use a minimal theme for better readability
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = unit(c(1, 0, 0, 1), "lines")
    ) + # Rotate x-axis text for better fit
  geom_point(data = vl_plot %>% filter(PCT == 100), 
             aes(x = var, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = vl_plot %>% filter(PCT == 0), 
             aes(x = var, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

b2 <- vl_plot %>% filter(var == "VL_HX_TOTAL") %>% 
  ggplot() + 
  geom_bar(
    aes(x = STUDYID, y = NUM),
    stat = "identity"
  ) + 
  coord_flip() +
  scale_y_continuous(
    name = "Dataset size"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    #axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = unit(c(1, 1, 0, 1), "lines"))

b3 <- vl_plot %>% group_by(var) %>% summarise(num = sum(NUM)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = var, y = num),
    stat = "identity"
  ) + 
  scale_y_continuous(
    limits = c(0, 5500),   
     breaks = seq(0, 5000, 1000),
    name = "Total present") +
  theme_minimal() +  # Use a minimal theme for better readability
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank())  # Rotate x-axis text for better fit

plot1 <- (b1 + b2 + plot_layout(guides = "collect")) + b3 + plot_spacer() + 
  plot_layout(
    axes = "collect", 
    height = c(4, 1),
    width = c(6, 1), 
    nrow = 2)

ggsave("Analysis/vl_plot_hx.pdf", plot = plot1, width = 12, height = 7.5)