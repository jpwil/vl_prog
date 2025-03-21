# plot missing MB values at baseline (BL) and initial cure (IC)

library(tidyverse)
library(patchwork)

rm(list = ls())

# merge datasets
source("Analysis/merge.R")   

# prepare missing data tables
source("Analysis/prepare_missing.R")

# Create plot
ads_missing <- ads_missing_pivot2 %>% 
  filter(str_detect(var, "(^MB_)|USUBJID")) %>% 
  mutate(
    var = as.character(var),
    var = ifelse(var == "USUBJID", "MB_BL_TOTAL", var)) %>% 
  filter(var %in% c("MB_BL_TOTAL", "MB_BL_LSHMANIA_SPLEEN", "MB_BL_LSHMANIA_BONE"))

ads_missing #%>% View()

ads %>% count(MB_BL_HBSAG) # 13 patients are HBsAg positive in 
ads %>% count(MB_BL_HIV)
ads %>% count(STUDYID, MB_BL_HIVAB) %>% print(n = Inf)
ads %>% count(STUDYID, MB_BL_LSHMANIA_BONE) %>% print(n = Inf) # VAQMOU and VDXALE have some bone marrow aspirates
ads %>% count(STUDYID, MB_BL_LSHMANIA_SPLEEN) %>% print(n =  Inf)
ads %>% count(STUDYID, MB_IC_LSHMANIA_SPLEEN) %>% print(n =  Inf) # VFEFCS has IC splenic aspirate results (remove these)
ads %>% count(MB_BL_LSHMRK39)
ads %>% count(MB_BL_MTB_SPUTUM_AFB) # 3 patients are AFB positive in VDXALE
ads %>% count(MB_BL_PLSMDM)
ads %>% count(MB_BL_PLSMDM_SMEAR) # 2 patients are malaria blood smear positive in VDXALE

## June 25th 2024: review missing spleen MB data for the following studies

# VSGPDL - relook complete, only 99 patients with baseline splenic aspirates available
# VLZUKHR - relook complete, no MB domain available
# VYDSGR - relook complete, no MB domain available
# VIVXJN - relook complete, MB domain added (forgot to update dataset)
# VFETIZ - need to request curation
# VLEALTT - relook complete, Only rK39 antibody data are available in MB domain
# VLNXMEA - exclude (relook not performed)
# VDXALE - relook complete, confirmed we have extracted all we can
# VAQMOU - relook complete, confirmed we have extracted all we can

mb_plot <- ads_missing %>% 
  mutate(
    MBTESTCD = str_match(var, "_([A-Z]+)$")[, 2]
    ) %>% 
  select(-var) %>% 
  relocate(STUDYID, MBTESTCD, NUM, PCT)

levels <- mb_plot %>% 
  group_by(MBTESTCD) %>% 
  summarise(n = sum(NUM)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(MBTESTCD)

mb_plot <- mb_plot %>% 
  mutate(MBTESTCD = factor(MBTESTCD, levels = levels))

shapes <- c("all_missing" = 20, "no_missing" = 1)
b1 <- mb_plot %>% 
  ggplot(aes(x = MBTESTCD, y = STUDYID, fill = PCT)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Baseline MB Domain Missingness",
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
  geom_point(data = mb_plot %>% filter(PCT == 100), 
             aes(x = MBTESTCD, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = mb_plot %>% filter(PCT == 0), 
             aes(x = MBTESTCD, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))


b2 <- mb_plot %>% filter(MBTESTCD == "TOTAL") %>% 
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

#ads_missing_pivot2$var %>% levels() == var_missing$variable %>% levels()
ads_missing %>% group_by(var) %>% summarise(num = sum(NUM)) %>% ungroup() 

b3 <- mb_plot %>% group_by(MBTESTCD) %>% summarise(num = sum(NUM)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = MBTESTCD, y = num),
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

ggsave("Analysis/mb_plot_bl.pdf", plot = plot1, width = 10, height = 7.5)