# plot missing LB values at baseline (BL) and initial cure (IC)

library(tidyverse)
library(patchwork)

rm(list = ls())

# merge datasets
source("Analysis/merge.R")   

# prepare missing data tables
source("Analysis/prepare_missing.R")

## June 25th 2024: review missing LB data from the following studies: 

# VSGPDL: missing AST at IC                             - relook complete. AST at IC not recoverable. The few missing values probably are recoverable from NA VISIT data, but this would take a long time.
# VLZUKHR: missing CREAT at BL & IC                     - relook complete. LB domain contains only 234 (/646) USUBJID. Very few patients had creatinine performed. 
# VGKSTG: missing WBC, AST, ALT, PLAT, CREAT at IC      - relook complete. Confirm that HGB is the only 30 day blood test result available. 
# VFETIZ: missing PLAT, CREAT at IC                     - relook complete. No PLAT/CREAT blood tests available. 
# VDXALE: missing HGB, WBC, AST, ALT, PLAT, CREAT at IC - relook complete. Confirm that only screening bloods are available. 

# Create plot
ads_missing <- ads_missing_pivot2 %>% 
  filter(str_detect(var, "(^LB_)|USUBJID")) %>% 
  mutate(
    var = as.character(var),
    var = ifelse(var == "USUBJID", "LB_BL_TOTAL", var))

ads_missing %>% count(var) %>% print(n = Inf)

lb_plot <- ads_missing %>% 
  mutate(
    TEMP = str_match(var, "_(BL|IC)")[, 2],
    LBTESTCD = str_match(var, "_([A-Z]+)$")[,2]
    ) %>% 
  select(-var) %>% 
  relocate(STUDYID, LBTESTCD, TEMP, NUM, PCT) %>% 
  pivot_wider(
    id_cols = c(STUDYID, LBTESTCD),
    names_from = TEMP,
    values_from = c(NUM, PCT),
    values_fill = 0) %>% 
    arrange(desc(PCT_BL), desc(PCT_IC)) %>% 
  mutate(
    NUM_IC = ifelse(LBTESTCD == "TOTAL", NUM_BL, NUM_IC),
    PCT_IC = ifelse(LBTESTCD == "TOTAL", 100, PCT_IC)
  )

levels <- lb_plot %>% 
  group_by(LBTESTCD) %>% 
  summarise(n = sum(NUM_BL)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(LBTESTCD)

lb_plot <- lb_plot %>% 
  mutate(LBTESTCD = factor(LBTESTCD, levels = levels))

# baseline labs
shapes <- c("all_missing" = 20, "no_missing" = 1)
b1 <- lb_plot %>% 
  ggplot(aes(x = LBTESTCD, y = STUDYID, fill = PCT_BL)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Baseline LB Domain Missingness",
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
  geom_point(data = lb_plot %>% filter(PCT_BL == 100), 
             aes(x = LBTESTCD, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = lb_plot %>% filter(PCT_BL == 0), 
             aes(x = LBTESTCD, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

b2 <- lb_plot %>% filter(LBTESTCD == "TOTAL") %>% 
  ggplot() + 
  geom_bar(
    aes(x = STUDYID, y = NUM_BL),
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

b3 <- lb_plot %>% group_by(LBTESTCD) %>% summarise(num = sum(NUM_BL)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = LBTESTCD, y = num),
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



# initial cure labs
shapes <- c("all_missing" = 20, "no_missing" = 1)
i1 <- lb_plot %>% 
  ggplot(aes(x = LBTESTCD, y = STUDYID, fill = PCT_IC)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Initial Cure LB Domain Missingness",
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
  geom_point(data = lb_plot %>% filter(PCT_IC == 100), 
             aes(x = LBTESTCD, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = lb_plot %>% filter(PCT_IC == 0), 
             aes(x = LBTESTCD, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

i2 <- lb_plot %>% filter(LBTESTCD == "TOTAL") %>% 
  ggplot() + 
  geom_bar(
    aes(x = STUDYID, y = NUM_BL),
    stat = "identity"
  ) + 
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    name = "Dataset size"
  ) +
  theme(
    axis.title.y = element_blank(),
    #axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = unit(c(1, 1, 0, 1), "lines"))

i3 <- lb_plot %>% group_by(LBTESTCD) %>% summarise(num = sum(NUM_IC)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = LBTESTCD, y = num),
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

plot2 <- (i1 + i2 + plot_layout(guides = "collect")) + i3 + plot_spacer() + 
  plot_layout(
    axes = "collect", 
    height = c(4, 1),
    width = c(6, 1), 
    nrow = 2)

ggsave("Analysis/lb_plot_bl.pdf", plot = plot1, width = 12, height = 7.5)
ggsave("Analysis/lb_plot_ic.pdf", plot = plot2, width = 12, height = 7.5)
