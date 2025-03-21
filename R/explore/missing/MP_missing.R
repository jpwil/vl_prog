# plot missing MP values at baseline (BL) and initial cure (IC)

rm(list = ls())
library(tidyverse)
library(patchwork)

## missing SPLEEN_LENGTH
# VFETIZ: not curated yet

## missing LIVER_LENGTH
# VGKSTG:   confirmed, no liver length available
# VFETIZ:   not curated yet
# VDXALE:   confirmed, no liver length available
# VLNAZSK:  confirmed, no liver length available
# VAQMOU:   confirmed, no liver length available

# merge datasets
source("Analysis/merge.R")   

# prepare missing data tables
source("Analysis/prepare_missing.R")

# Create plot
ads_missing <- ads_missing_pivot2 %>% 
  filter(str_detect(var, "(^MP_)|USUBJID")) %>% 
  mutate(
    var = as.character(var),
    var = ifelse(var == "USUBJID", "MP_BL_TOTAL", var)) %>% 
  filter(
    var != "MP_IC_SPLEEN_WIDTH",
    var != "MP_BL_SPLEEN_WIDTH")

ads_missing %>% count(var) %>% print(n = Inf)

mp_plot <- ads_missing %>% 
  mutate(
    TEMP = str_match(var, "_(BL|IC)")[, 2],
    MPTESTCD = str_match(var, "^MP_(BL|IC)_(.+)$")[, 3]
    ) %>% 
  select(-var) %>% 
  relocate(STUDYID, MPTESTCD, TEMP, NUM, PCT) %>% 
  pivot_wider(
    id_cols = c(STUDYID, MPTESTCD),
    names_from = TEMP,
    values_from = c(NUM, PCT),
    values_fill = 0) %>% 
    arrange(desc(PCT_BL), desc(PCT_IC)) %>% 
  mutate(
    NUM_IC = ifelse(MPTESTCD == "TOTAL", NUM_BL, NUM_IC),
    PCT_IC = ifelse(MPTESTCD == "TOTAL", 100, PCT_IC)
  )

levels <- mp_plot %>% 
  group_by(MPTESTCD) %>% 
  summarise(n = sum(NUM_BL)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(MPTESTCD)

mp_plot <- mp_plot %>% 
  mutate(MPTESTCD = factor(MPTESTCD, levels = levels))

# baseline labs
shapes <- c("all_missing" = 20, "no_missing" = 1)
b1 <- mp_plot %>% 
  ggplot(aes(x = MPTESTCD, y = STUDYID, fill = PCT_BL)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Baseline MP Domain Missingness",
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
  geom_point(data = mp_plot %>% filter(PCT_BL == 100), 
             aes(x = MPTESTCD, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = mp_plot %>% filter(PCT_BL == 0), 
             aes(x = MPTESTCD, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

b2 <- mp_plot %>% filter(MPTESTCD == "TOTAL") %>% 
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

b3 <- mp_plot %>% group_by(MPTESTCD) %>% summarise(num = sum(NUM_BL)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = MPTESTCD, y = num),
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
i1 <- mp_plot %>% 
  ggplot(aes(x = MPTESTCD, y = STUDYID, fill = PCT_IC)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Initial Cure MP Domain Missingness",
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
  geom_point(data = mp_plot %>% filter(PCT_IC == 100), 
             aes(x = MPTESTCD, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = mp_plot %>% filter(PCT_IC == 0), 
             aes(x = MPTESTCD, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

i2 <- mp_plot %>% filter(MPTESTCD == "TOTAL") %>% 
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

i3 <- mp_plot %>% group_by(MPTESTCD) %>% summarise(num = sum(NUM_IC)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = MPTESTCD, y = num),
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

ggsave("Analysis/mp_plot_bl.pdf", plot = plot1, width = 12, height = 7.5)
ggsave("Analysis/mp_plot_ic.pdf", plot = plot2, width = 12, height = 7.5)
