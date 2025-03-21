# plot missing SA values at baseline (BL) and initial cure (IC)

rm(list = ls())
library(tidyverse)
library(patchwork)

# merge datasets
source("Analysis/merge.R")   

# prepare missing data tables
source("Analysis/prepare_missing.R")

# Create plot
ads_missing <- ads_missing_pivot2 %>% 
  filter(str_detect(var, "(^SA_)|USUBJID")) %>% 
  mutate(
    var = as.character(var),
    var = ifelse(var == "USUBJID", "SA_HX_TOTAL", var))

ads_missing %>% count(var) %>% print(n = Inf)

sa_plot <- ads_missing %>% 
  mutate(
    TEMP   = str_match(var, "_(HX|BL|IC)")[, 2],
    SACODE = str_match(var, "^SA_[A-Z][A-Z]_(.+)$")[, 2]
    ) %>% 
  select(-var) %>% 
  relocate(STUDYID, SACODE, TEMP, NUM, PCT) %>% 
  pivot_wider(
    id_cols = c(STUDYID, SACODE),
    names_from = TEMP,
    values_from = c(NUM, PCT),
    values_fill = 0) %>% 
  arrange(desc(PCT_BL), desc(PCT_IC)) %>% 
  mutate(
    NUM_BL = ifelse(SACODE == "TOTAL", NUM_HX, NUM_BL),
    NUM_IC = ifelse(SACODE == "TOTAL", NUM_HX, NUM_IC),
    PCT_BL = ifelse(SACODE == "TOTAL", 100, PCT_BL),
    PCT_IC = ifelse(SACODE == "TOTAL", 100, PCT_IC)
  )
    
levels <- sa_plot %>% 
  group_by(SACODE) %>% 
  summarise(n = sum(NUM_HX)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(SACODE)

sa_plot <- sa_plot %>% 
  mutate(SACODE = factor(SACODE, levels = levels))

# baseline labs
shapes <- c("all_missing" = 20, "no_missing" = 1)
b1 <- sa_plot %>% 
  ggplot(aes(x = SACODE, y = STUDYID, fill = PCT_HX)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "History SA Domain Missingness",
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
  geom_point(data = sa_plot %>% filter(PCT_HX == 100), 
             aes(x = SACODE, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = sa_plot %>% filter(PCT_HX == 0), 
             aes(x = SACODE, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))


b2 <- sa_plot %>% filter(SACODE == "TOTAL") %>% 
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

b3 <- sa_plot %>% group_by(SACODE) %>% summarise(num = sum(NUM_HX)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = SACODE, y = num),
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

# baseline cure
shapes <- c("all_missing" = 20, "no_missing" = 1)
i1 <- sa_plot %>% 
  ggplot(aes(x = SACODE, y = STUDYID, fill = PCT_BL)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Baseline Cure LB Domain Missingness",
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
  geom_point(data = sa_plot %>% filter(PCT_BL == 100), 
             aes(x = SACODE, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = sa_plot %>% filter(PCT_BL == 0), 
             aes(x = SACODE, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

i2 <- sa_plot %>% filter(SACODE == "TOTAL") %>% 
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

i3 <- sa_plot %>% group_by(SACODE) %>% summarise(num = sum(NUM_BL)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = SACODE, y = num),
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


# initial cure
shapes <- c("all_missing" = 20, "no_missing" = 1)
j1 <- sa_plot %>% 
  ggplot(aes(x = SACODE, y = STUDYID, fill = PCT_IC)) +
  geom_tile() +  # Use geom_tile for a density-like plot with continuous fill
  scale_fill_gradient(low = "white", high = "darkgreen") +  # Customize color gradient
  labs(title = "Baseline Cure LB Domain Missingness",
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
  geom_point(data = sa_plot %>% filter(PCT_IC == 100), 
             aes(x = SACODE, y = STUDYID, shape = "no_missing"), 
             color = "green", size = 2) +  # Star shape (8) with yellow color
  geom_point(data = sa_plot %>% filter(PCT_IC == 0), 
             aes(x = SACODE, y = STUDYID, shape = "all_missing"), 
             color = "black", size = 2) +  # Square shape (15) with black color
  scale_shape_manual(
    values = shapes,
    labels = c("All missing", "All present")
    ) +   
  guides(shape = guide_legend(title = "Data quality"))

j2 <- sa_plot %>% filter(SACODE == "TOTAL") %>% 
  ggplot() + 
  geom_bar(
    aes(x = STUDYID, y = NUM_IC),
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

j3 <- sa_plot %>% group_by(SACODE) %>% summarise(num = sum(NUM_IC)) %>% ungroup() %>% 
  ggplot() +
  geom_bar(
    aes(x = SACODE, y = num),
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

plot3 <- (j1 + j2 + plot_layout(guides = "collect")) + j3 + plot_spacer() + 
  plot_layout(
    axes = "collect", 
    height = c(4, 1),
    width = c(6, 1), 
    nrow = 2)

ggsave("Analysis/sa_plot_hx.pdf", plot = plot1, width = 12, height = 7.5)
ggsave("Analysis/sa_plot_bl.pdf", plot = plot2, width = 12, height = 7.5)
ggsave("Analysis/sa_plot_ic.pdf", plot = plot3, width = 12, height = 7.5)