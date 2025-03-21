##################
## Plot Missing ##
##################

# load dataset
rm(list = ls())

# source("Analysis/merge.R")
# load(file = "Analysis/ads.RData")

load(file = "Analysis/ads_clean.RData")
library(tidyverse)
library(patchwork)

# ads_clean is the analysis dataset (excluding HIV, pregnant, and those patients who likely achieved initial)
# create dataset with total number of patients, and % of missing data per variable
ads_clean %>% names() %>% sort()

# select variables to be included in analysis

variables <- c(
  "STUDYID",
  "MB_COMBINED", 
  "VL_DURATION", "VL_HISTORY", 
  "DM_AGE", 
  "MP_BL_SPLEEN_LENGTH", 
  "LB_BL_ALT", "LB_BL_CREAT", "LB_BL_HGB", "LB_BL_PLAT", "LB_BL_WBC",
  "VS_BL_HEIGHT", "VS_BL_WEIGHT"
  )

# complete variables: STUDYID, OUT_DC_RELAPSE, DM_SEX, TREATMENT GROUP

# tidy
ads_missing <- ads_clean %>% 
  select(all_of(variables)) %>% 
  group_by(STUDYID) %>% 
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x)),
      .names = "{.col}_missing"
    ),
    n = n(),
  ) %>% 
  ungroup() %>% 
  mutate(
    across(
      !c(n, STUDYID),
      ~ 100 * .x / n,
      .names = "{.col}pct"
    )
  )

# pivot longer
ads_missing_long <- ads_missing %>% 
  pivot_longer(
    cols = -c(n, STUDYID),
    names_to = c("variable", ".value"),
    names_pattern = "^(.*)_(missing|missingpct)$"
  ) %>% 
  rename(
    missing_num = missing,
    missing_pct = missingpct
  )

# rename variable to long form
ads_missing_long <- ads_missing_long %>% 
  mutate(
    variable_long = case_match(
      variable,
      "DM_AGE" ~ "Age",
      "LB_BL_ALT" ~ "Lab: ALT",
      "LB_BL_CREAT" ~ "Lab: Creatinine",
      "LB_BL_HGB" ~ "Lab: Haemoglobin",
      "LB_BL_WBC" ~ "Lab: White blood cells",
      "LB_BL_PLAT" ~ "Lab: Platelets",
      "MP_BL_SPLEEN_LENGTH" ~ "Spleen length",
      "MB_COMBINED" ~ "Parasite grade",
      "VL_DURATION" ~ "Duration of fever",
      "VL_HISTORY" ~ "History of VL",
      "VS_BL_HEIGHT" ~ "Height",
      "VS_BL_WEIGHT" ~ "Weight"
    )
  )

# create factors for plotting
variable_factors <- ads_missing_long %>% group_by(variable_long) %>% summarise(total = sum(missing_num)) %>% arrange(total) %>% pull(variable_long)
ads_missing_long <- ads_missing_long %>% 
  mutate(
    STUDYID = reorder(STUDYID, n),
    variable_long = factor(variable_long, levels = variable_factors)
  )

ads_missing_long %>% glimpse()

plot <- ads_missing_long %>% 
  ggplot() + 
  geom_point(
    aes(
      x = variable_long,
      y = STUDYID,
      fill = missing_pct
    ),
    shape = 22,
    size = 8
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Variable with missing data",
    y = "Dataset ID",
    fill = "% missing"
  ) + 
  scale_fill_gradientn(
    colours = c("green", "yellow", "red", "purple", "black")
    ) + 
  guides(
    fill = guide_colorbar(
      barheight = 25
    )) + 
  geom_point(
    aes(
      x = variable_long,
      y = STUDYID,
    ),
    data = ads_missing_long %>% filter(missing_pct == 0)
  ) + 
  geom_point(
    aes(
      x = variable_long,
      y = STUDYID,
    ),
    shape = 21,
    fill = "white",
    data = ads_missing_long %>% filter(missing_pct == 100)
  )
plot

total_num <- ads_missing_long %>% group_by(STUDYID, n) %>% summarise(total_missing = sum(missing_num)) %>% pull(n) %>% sum()
plot_missing <- ads_missing_long %>% 
  group_by(variable_long) %>% 
  summarise(total = 100 * sum(missing_num) / total_num)

plot_missing %>% 
  ggplot() + 
  geom_bar(
    stat = "identity",
    aes(
      x = variable_long,
      y = total
    ),
    width = 0.4
  ) + 
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50),
    name = "% missing"
  ) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "variable with missing data")

ads_missing_long %>% 
  distinct(STUDYID, n) %>% 
  ggplot() + 
  geom_bar(
    stat = "identity",
    aes(
      y = STUDYID,
      x = n
    ),
    width = 0.75
  ) + 
  scale_x_continuous(
    name = "IPD per dataset",
    breaks = seq(0, 1000, 200)
  ) + 
  theme_minimal()

