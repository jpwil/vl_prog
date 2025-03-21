###############
## TREATMENT ##
###############

rm(list = ls())
library(tidyverse)
library(lme4)
library(micemd)
load("Analysis/ads_clean.RData")
load("Analysis/ads_dirty.RData") 

# run this if updating the Excel spreadsheet
#source("Analysis/clean.R")

ads_model <- ads_clean %>% 
  mutate(
    LIVER_DIFF = (MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) / IC_DAYS,
    SPLEEN_DIFF = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS,
    DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10,
    MB_relevel = relevel(factor(MB_COMBINED), ref = "1"),
    OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE),
    WEIGHT_DIFF = (VS_IC_WEIGHT - VS_BL_WEIGHT)/IC_DAYS,
    VS_BL_WEIGHTs = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
    BMI = VS_BL_WEIGHT / (VS_BL_HEIGHT/100)^2,
    BMIs = (BMI - mean(BMI, na.rm = TRUE)) / 10
  )

ads_model %>% names() %>% sort()
ads_model %>% count(DM_ARM, TREAT_DRUG_NAME) %>% print(n = Inf)

STUDYID_FCT <- ads_model %>% 
  group_by(STUDYID) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  pull(STUDYID)
STUDYID_FCT

TREATMENT_FCT <- ads_model %>% 
  group_by(TREAT_DRUG_NAME) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  pull(TREAT_DRUG_NAME)
TREATMENT_FCT

treat_dist1a <- ads_model %>% 
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  ggplot(
    aes(
      x = factor(STUDYID, levels = STUDYID_FCT),
      y = total,
      fill = TREAT_DRUG_NAME,
      group = factor(TREAT_TEXT)
    )
  ) + 
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) + 
  scale_fill_manual(
    values = c(
      "Ampho." = "#ff860d",
      "L-AmB" = "yellow",
      "L-AmB (BS)" = "yellow4",
      "L-AmB (LI)" = "#a7d631",
      "MF" = "#f45252",
      "L-AmB / MF" = "#c1c0c0",
      "PM" = "lightgreen",
      "SSG" = "deepskyblue",
      "HPE" = "white"
      )
    ) + 
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Treatment regimen by IDDO VL Dataset",
    x = "IDDO VL Dataset",
    y = "Number of patients",
    caption = "Each colour corresponds to different treatment (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "Treatment"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1),
    plot.caption = element_text(
      hjust = 0,    
      vjust = 1,
      size = 10
  ))

treat_dist1a

ggsave(filename = "Analysis/treatment/treat_dist1a.pdf", plot = treat_dist1a, width = 20, height = 12)

ads_model_adj <- ads_model %>% 
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>% 
  summarise(total_sg = n()) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(total_study = sum(total_sg)) %>% 
  ungroup() %>% 
  mutate(prop = total_sg / total_study)

treat_dist1b <- ads_model_adj %>% 
  ggplot(
    aes(
      x = factor(STUDYID, levels = STUDYID_FCT),
      y = prop,
      fill = TREAT_DRUG_NAME,
      group = factor(TREAT_TEXT)
    )
  ) + 
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) + 
  scale_fill_manual(
    values = c(
      "Ampho." = "#ff860d",
      "L-AmB" = "yellow",
      "L-AmB (BS)" = "yellow4",
      "L-AmB (LI)" = "#a7d631",
      "MF" = "#f45252",
      "L-AmB / MF" = "#c1c0c0",
      "PM" = "lightgreen",
      "SSG" = "deepskyblue",
      "HPE" = "white"
      )
    ) + 
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Treatment regimen by IDDO VL Dataset",
    x = "IDDO VL Dataset",
    y = "Proportion of patients",
    caption = "Each colour corresponds to different treatment (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "Treatment"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1),
    plot.caption = element_text(
      hjust = 0,    
      vjust = 1,
      size = 10
  ))

ggsave(filename = "Analysis/treatment/treat_dist1b.pdf", plot = treat_dist1b, width = 16, height = 12)

treat_dist1c <- ads_model %>% 
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  ggplot(
    aes(
      x = factor(TREAT_DRUG_NAME, levels = TREATMENT_FCT),
      y = total,
      fill = STUDYID,
      group = factor(TREAT_TEXT)
    )
  ) + 
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) + 
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "VL Datasets by Treatment",
    x = "Treatment",
    y = "Number of patients",
    caption = "Each colour corresponds to different dataset (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "VL Dataset"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1),
    plot.caption = element_text(
      hjust = 0,    
      vjust = 1,
      size = 10
  ))

ggsave(filename = "Analysis/treatment/treat_dist1c.pdf", plot = treat_dist1c, width = 20, height = 12)


ads_model_adj2 <- ads_model %>% 
  arrange(TREAT_DRUG_NAME, TREAT_TEXT) %>% 
  group_by(STUDYID, TREAT_DRUG_NAME, TREAT_TEXT) %>% 
  summarise(total_sg = n()) %>% 
  ungroup() %>% 
  group_by(TREAT_DRUG_NAME) %>% 
  mutate(total_treat = sum(total_sg)) %>% 
  ungroup() %>% 
  mutate(prop = total_sg / total_treat)

treat_dist1d <- ads_model_adj2 %>% 
  ggplot(
    aes(
      x = factor(TREAT_DRUG_NAME, levels = TREATMENT_FCT),
      y = prop,
      fill = STUDYID,
      group = factor(TREAT_TEXT)
    )
  ) + 
  geom_bar(
    stat = "identity",
    colour = "black",
    position = "stack"
  ) + 
  geom_label(
    aes(
      label = TREAT_TEXT
    ),
    size = 3,
    show.legend = FALSE,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "VL Datasets by Treatment",
    x = "Treatment",
    y = "Proportion of patients",
    caption = "Each colour corresponds to different dataset (as per legend). Labels distinguish treatment regimens.\nVLNAZSK and VVNGOE are datasets from the same publication, corresponding to different sites (Patna and Muzaffarpur, respectively)\n\nAbbreviations: Ampho: amphotericin B deoxycholate; HPE: human placenta extract; L-AmB: liposomal amphotericin B; LI: Lifecare Innovations; BS: Bharat Serums and Vaccines; MF, M: miltefosine; ALT: alternate days; CONS: consecutive days.\nPM: paromomycin; SSG: sodium stibogluconate; m: miligrams per kilogram per day; D: day(s)\n\nFor example: 10m 1D corresponds to 10mg/kg/day for one day.Unless otherwise stated, L-AmB manufacturer is Gilead. L-AmB / MF corresponds to combination therapy with L-AmB and MF.\n\nPM regimens: always 11mg/kg/day (base), intramuscular. Ampho. regimens: always 1mg/kg doses either on alternate or consecutive days. PE dose of 2ml Placentrex (2.06mg) intramuscular.\nMF dose is standardised across all studies (based on weight and age). SSG regimens are always 20mg/kg/day for 30 days.",
    fill = "Treatment"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1),
    plot.caption = element_text(
      hjust = 0,    
      vjust = 1,
      size = 10
  ))

ggsave(filename = "Analysis/treatment/treat_dist1d.pdf", plot = treat_dist1d, width = 16, height = 12)

# let's do some modelling
ads_model %>% 
  group_by(TREAT_GRP3) %>% 
  summarise(
    prop_relapse = sum(OUT_DC_RELAPSE) / n(),
    total = n()
    )  %>% 
  arrange(prop_relapse)

group1_uni <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP1 + (1 | STUDYID),
  family = binomial(),
  data = ads_model
  )
summary(group1_uni)

# as expected, this does not converge (unsolvable due to collinearity)
group2_uni <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP2 + (1 | STUDYID),
  family = binomial(),
  data = ads_model
  )
summary(group2_uni)

group2_multi <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP2 + DM_AGEs + I(DM_AGEs^2) + DM_SEX + MB_COMBINED + (1 | STUDYID),
  family = binomial(),
  data = ads_model
  )
summary(group2_multi)

# group3 - parisominious group
group3_uni <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP3 + (1 | STUDYID),
  family = binomial(),
  data = ads_model
  )
summary(group3_uni)

group3_multi <- glmer(
  OUT_DC_RELAPSE ~ TREAT_GRP3 + DM_AGEs + I(DM_AGEs^2) + DM_SEX + MB_COMBINED + (1 | STUDYID),
  family = binomial(),
  data = ads_model
  )
summary(group3_multi)