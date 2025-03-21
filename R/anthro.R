##################
# WHO ANTHRO (+) #
##################

rm(list = ls())
library("anthro")
library("anthroplus")
library("tidyverse")
library("lme4")
library("ggrepel")
library("micemd")

source("definitions.R")
load("Analysis/ads_clean.RData")

#source("Analysis/clean.R")
ads_model <- ads_clean

# identify weight/age/height outliers ----

ads_clean <- ads_clean %>%  # from inspecting weight / age scatter plot (see below)
  mutate(
    WEIGHT_AGE_OUTLIERS = 
      (DM_AGE < 10 & VS_BL_WEIGHT > 35) | 
      (VS_BL_WEIGHT < 10 & DM_AGE > 10) |
      (DM_AGE > 5 & VS_BL_WEIGHT < 7) |
      (DM_AGE > 40 & VS_BL_WEIGHT < 15),
    label = ifelse(WEIGHT_AGE_OUTLIERS, USUBJID, NA)
  )


ads_model <- ads_clean %>% 
  mutate(
    LIVER_DIFF = (MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) / IC_DAYS,
    SPLEEN_DIFF = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS,
    DM_AGEs = (DM_AGE - mean(DM_AGE, na.rm = TRUE)) / 10,
    MB_relevel = relevel(factor(MB_COMBINED), ref = "1"),
    OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE),
    WEIGHT_DIFF = (VS_IC_WEIGHT - VS_BL_WEIGHT)/IC_DAYS,
    VS_BL_WEIGHTs = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
    BMI = VS_BL_WEIGHT / (VS_BL_HEIGHT/100)^2,
    BMIs = (BMI - mean(BMI, na.rm = TRUE)) / 10
  )

ads_clean %>% filter(WEIGHT_AGE_OUTLIERS) %>% pull(USUBJID)
# VSGPDL_MUZAFFARPUR_JDF-0620: age is incorrect
# VLNAZSK_PATNA_2004: age is (likely) incorrect
# VSGPDL_MUZAFFARPUR_JDF-0485: weight is incorrect (both baseline and initial cure)
# VSGPDL_MUZAFFARPUR_JDF-0384: age is incorrect
# VSGPDL_MUZAFFARPUR_JDF-0677: weight is incorrect (both baseline and initial cure)
# VRBQIF_MUZAFFARPUR_FST-008: age is incorrect
# VLEALTT_PATNA_52: based on weight-weight, assume age is incorrect (no height)
# VLAULV_MUZAFFARPUR_AA-219: based on weight-weight, assume age is incorrect (no height)

p1 <- ads_clean %>% 
  arrange(WEIGHT_AGE_OUTLIERS) %>% 
  ggplot() +
  geom_jitter(
    aes(
      x = DM_AGE,
      y = VS_BL_WEIGHT,
      colour = WEIGHT_AGE_OUTLIERS),
    height = 0, 
    width = 0
  ) + 
  geom_text_repel(
    aes(
      x = DM_AGE,
      y = VS_BL_WEIGHT,
      label = label
      )
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 5)
  )
ggsave(filename = "Analysis/anthro/outlier_age_weight.png", plot = p1, width = 13, height = 13)

p2 <- ads_clean %>% 
  arrange(WEIGHT_AGE_OUTLIERS) %>% 
  ggplot() +
  geom_jitter(
    aes(
      x = DM_AGE,
      y = VS_BL_HEIGHT,
      colour = WEIGHT_AGE_OUTLIERS
    ),
    height = 0, 
    width = 0
  ) + 
  geom_text_repel(
    aes(
      x = DM_AGE,
      y = VS_BL_HEIGHT,
      label = label
      )
  ) +
  scale_y_continuous(
    #breaks = seq(0, 100, 5)
  )
ggsave(filename = "Analysis/anthro/outlier_age_height.png", plot = p2, width = 13, height = 13)

p3 <- ads_clean %>% 
  arrange(WEIGHT_AGE_OUTLIERS) %>% 
  ggplot() +
  geom_jitter(
    aes(
      y = VS_BL_WEIGHT,
      x = VS_BL_HEIGHT,
      colour = WEIGHT_AGE_OUTLIERS
    ),
    height = 0, 
    width = 0
  ) + 
  geom_text_repel(
    aes(
      y = VS_BL_WEIGHT,
      x = VS_BL_HEIGHT,
      label = label
      )
  ) +
  scale_y_continuous(
    #breaks = seq(0, 100, 5)
  )
ggsave(filename = "Analysis/anthro/outlier_height_weight.png", plot = p3, width = 13, height = 13)

p4 <- ads_clean %>% 
  arrange(WEIGHT_AGE_OUTLIERS) %>% 
  ggplot() +
  geom_jitter(
    aes(
      y = VS_IC_WEIGHT,
      x = VS_BL_WEIGHT,
      colour = WEIGHT_AGE_OUTLIERS
    ),
    height = 0, 
    width = 0
  ) + 
  geom_text_repel(
    aes(
      y = VS_IC_WEIGHT,
      x = VS_BL_WEIGHT,
      label = label
      )
  ) +
  scale_y_continuous(
    #breaks = seq(0, 100, 5)
  )
ggsave(filename = "Analysis/anthro/outlier_weight_weight.png", plot = p4, width = 13, height = 13)

# create z-scores ----

ads_model %>% names() %>% sort()

# for age < 5 years
ads_model_anthro_u5 <- ads_model %>% 
  filter(DM_AGE < 5)

out1 <- anthro_zscores(
  sex = ads_model_anthro_u5$DM_SEX,
  age = ads_model_anthro_u5$DM_AGE * 12,
  weight = ads_model_anthro_u5$VS_BL_WEIGHT,
  lenhei = ads_model_anthro_u5$VS_BL_HEIGHT,
  is_age_in_month = TRUE
)

ads_model_anthro_u5 <- out1 %>% select(zwfl, zbmi) %>% cbind(ads_model_anthro_u5)
ads_model_anthro_u5 %>% names() %>% sort()

# for age >= 5 years
ads_model_anthro_o5 <- ads_model %>% 
  filter(DM_AGE >= 5 | is.na(DM_AGE))

out2 <- anthroplus_zscores(
  sex = ads_model_anthro_o5$DM_SEX,
  age_in_months = ads_model_anthro_o5$DM_AGE * 12 + 1,
  weight_in_kg = ads_model_anthro_o5$VS_BL_WEIGHT,
  height_in_cm = ads_model_anthro_o5$VS_BL_HEIGHT
)
ads_model_anthro_o5 <- out2 %>% select(zbfa) %>% cbind(ads_model_anthro_o5) %>% 
  rename(zbmi = zbfa) %>% mutate(zwfl = NA)
ads_model_anthro_o5 %>% names() %>% sort()

ads_model_anthro_o5 %>% ggplot() + geom_histogram(aes(x = zbmi))
ads_model_anthro_o5 %>% ggplot() + geom_point(aes(x = DM_AGE, y = zbmi))


ads_model_bind <- rbind(ads_model_anthro_u5, ads_model_anthro_o5)
ads_model_bind %>% select(DM_AGE, VS_BL_WEIGHT, VS_BL_HEIGHT, zbmi, zwfl) %>% md.pattern()

# explore z scores....

# severe wasting

# if age < 5 use weight for length/height
# if age 5 - 19 (inclusive) use BMI for age zscore
# if age > 19 use BMI with cut-offs

ads_model_bind2 <- ads_model_bind %>%
  mutate(
    BMI = VS_BL_WEIGHT / ((VS_BL_HEIGHT / 100)^2),
    wast_sev = case_when(
      DM_AGE < 5 & zwfl <= -3                   ~ TRUE,
      DM_AGE < 5 & is.na(zwfl)                  ~ NA,
      DM_AGE >= 5 & DM_AGE < 19 & zbmi <= -3    ~ TRUE,
      DM_AGE >= 5 & DM_AGE < 19 & is.na(zbmi)   ~ NA,
      DM_AGE >= 19 & BMI < 16                   ~ TRUE,
      DM_AGE >= 19 & is.na(BMI)                 ~ NA,
      is.na(DM_AGE)                             ~ NA,
      .default = FALSE
    ),
    wast_mod = case_when(
      DM_AGE < 5 & zwfl > -3 & zwfl <= -2                     ~ TRUE,
      DM_AGE < 5 & is.na(zwfl)                                ~ NA,
      DM_AGE >= 5 & DM_AGE < 19 & zbmi > -3 & zbmi <= -2      ~ TRUE,
      DM_AGE >= 5 & DM_AGE < 19 & is.na(zbmi)                 ~ NA,
      DM_AGE >= 19 & BMI < 18.5 & BMI >= 16                   ~ TRUE,
      DM_AGE >= 19 & is.na(BMI)                               ~ NA,
      is.na(DM_AGE)                                           ~ NA,
      .default = FALSE 
    )
  )

ads_model_bind2 %>% count(wast_mod, wast_sev) # mutually exclusive
ads_model %>% select(VS_BL_WEIGHT, VS_BL_HEIGHT, DM_AGE) %>% md.pattern() # joint missing weight & height = 2949

ads_model_bind3 <- ads_model_bind2 %>% 
  mutate(
    DM_MAL = case_when(
      wast_mod ~ "MODERATE",
      wast_sev ~ "SEVERE",
      is.na(wast_mod) ~ NA,
      !wast_mod & !wast_sev ~ "NORMAL/HIGH"
    )
  ) 

ads_model_bind4 <- ads_model_bind3 %>% 
  mutate(
    DM_AGE_GRP = case_when(
      DM_AGE < 5 ~ "UNDER 5",
      DM_AGE >= 5 & DM_AGE < 19 ~ "5-19",
      DM_AGE >= 19 ~ "19 and over",
      is.na(DM_AGE) ~ NA,
      .default = "ERROR"
    )
  )
ads_model_bind4 %>% count(DM_AGE_GRP, !is.na(zwfl), !is.na(zbmi))
ads_model_bind4 %>% 
  filter(!is.na(DM_MAL)) %>% 
  group_by(DM_AGE_GRP) %>% 
  summarise(
    prop_severe = 100 * sum(DM_MAL == "SEVERE", na.rm = TRUE) / n(),
    prop_moderate = 100 * sum(DM_MAL == "MODERATE", na.rm = TRUE) / n(),
    prop_normal = 100 * sum(DM_MAL == "NORMAL/HIGH", na.rm = TRUE) / n(),
    )
ads_model_bind5 <- ads_model_bind4 %>% 
  mutate(DM_MAL = relevel(factor(DM_MAL), ref = "NORMAL/HIGH"))

# https://www.who.int/tools/child-growth-standards

# modelling
mal_uni1 <- glmer(
  OUT_DC_RELAPSE ~ DM_MAL + (1 | STUDYID),
  family = binomial(),
  data = ads_model_bind5
)
summary(mal_uni1)

mal_multi1 <- glmer(
  OUT_DC_RELAPSE ~ bs(DM_AGEs) + (1 | STUDYID),
  family = binomial(),
  data = ads_model_bind5
)
summary(mal_multi1)
