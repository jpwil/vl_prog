########
# SITE #
########

rm(list = ls())
library(tidyverse)
library(lme4)
load("Analysis/ads_clean.RData")
load("Analysis/ads_dirty.RData")

source("definitions.R")

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

ads_model %>% count(STUDYID, DM_SITE) %>% print(n = Inf)
ads_model %>% group_by(STUDYID, DM_SITE) %>% summarise(prop_relapse = sum(OUT_DC_RELAPSE)/n(), tot = n()) %>% arrange(STUDYID, prop_relapse) %>% print(n = Inf)
# except for 1 VL dataset (VLZUKHR), there is a 1:1 relationship with dataset and site
  # Ojo: VVNGOE and VLNAZSK data are published in the same manuscript, however they are curated as separate datasets

# VLZUKHR:
  # MOTIHARA:                     district hospital in main city in East Champaran District of Bihar (n = 446)
  # PHC, BANIYAPUR:               primary health centre in Saran District of Bihar (n = 62)
  # Sadar Hospital Muzaffarpur:   not described in publication (n = 3)
  # SAMASTIPUR:                   district hospital in Samastipur District of Bihar (n = 40)
  # PHC,Paroo:                    primary health centre in Muzaffarpur (n = 49)

ads_model %>% filter(STUDYID == "VLZUKHR") %>% count() # n = 600 (n = 646 in the publication)

ads_model %>% count(STUDYID, DM_SITE, CLUSTER) %>% print(n = Inf)
# models...

site_uni <- glmer(
  OUT_DC_RELAPSE ~ (1 | CLUSTER),
  family = binomial(),
  data = ads_model
  )
summary(site_uni)

site_multi <- glmer(
  OUT_DC_RELAPSE ~ DM_SEX + DM_AGEs + I(DM_AGEs^2) + MB_COMBINED + TREAT_GRP3 + (1 | CLUSTER),
  family = binomial(),
  data = ads_model
  )
summary(site_multi)
