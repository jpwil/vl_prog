#########
## SEX ##
#########

rm(list = ls())
library(tidyverse)
library(lme4)
load("Analysis/ads_clean.RData")
source("definitions.R")

ads_model <- ads_clean %>% 
  mutate(
    DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10,
    DM_SEX = ifelse(DM_SEX == "M", 1, ifelse(DM_SEX == "F", 0, NA))
  )

ads_model %>% var_sum(DM_SEX)
ads_model %>% var_sum(DM_SEX, STUDYID) %>% arrange(mean)

# no missing data for sex
# 59.6% are male
# across studies, mean proportion of male ranges from 50% to 76.7%

# models ----

model_sex_f1 <- glm(
    OUT_DC_RELAPSE ~ DM_SEX,
    family = binomial(),
    data = ads_model
    )
summary(model_sex_f1)
AIC(model_sex_f1)

model_sex_f2 <- glm(
    DM_SEX ~ MB_COMBINED,
    family = binomial(),
    data = ads_model
    )
summary(model_sex_f2)

model_sex_f3 <- glm(
    DM_SEX ~ log(VL_DURATION),
    data = ads_model
    )
summary(model_sex_f3)

model_sex_f4 <- glm(
  DM_SEX ~ VL_HISTORY,
  data = ads_model
)
summary(model_sex_f4)

table(ads_model$VL_HISTORY, ads_model$DM_SEX) %>% chisq.test()

model_sex_f5 <- glm(
  DM_SEX ~ MP_BL_SPLEEN_LENGTH,
  data = ads_model
)
summary(model_sex_f5)

