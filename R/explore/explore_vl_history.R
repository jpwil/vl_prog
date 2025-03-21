################
## VL_HISTORY ##
################

rm(list = ls())
library(tidyverse)
library(lme4)
library(micemd)
load("Analysis/ads_clean.RData")
source("definitions.R")

ads_clean %>% names() %>% sort()
ads_clean %>% var_sum(VL_HISTORY)
ads_clean %>% var_sum(VL_HISTORY, STUDYID)

model_vlh1 <- glm(
    OUT_DC_RELAPSE ~ VL_HISTORY,
    family = binomial(),
    data = ads_clean
    )
summary(model_vlh1) 
model_est(model_vlh1)
model_vlh1 %>% fitted.values() %>% table()
model_vlh1 %>% coefficients() %>% exp()

model_vlh2 <- glmer(
    OUT_DC_RELAPSE ~ VL_HISTORY + (1 | STUDYID),
    family = binomial(),
    data = ads_clean
    )
summary(model_vlh2) 

# where measured, 11.0% of cases have previous VL history
# this ranges from 0% to 18.4% across studies

# no significant baseline association between VL history and relapse risk (in both fixed and random effects model)


# is vl relapse associated with higher parasitaemia?

model_vlh3 <- glmer(
    VL_HISTORY ~ MB_COMBINED + (1 | STUDYID),
    family = binomial(),
    data = ads_clean
    )
summary(model_vlh3) 

model_vlh3 <- glmer(
    OUT_DC_RELAPSE ~ VL_HISTORY  + MB_COMBINED + (1 | STUDYID),
    family = binomial(),
    data = ads_clean
    )
summary(model_vlh3) 

ads_clean %>% 
  filter(!is.na(VL_HISTORY)) %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_COMBINED,
      fill = factor(VL_HISTORY)
    ),
    position = "fill"
  )