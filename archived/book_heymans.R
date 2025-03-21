# https://bookdown.org/mwheymans/bookmi/

#######

rm(list = ls())
library(mice)
library(tidyverse)
library(haven)
library(VIM)

load("Analysis/ads_clean.RData")

ads_model <- ads_clean %>% 
  mutate(
    STUDYID = as.integer(factor(STUDYID)),   # cluster level variable must be integer
    VL_HISTORY = factor(VL_HISTORY),         # binary variables must be factors
    TREAT_GRP3 = factor(TREAT_GRP3),
    DM_SEX = factor(DM_SEX),
    LB_BL_HGBs = (LB_BL_HGB - mean(LB_BL_HGB, na.rm = TRUE)) / 10,
    LB_BL_ALTs = log(LB_BL_ALT) - mean(log(LB_BL_ALT), na.rm = TRUE),
    LB_BL_WBCs = log(LB_BL_WBC) - mean(log(LB_BL_WBC), na.rm = TRUE), 
    LB_BL_PLATs = log(LB_BL_PLAT) - mean(log(LB_BL_PLAT), na.rm = TRUE),
    LB_BL_CREATs = log(LB_BL_CREAT) - mean(log(LB_BL_CREAT), na.rm = TRUE),
    LB_IC_HGBs = (LB_IC_HGB - mean(LB_IC_HGB, na.rm = TRUE)) / 10,
    LB_IC_ALTs = log(LB_IC_ALT) - mean(log(LB_IC_ALT), na.rm = TRUE),
    LB_IC_WBCs = log(LB_IC_WBC) - mean(log(LB_IC_WBC), na.rm = TRUE), 
    LB_IC_PLATs = log(LB_IC_PLAT) - mean(log(LB_IC_PLAT), na.rm = TRUE),
    LB_IC_CREATs = log(LB_IC_CREAT) - mean(log(LB_IC_CREAT), na.rm = TRUE),
    MP_BL_SPLEEN_LENGTHs = log(MP_BL_SPLEEN_LENGTH) - mean(log(MP_BL_SPLEEN_LENGTH), na.rm = TRUE), 
    MP_IC_SPLEEN_LENGTHs = log(MP_IC_SPLEEN_LENGTH) - mean(log(MP_IC_SPLEEN_LENGTH), na.rm = TRUE), 
    DM_AGEs = (DM_AGE - mean(DM_AGE, na.rm = TRUE)) / 10,
    MB_COMBINED = as.integer(MB_COMBINED),  # must be an integer if treating as Poisson
    VL_DURATIONs = log(VL_DURATION) - mean(log(VL_DURATION), na.rm = TRUE),
    VS_BL_WEIGHTs = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
    VS_BL_HEIGHTs = (VS_BL_HEIGHT - mean(VS_BL_HEIGHT, na.rm = TRUE)) / 10,
    VS_IC_WEIGHTs = (VS_IC_WEIGHT - mean(VS_IC_WEIGHT, na.rm = TRUE)) / 10
  ) %>% 
  select(
    STUDYID,
    VL_HISTORY,
    TREAT_GRP3,
    DM_SEX,
    LB_BL_HGBs,
    LB_BL_ALTs,
    LB_BL_WBCs,
    LB_BL_PLATs,
    LB_BL_CREATs,
    LB_IC_HGBs,
    LB_IC_ALTs,
    LB_IC_WBCs,
    LB_IC_PLATs,
    LB_IC_CREATs,
    MP_BL_SPLEEN_LENGTHs,   
    MP_IC_SPLEEN_LENGTHs,
    DM_AGEs,
    DM_SEX,
    MB_COMBINED,
    VL_DURATIONs,
    VS_BL_WEIGHTs,
    VS_BL_HEIGHTs,
    VS_IC_WEIGHTs,
    OUT_DC_RELAPSE
  )

# missing data visualisation
pdf("Analysis/MI/missing_data.pdf", width = 5, height = 10)
md.pattern(ads_model %>% select(starts_with("LB_")), rotate.names = TRUE)
dev.off()

aggr(
  ads_model %>% select(starts_with("LB_BL")), 
  col = c('white', 'red'), 
  numbers = TRUE, 
  sortVars = TRUE, 
  cex.axis = .7, 
  gap = 3, 
  ylab = c("Percentage of missing data", "Missing Data Pattern"))
