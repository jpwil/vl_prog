################
# correlograms #
################

rm(list = ls())
library(lme4)
library(micemd)
library(tidyverse)
library(GGally)
source("R/definitions.R")

#source("Analysis/clean.R")
ads_clean <- readRDS("data/ads_clean.rds")
ads_clean %>% names()

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
  )

ads_model %>% names() %>% sort()

ads_corr_bl_labs <- ads_model %>% 
  dplyr::select(
    OUT_DC_RELAPSE,
    VS_BL_WEIGHTs,
    VS_IC_WEIGHTs,
    VS_BL_HEIGHTs,
    DM_SEX,
    DM_AGEs
  )

ads_corr_bl_labs <- ads_corr_bl_labs %>% ggpairs()
ggsave(
  filename = "results/correlation/bl_labs.pdf", 
  plot = ads_corr_bl_labs, 
  width = 10,
  height = 10
  )

df_scaled <- readRDS(file = "data/ads_impute.rds")
ads_corr_model_labs <- df_scaled %>% 
  dplyr::select(
    OUT_DC_RELAPSE,
    DM_SEX,
    DM_AGEs,
    LB_BL_HGBs,
    ZZ_BMIs,
    MP_BL_SPLEEN_LENGTHs2,
    TREAT_GRP4,
    VL_DURATIONs,
    MB_COMBINEDs,
    VL_HISTORY
)

ads_corr_model_labs <- ads_corr_model_labs %>% ggpairs()
ggsave(
  filename = "results/correlation/model_labs.pdf", 
  plot = ads_corr_model_labs, 
  width = 10,
  height = 10
  )

ads_corr_model_labs <- ads_corr_model_labs %>% ggpairs()
ggsave(
  filename = "results/correlation/model_labs.pdf", 
  plot = ads_corr_model_labs, 
  width = 10,
  height = 10
  )

ads_corr_ic_labs <- ads_model %>% 
  select(
    OUT_DC_RELAPSE,
    LB_IC_HGBs,
    LB_IC_WBCs,
    LB_IC_PLATs,
    LB_IC_ALTs,
    LB_IC_CREATs
  )

ads_corr_ic_labs <- ads_corr_ic_labs %>% ggpairs()
ggsave(
  filename = "Analysis/correlation/ic_labs.pdf", 
  plot = ads_corr_ic_labs, 
  width = 10,
  height = 10
  )

ads_corr_all_labs <- ads_model %>% 
  select(
    LB_IC_HGBs,
    LB_IC_WBCs,
    LB_IC_PLATs,
    LB_IC_ALTs,
    LB_IC_CREATs,
    LB_BL_HGBs,
    LB_BL_WBCs,
    LB_BL_PLATs,
    LB_BL_ALTs,
    LB_BL_CREATs
  )

ads_corr_all_labs <- ads_corr_all_labs %>% ggpairs()
ggsave(
  filename = "Analysis/correlation/all_labs.pdf", 
  plot = ads_corr_all_labs, 
  width = 10,
  height = 10
  )
