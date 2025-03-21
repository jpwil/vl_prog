#######################
## PREPARE VARIABLES ##
#######################

#source("Analysis/clean.R")
load("Analysis/ads_clean.RData")

small_clusters <- ads_clean %>% group_by(STUDYID) %>% summarise(n = n()) %>% filter(n <= 0) %>% pull(STUDYID) 
kamrc_clusters <- c("VEZMZD", "VFEFCS", "VIVXJN", "VIZGFA", "VLAULV", "VQKRHN", "VRBQIF", "VSGPDL", "VVNGOE", "VWPJRM", "VYDSGR", "VFFFOP")

ads_model <- ads_clean %>% 
  filter(
    MB_COMBINED != 0 | is.na(MB_COMBINED),
    (STUDYID %in% kamrc_clusters))

ads_model <- ads_model %>% 
  mutate(
    STUDYID = as.integer(factor(STUDYID)),    # cluster level variable must be integer
    VL_HISTORY = as.integer(VL_HISTORY),             # binary variables must be factors
    DM_SEX = as.integer(ifelse(DM_SEX == "M", 1, 0)),
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
    #MP_BL_SPLEEN_LENGTHs = log(MP_BL_SPLEEN_LENGTH) - mean(log(MP_BL_SPLEEN_LENGTH), na.rm = TRUE), # log(0) = -Inf
    MP_BL_SPLEEN_ROUND = as.integer(round(MP_BL_SPLEEN_LENGTH)),
    MP_BL_SPLEEN_LENGTHs1 = MP_BL_SPLEEN_LENGTH - mean(MP_BL_SPLEEN_LENGTH, na.rm = TRUE),
    MP_BL_SPLEEN_LENGTHs2 = log(MP_BL_SPLEEN_LENGTH + 1),
    DM_AGEs = (DM_AGE - mean(DM_AGE, na.rm = TRUE)) / 10,
    ZZ_AGEs2 = DM_AGEs^2,
    ZZ_AGEs3 = DM_AGEs^3,
    MB_COMBINED = as.integer(MB_COMBINED),  # must be an integer if treating as Poisson
    MB_COMBINEDs = MB_COMBINED - 1,
    VL_DURATIONs = log(VL_DURATION) - mean(log(VL_DURATION), na.rm = TRUE),
    VS_BL_WEIGHTs = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
    VS_BL_HEIGHTs = (VS_BL_HEIGHT - mean(VS_BL_HEIGHT, na.rm = TRUE)) / 10,
    VS_IC_WEIGHTs = (VS_IC_WEIGHT - mean(VS_IC_WEIGHT, na.rm = TRUE)) / 10,
    VS_BMI = VS_BL_WEIGHT / ((VS_BL_HEIGHT / 100)^2),
    ZZ_BMIs = (VS_BMI - mean(VS_BMI, na.rm = TRUE)) / 10,
    VS_BMI_Z_u5 = 
      anthro_zscores(
        sex = 2 - DM_SEX,
        age = if_else(DM_AGE == 5, 59, DM_AGE * 12), # otherwise no BMI z-score for 60 month old!
        is_age_in_month = TRUE,
        weight = VS_BL_WEIGHT,
        lenhei = VS_BL_HEIGHT)$zbmi,
    VS_BMI_Z_o5 = 
      anthroplus_zscores(
        sex = 2 - DM_SEX,
        age = if_else(DM_AGE * 12 > 228, 228, DM_AGE * 12),
        weight_in_kg = VS_BL_WEIGHT,
        height_in_cm = VS_BL_HEIGHT
      )$zbfa) %>% 
  mutate(
    ZZ_BMI_Z = if_else(
      DM_AGE * 12 <= 60, 
      VS_BMI_Z_u5, 
      VS_BMI_Z_o5),
    TREAT_GRP4 = case_when(
      TREAT_GRP3 == "MF" ~ "MF",
      TREAT_GRP3 == "SDA" ~ "SDA",
      .default = "OTHER")) %>% 
  mutate(
    TREAT_GRP4 = factor(TREAT_GRP4, levels = c("MF", "SDA", "OTHER")))

ads_impute <- ads_model %>% 
    dplyr::select(
      STUDYID,
      DM_SEX,
      DM_AGEs,
      VS_BL_WEIGHTs,
      VS_BL_HEIGHTs,
      VL_HISTORY,
      VL_DURATIONs,
      TREAT_GRP4,
      LB_BL_HGBs,
      LB_BL_ALTs,
      LB_BL_WBCs,
      LB_BL_PLATs,
      LB_BL_CREATs,
      MP_BL_SPLEEN_LENGTHs2,
      MB_COMBINEDs,
      OUT_DC_RELAPSE,
      ZZ_BMIs,
      ZZ_BMI_Z,
      ZZ_AGEs2,
      ZZ_AGEs3
  )
