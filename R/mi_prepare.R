#######################
## PREPARE VARIABLES ##
#######################

ads_clean <- readRDS(file = "data/ads_clean.rds")

small_clusters <- ads_clean %>% 
  group_by(STUDYID) %>% 
  summarise(n = n()) %>% 
  filter(n <= 0) %>% 
  pull(STUDYID) 

ads_model <- ads_clean %>% 
  filter(
    MB_COMBINED != 0 | is.na(MB_COMBINED), # must have positive parasitology (if parasitology is present)
    !(STUDYID %in% small_clusters),
    !OUT_DC_OTHER)     # remove patients who were lost to follow-up

ads_model <- ads_model %>% 
  mutate(
    STUDYID = as.integer(factor(STUDYID)),             # cluster level variable must be integer
    VL_HISTORY = as.integer(VL_HISTORY),               # binary variables must be factors
    DM_SEX = as.integer(ifelse(DM_SEX == "M", 1, 0)),
    LB_BL_HGBs = scale(LB_BL_HGB)[, 1],
    LB_BL_ALTs = scale(log(LB_BL_ALT))[, 1],
    LB_BL_WBCs = scale(log(LB_BL_WBC))[, 1], 
    LB_BL_PLATs = scale(log(LB_BL_PLAT))[, 1],
    LB_BL_CREATs = scale(log(LB_BL_CREAT))[, 1],
    MP_BL_SPLEEN_LENGTHs2 = log(MP_BL_SPLEEN_LENGTH + 1),
    DM_AGEs = scale(DM_AGE)[, 1],
    ZZ_AGEs2 = DM_AGEs^2,
    ZZ_AGEs3 = DM_AGEs^3,
    MB_COMBINED = as.integer(MB_COMBINED),  # must be an integer if treating as Poisson
    MB_COMBINEDs = MB_COMBINED - 1,
    VL_DURATIONs = scale(log(VL_DURATION))[, 1],
    VS_BL_WEIGHTs = scale(VS_BL_WEIGHT)[, 1],
    VS_BL_HEIGHTs = scale(VS_BL_HEIGHT)[, 1],
    VS_BMI = VS_BL_WEIGHT / ((VS_BL_HEIGHT / 100)^2),
    ZZ_BMIs = scale(VS_BMI)[, 1],
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

scale <- data.frame(
  var = c("LB_BL_HGB", "LB_BL_ALT", "LB_BL_WBC", "LB_BL_PLAT", "LB_BL_CREAT", "DM_AGE", "VL_DURATIONs", "VS_BL_WEIGHT", "VS_BL_HEIGHT", "VS_BMI"),
  log_scale = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE),
  means = c(mean(ads_model$LB_BL_HGB, na.rm = TRUE), mean(log(ads_model$LB_BL_ALT), na.rm = TRUE), mean(log(ads_model$LB_BL_WBC), na.rm = TRUE), mean(log(ads_model$LB_BL_PLAT), na.rm = TRUE), mean(log(ads_model$LB_BL_CREAT), na.rm = TRUE), mean(ads_model$DM_AGE, na.rm = TRUE), mean(log(ads_model$VL_DURATION), na.rm = TRUE), mean(ads_model$VS_BL_WEIGHT, na.rm = TRUE), mean(ads_model$VS_BL_HEIGHT, na.rm = TRUE), mean(ads_model$VS_BMI, na.rm = TRUE)),
  sd = c(sd(ads_model$LB_BL_HGB, na.rm = TRUE), sd(log(ads_model$LB_BL_ALT), na.rm = TRUE), sd(log(ads_model$LB_BL_WBC), na.rm = TRUE), sd(log(ads_model$LB_BL_PLAT), na.rm = TRUE), sd(log(ads_model$LB_BL_CREAT), na.rm = TRUE), sd(ads_model$DM_AGE, na.rm = TRUE), sd(log(ads_model$VL_DURATION), na.rm = TRUE), sd(ads_model$VS_BL_WEIGHT, na.rm = TRUE), sd(ads_model$VS_BL_HEIGHT, na.rm = TRUE), sd(ads_model$VS_BMI, na.rm = TRUE))
  )

ads_impute <- ads_model %>% 
    select(
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

saveRDS(ads_impute, file = "data/ads_impute.rds")
saveRDS(scale, file = "data/ads_impute_scale.rds")
