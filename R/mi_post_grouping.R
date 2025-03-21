##############################
## POST IMPUTATION GROUPING ##
##############################

mi_post_grouping <- function(data) {
  
  if (is.null(data$mice$result)) {
  cat("\nMICE FAILED\n") 
  return(NULL)
  }

  long <- complete(data$mice$result, action = "long", include = TRUE)
  df_s <- data$df_scaled_values

  long <- long %>% 
    mutate(
      DM_AGE    = DM_AGEs     * df_s[df_s$var == "DM_AGE", "sd"] + df_s[df_s$var == "DM_AGE", "means"],
      LB_BL_HGB = LB_BL_HGBs  * df_s[df_s$var == "LB_BL_HGB", "sd"] + df_s[df_s$var == "LB_BL_HGB", "means"],
      ZZ_BMI    = ZZ_BMIs     * df_s[df_s$var == "VS_BMI", "sd"] + df_s[df_s$var == "VS_BMI", "means"],
    )

  ## create BMI severity categories

  # for BMI-z scores, defined for age < 18

  # (-Inf, -3]    severe
  # (-3, -2]      moderate
  # (-2, Inf)     mild/normal

  long <- long %>% 
    mutate(
      ZZ_BMI_Z_GRP = case_when(
        DM_AGE < 18 & ZZ_BMI_Z > -2 & !is.na(ZZ_BMI_Z)                  ~ "Mild_norm",
        DM_AGE < 18 & ZZ_BMI_Z <= -2 & ZZ_BMI_Z > -3 & !is.na(ZZ_BMI_Z) ~ "Moderate",
        DM_AGE < 18 & ZZ_BMI_Z <= -3 & !is.na(ZZ_BMI_Z)                 ~ "Severe",
        .default = "Not defined"
      ),
      ZZ_BMI_Z_GRP = factor(ZZ_BMI_Z_GRP, levels = c("Mild_norm", "Moderate", "Severe"))
    )

  # for BMI, defined for age > 18

  # (0, 16)        severe
  # [16, 18.5)     moderate
  # [18.5, Inf)    mild/normal

  long <- long %>% 
    mutate(
      ZZ_BMIs_GRP = case_when(
        DM_AGE >= 18 & ZZ_BMI >= 18.5 & !is.na(ZZ_BMIs)                   ~ "Mild_norm",
        DM_AGE >= 18 & ZZ_BMI < 18.5  & ZZ_BMI >= 16 & !is.na(ZZ_BMIs)    ~ "Moderate",
        DM_AGE >= 18 & ZZ_BMI < 16    & !is.na(ZZ_BMIs)                   ~ "Severe",
        .default = "Not defined"
      ),
      ZZ_BMIs_GRP = factor(ZZ_BMIs_GRP, levels = c("Mild_norm", "Moderate", "Severe"))
    )

  # combine BMI severity
  long <- long %>% 
    mutate(
      ZZ_MAL = case_when(
        DM_AGE >= 18 ~ ZZ_BMIs_GRP,
        DM_AGE < 18  ~ ZZ_BMI_Z_GRP,
        .default = NA
      )
    )

  # Guideline on haemoglobin cutoffs to define anaemia in individuals and populations, WHO March 2024
  # See Table 3 on page xii (values below are excluding pregnant women)

  # Children, 6 - 23 months

  # (0, 70)    severe anaemia
  # [70, 95)   moderate anaemia
  # [95, 105)  mild anaemia
  # [105, Inf) no anaemia  

  # Children, 24 - 59 months

  # (0, 70)    severe anaemia
  # [70, 100)   moderate anaemia
  # [100, 110)  mild anaemia
  # [110, Inf) no anaemia 

  # Children, 5 - 11 years

  # (0, 80)      severe anaemia
  # [80, 110)    moderate anaemia
  # [110, 115)   mild anaemia
  # [115, Inf)   no anaemia 

  # Children, 12 - 14 years AND female adults 15 - 65 years

  # (0, 80)      severe anaemia
  # [80, 110)    moderate anaemia
  # [110, 120)   mild anaemia
  # [120, Inf)   no anaemia 

  # Adults 15 - 65 years

  # (0, 80)      severe anaemia
  # [80, 110)    moderate anaemia
  # [110, 130)   mild anaemia
  # [130, Inf)   no anaemia 

  long <- long %>% 
    mutate(
      LB_BL_HGB_GRP1 = case_when(
        DM_AGE < 2 & LB_BL_HGB < 70                                       ~ "Severe",
        DM_AGE < 2 & LB_BL_HGB >= 70 & LB_BL_HGB < 95                     ~ "Moderate",
        DM_AGE < 2 & LB_BL_HGB >= 95 & LB_BL_HGB < 105                    ~ "Mild",
        DM_AGE < 2 & LB_BL_HGB >= 105                                     ~ "No",
        DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB < 70                         ~ "Severe",
        DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB >= 70 & LB_BL_HGB < 100      ~ "Moderate",
        DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB >= 100 & LB_BL_HGB < 110     ~ "Mild",
        DM_AGE >= 2 & DM_AGE < 5 & LB_BL_HGB >= 110                       ~ "No",
        DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB < 80                        ~ "Severe",
        DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB >= 80 & LB_BL_HGB < 110     ~ "Moderate",
        DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB >= 110 & LB_BL_HGB < 115    ~ "Mild",
        DM_AGE >= 5 & DM_AGE < 12 & LB_BL_HGB >= 115                      ~ "No",      
        DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB < 80                       ~ "Severe",
        DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB >= 80 & LB_BL_HGB < 110    ~ "Moderate",
        DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB >= 110 & LB_BL_HGB < 120   ~ "Mild",
        DM_AGE >= 12 & DM_AGE < 15 & LB_BL_HGB >= 120                     ~ "No",     
        DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB < 80                     ~ "Severe",
        DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB >= 80 & LB_BL_HGB < 110  ~ "Moderate",
        DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB >= 110 & LB_BL_HGB < 130 ~ "Mild",
        DM_SEX == 1 & DM_AGE >= 15 & LB_BL_HGB >= 130                   ~ "No",    
        DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB < 80                     ~ "Severe",
        DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB >= 80 & LB_BL_HGB < 110  ~ "Moderate",
        DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB >= 110 & LB_BL_HGB < 120 ~ "Mild",
        DM_SEX == 0 & DM_AGE >= 15 & LB_BL_HGB >= 120                   ~ "No",  
        .default = NA  
      ),
      LB_BL_HGB_GRP2 = case_when(
        LB_BL_HGB_GRP1 %in% c("No", "Mild") ~ "Mild_norm",
        is.na(LB_BL_HGB_GRP1) ~ NA,
        .default = LB_BL_HGB_GRP1
      ),
      LB_BL_HGB_GRP3 = case_when(
        LB_BL_HGB_GRP1 %in% c("Severe") ~ 1, 
        LB_BL_HGB_GRP1 %in% c("Moderate", "Mild", "No") ~ 0,
        .default = NA
      )
    )

  long <- long %>% mutate(
    LB_BL_HGB_GRP2 = factor(LB_BL_HGB_GRP2, levels = c("Severe", "Moderate", "Mild_norm"))
  ) 


  #long %>% count(.imp, LB_BL_HGB_GRP1) # only about 50 patients per imputation with no anaemia

  #levels(long$LB_BL_HGB_GRP2)
  #levels(long$ZZ_MAL)
  #levels(long$TREAT_GRP4)

  # long %>% count(DM_SEX, DM_AGE, LB_BL_HGB, LB_BL_HGB_GRP) %>% View() 
  # long %>% group_by(LB_BL_HGB_GRP2) %>% summarise(n = n(), pct = 100 * n / nrow(long))
  # long %>% count(LB_BL_HGB_GRP2, .imp)
  # long %>% count(LB_BL_HGB_GRP1, LB_BL_HGB_GRP2)

  # create variables compatible with model to allow easy extraction of predictions 
  long <- long %>% 
    mutate(
      INT = 1,
      TREAT_GRP4MF = as.integer(TREAT_GRP4 == "MF"),
      TREAT_GRP4SDA = as.integer(TREAT_GRP4 == "SDA"),
      TREAT_GRP4OTHER = as.integer(TREAT_GRP4 == "OTHER"),
      LB_BL_HGB_GRP2Severe = as.integer(LB_BL_HGB_GRP2 == "Severe"),
      LB_BL_HGB_GRP2Moderate = as.integer(LB_BL_HGB_GRP2 == "Moderate"),
      LB_BL_HGB_GRP2Mild_norm = as.integer(LB_BL_HGB_GRP2 == "Mild_norm"),
      ZZ_MALMild_norm = as.integer(ZZ_MAL == "Mild_norm"),
      ZZ_MALModerate = as.integer(ZZ_MAL == "Moderate"),
      ZZ_MALSevere = as.integer(ZZ_MAL == "Severe")
    )
  # convert back
  if (any(sapply(long[long$.imp != 0, !names(long) %in% c("ZZ_BMIs_GRP", "ZZ_BMI_Z_GRP")], function(x) sum(is.na(x)))) > 0) {
    warning("NA values created")
  } # this is for debugging

  return(as.mids(long))
}
