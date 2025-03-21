
select_rr_10_sex <- select_rr(
  data = model_test, 
  keep = c("DM_SEX", "DM_AGEs", "ZZ_AGEs2"), 
  p_select = 0.1,
  predictors_init = 
    c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
    )
  
saveRDS(select_rr_10_sex, "results/select_rr_10_sex.rds")
##

select_rr_157_sex <- select_rr(
  data = model_test, 
  keep = c("DM_SEX", "DM_AGEs", "ZZ_AGEs2"), 
  p_select = 0.157,
  predictors_init = 
    c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
    )
  
saveRDS(select_rr_157_sex, "results/select_rr_157_sex.rds")
##

select_rr_5_sex <- select_rr(
  data = model_test, 
  keep = c("DM_SEX", "DM_AGEs", "ZZ_AGEs2"), 
  p_select = 0.05,
  predictors_init = 
    c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
    )
  
saveRDS(select_rr_5_sex, "results/select_rr_5_sex.rds")
####

select_rr_5_nk <- select_rr(
  data = model_test, 
  keep = NULL, 
  p_select = 0.05,
  predictors_init = 
    c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
    )
  
saveRDS(select_rr_5_nk, "results/select_rr_5_nk.rds")
##

select_rr_157_nk <- select_rr(
  data = model_test, 
  keep = NULL, 
  p_select = 0.157,
  predictors_init = 
    c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
    )
  
saveRDS(select_rr_157_nk, "results/select_rr_157_nk.rds")
##

select_rr_10_nk <- select_rr(
  data = model_test, 
  keep = NULL, 
  p_select = 0.1,
  predictors_init = 
    c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
    )
  
saveRDS(select_rr_10_nk, "results/select_rr_10_nk.rds")
##
