###################
## INITIALISE MI ##
###################

#library(micemd)
scale <- readRDS(file = "data/ads_impute_scale.rds")
ads_impute <- readRDS(file = "data/ads_impute.rds")

## PREDICTOR MATRIX

mice_init <- mice(data = ads_impute, maxit = 0)
predictor.matrix <- mice_init$predictorMatrix

# define cluster variable ('class' variable as described in mice help file)
predictor.matrix[, "STUDYID"] <- -2
predictor.matrix[c("MB_COMBINEDs"), "TREAT_GRP4"] <- 1
predictor.matrix[c("MB_COMBINEDs"), c("ZZ_BMIs", "ZZ_BMI_Z", "ZZ_AGEs2", "ZZ_AGEs3")] <- 1
predictor.matrix[c("MB_COMBINEDs"), "STUDYID"] <- 0 # remove clustering from MB_COMBINED

# adjust for passive imputation to avoid convergence issues / loops
predictor.matrix[c("VS_BL_WEIGHTs", "VS_BL_HEIGHTs"), c("ZZ_BMIs", "ZZ_BMI_Z")] <- 0
predictor.matrix[c("DM_AGEs"), c("ZZ_AGEs2", "ZZ_AGEs3")] <- 0

# simplify MP_BL_SPLEEN_LENGTHs and VL_HISTORY imputation models to prevent from crashing
predictor.matrix[c("VL_HISTORY"), c("ZZ_AGEs2", "ZZ_AGEs3", "ZZ_BMIs", "ZZ_BMI_Z")] <- 0

##  POST IMPUTATION ADJUSTMENTS (prevent anthro functions crashing)
post_age    <- expression(imp[[j]][, i] <- squeeze(imp[[j]][, i], (c(1, 80)   - df_scaled_values[df_scaled_values$var == "DM_AGE", "means"])       / df_scaled_values[df_scaled_values$var == "DM_AGE", "sd"]))
post_weight <- expression(imp[[j]][, i] <- squeeze(imp[[j]][, i], (c(1, 100)  - df_scaled_values[df_scaled_values$var == "VS_BL_WEIGHT", "means"]) / df_scaled_values[df_scaled_values$var == "VS_BL_WEIGHT", "sd"]))
post_height <- expression(imp[[j]][, i] <- squeeze(imp[[j]][, i], (c(40, 200) - df_scaled_values[df_scaled_values$var == "VS_BL_HEIGHT", "means"]) / df_scaled_values[df_scaled_values$var == "VS_BL_HEIGHT", "sd"]))

post <- mice_init$post
post["DM_AGEs"]       <- paste(post_age)
post["VS_BL_WEIGHTs"] <- paste(post_weight)
post["VS_BL_HEIGHTs"] <- paste(post_height)

## INITIALISE METHODS

# passive imputation: expression for BMI
expr_bmi <- expression({
  VS_BL_WEIGHT <- df_scaled_values[df_scaled_values$var == "VS_BL_WEIGHT", "sd"] * VS_BL_WEIGHTs + df_scaled_values[df_scaled_values$var == "VS_BL_WEIGHT", "means"]
  VS_BL_HEIGHT <- df_scaled_values[df_scaled_values$var == "VS_BL_HEIGHT", "sd"] * VS_BL_HEIGHTs + df_scaled_values[df_scaled_values$var == "VS_BL_HEIGHT", "means"]
  VS_BMIs <- (VS_BL_WEIGHT / ((VS_BL_HEIGHT) / 100)^2 - df_scaled_values[df_scaled_values$var == "VS_BMI", "means"]) / df_scaled_values[df_scaled_values$var == "VS_BMI", "sd"]
  VS_BMIs
})

# passive imputation: expression for BMI_z score
expr_bmi_z <- expression({
  #print("started running expr_bmi_z")
  DM_AGE <-       df_scaled_values[df_scaled_values$var == "DM_AGE", "sd"]       * DM_AGEs       + df_scaled_values[df_scaled_values$var == "DM_AGE", "means"]
  VS_BL_WEIGHT <- df_scaled_values[df_scaled_values$var == "VS_BL_WEIGHT", "sd"] * VS_BL_WEIGHTs + df_scaled_values[df_scaled_values$var == "VS_BL_WEIGHT", "means"]
  VS_BL_HEIGHT <- df_scaled_values[df_scaled_values$var == "VS_BL_HEIGHT", "sd"] * VS_BL_HEIGHTs + df_scaled_values[df_scaled_values$var == "VS_BL_HEIGHT", "means"]

# BMI-for-age z-score for under 5
  VS_BMI_Z_u5 <- anthro_zscores(
    sex = 2 - DM_SEX,
    age = if_else(DM_AGE == 5, 59, DM_AGE * 12), # needs this line because no BMI z-score for 60 month old!
    is_age_in_month = TRUE,
    weight = VS_BL_WEIGHT,
    lenhei = VS_BL_HEIGHT
    )$zbmi  

# BMI-for-age z-score for 5 - 18
  VS_BMI_Z_o5 <- anthroplus_zscores(
    sex = 2 - DM_SEX,
    age = if_else(
      DM_AGE * 12 > 228, 228, 
      if_else(DM_AGE >= 5 & DM_AGE < 5.1, 5.1 * 12, DM_AGE * 12)),  # blind spot when age is between 5 and 5.1 years
    weight_in_kg = VS_BL_WEIGHT,
    height_in_cm = VS_BL_HEIGHT
    )$zbfa

  temp <- data.frame(
    "DM_AGE" = DM_AGE,
    "VS_BMI_Z_u5" = VS_BMI_Z_u5,
    "VS_BMI_Z_o5" = VS_BMI_Z_o5
  )
  
  temp <- temp %>% 
    mutate(
      ZZ_BMI_Z <- if_else(
        DM_AGE * 12 <= 60, 
        VS_BMI_Z_u5, 
        VS_BMI_Z_o5
    )
  )
  temp$ZZ_BMI_Z # this is the expression output
})

method <- mice_init$method

method[c(
    "DM_AGEs", "VS_BL_WEIGHTs", "VS_BL_HEIGHTs", "VL_DURATIONs", 
    "LB_BL_HGBs", "LB_BL_ALTs", "LB_BL_WBCs", "LB_BL_PLATs", "LB_BL_CREATs")] <- "2l.lmer"

method[c("MP_BL_SPLEEN_LENGTHs2")]  <- "2l.lmer"
method[c("VL_HISTORY")]             <- "2l.bin"

method["ZZ_BMIs"]                   <- paste0("~I(", expr_bmi, ")")
method["ZZ_BMI_Z"]                  <- paste0("~I(", expr_bmi_z, ")")
method["ZZ_AGEs2"]                  <- paste0("~I(DM_AGEs^2)")
method["ZZ_AGEs3"]                  <- paste0("~I(DM_AGEs^3)")

# mice.impute.2l.2stage.pois.reml <- function(y, ry, x, type, wy = NULL, ...) {
#   micemd:::mice.impute.2l.2stage.pois(y, ry, x, type, wy, method_est = "reml", ...)
# }

method[c("MB_COMBINEDs")] <- "pois" # we are ignoring clustering in the parasite grade

saveRDS(method, "data/mi_method.rds")
saveRDS(predictor.matrix, "data/mi_pm.rds")
saveRDS(post, "data/mi_post.rds")

