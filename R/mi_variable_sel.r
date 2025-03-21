########################
## VARIABLE SELECTION ##
########################

library(lme4)
library(mice)
library(broom.mixed)

# source("Analysis/MI/select_wald.R")
# source("Analysis/MI/select_aic.R")
# source("Analysis/MI/select_lrt.R")
source("Analysis/MI/select_rr.r")

####################
# try Rubins Rules #
####################

out1 <- select_rr(mice2, keep = NULL)
out2 <- select_rr(mice2, keep = c("DM_AGEs", "ZZ_AGEs2", "DM_SEX"))

out1

# fit final model
fit1 <- with(
  mice2,
  glmer(
    OUT_DC_RELAPSE ~ DM_AGEs + ZZ_AGEs2 + VL_HISTORY + VL_DURATIONs + MB_COMBINEDs + TREAT_GRP4 + LB_BL_HGB_GRP2 + (1 | STUDYID),
    family = binomial
  )
)

fit2 <- with(
  mice2,
  glmer(
    OUT_DC_RELAPSE ~ DM_AGEs + ZZ_AGEs2 + VL_HISTORY + VL_DURATIONs + MB_COMBINEDs + TREAT_GRP4 + LB_BL_HGB_GRP2 + (1 | STUDYID),
    family = binomial
  )
)

pool(fit2) %>% summary()  %>% View()


fit2 <- with(
    mice1,
    glmer(
    OUT_DC_RELAPSE ~  DM_SEX + DM_AGEs + ZZ_AGEs2 + VL_DURATIONs + MP_BL_SPLEEN_LENGTHs2 + MB_COMBINEDs + 
                      TREAT_GRP4 + (1 | STUDYID),
    family = binomial
  ))

pool(fit2) %>% summary() %>% as_tibble()
pool(fit2) %>% summary() %>% plot_model()

# DM_SEX
# DM_AGEs
# ZZ_AGEs2
# VL_DURATIONs
# MP_BL_SPLEEN_LENGTHs2
# MB_COMBINEDs
# TREAT_GRP4

######################
# try stacked method #
######################

## In Austin et al 2019, this is described as 'Stacked Imputed Datasets With Weighted Regressions (W1, W2, and W3)'
## W1: w = 1/M  
## W2: w = (1-f)/M
## W3: w_j = (1-f_j)/M
## where M is the number of imputations, and f is the proportion of missing data (either overall or at the variable j level)

mice3_not_include <- mice2 %>% complete(action = "long", include = FALSE)
mice3_include <- mice2 %>% complete(action = "long", include = TRUE)

na_fraction <- mice3_include %>% filter(.imp == 0) %>% select(-c(.imp, .id)) %>% unlist() 
f <- sum(is.na(na_fraction)) / length(na_fraction) # 25.3% missing data

mice3_stacked <- mice3_not_include %>% 
  mutate(
    weights = (1 - f) / 40
  )

# isSingular warning (even without the TREAT_GRP4 term)
# model <- glmer(
#   as.integer(OUT_DC_RELAPSE) ~ 
#     DM_SEX + DM_AGEs + ZZ_AGEs2 + ZZ_AGEs3 + ZZ_MAL + VL_HISTORY + VL_DURATIONs + LB_BL_HGB_GRP2 + LB_BL_WBCs + LB_BL_PLATs + LB_BL_ALTs + LB_BL_CREATs + MP_BL_SPLEEN_LENGTHs2 + MB_COMBINEDs + (1 | STUDYID),
#   data = mice3_stacked,
#   family = binomial(),
#   weights = weights
#     )

# model %>% summary()
# fit5 <- select_chi2(model, keep = c("DM_SEX", "TREAT_GRP4", "DM_AGEs"))
# fit5$end_model %>% summary()