########################
## VARIABLE SELECTION ##
########################

source("Analysis/MI/select_wald.R")
source("Analysis/MI/select_aic.R")
source("Analysis/MI/select_lrt.R")
source("Analysis/MI/select_rr.R")

## In Austin et al 2019, this is described as 'Separate imputations (S1, S2, and S3)'
## S1: variables selected in at least one imputation
## S2: variables selected in at least 50% of imputations
## S3: variables selected in all of imputations

## using Wald test (including multivariate Wald for categorical predictors)

out1 <- select_wald(data = mice2, keep = NULL)
out2 <- select_wald(data = mice2, keep = c("DM_SEX", "DM_AGEs", "ZZ_AGEs2"))

variables <- lapply(out2, function(x) x$term) %>% unlist() %>% as_tibble() %>% count(value)
variables <- variables %>% 
  mutate(
    value = case_when(
      value == "ZZ_AGEs3" ~ "Age^3",
      value == "LB_BL_ALTs" ~ "ALT (log scale)",
      value == "ZZ_MAL" ~ "Malnutrition (group)",
      value == "LB_BL_HGB_GRP2" ~ "Anaemia (group)",
      value == "VL_HISTORY" ~ "History of VL",
      value == "MP_BL_SPLEEN_LENGTHs2" ~ "Spleen length (log scale)",
      value == "TREAT_GRP4" ~ "Treatment (group)",
      value == "DM_AGEs" ~ "Age",
      value == "ZZ_AGEs2" ~ "Age^2",
      value == "VL_DURATIONs" ~ "Duration of fever (log scale)",
      value == "MB_COMBINEDs" ~ "Smear parasite count",
      value == "DM_SEX" ~ "Sex",
      value == "LB_BL_CREATs" ~ "Creatinine (log scale)",
      .default = value
    )
  ) 

add_rows <- tibble(
  value = c("Platelet count (log scale)", "WBC (log scale)", "Creatinine (log scale)"),
  n = c(0, 0, 0)
)

variables <- rbind(variables, add_rows) %>% 
  arrange(n) %>% 
  mutate(value = reorder(value, n)) 

variables %>% ggplot() + 
  geom_bar(
    stat = "identity",
    aes(
      y = value,
      x = n
    )
  ) + 
  theme_minimal()


## using chi2 (likelihood ratio test)

expr <- expression({
  cat("Imputation dataset: ", i, "\n")
  i <<- i + 1
  model <- glmer(
    OUT_DC_RELAPSE ~ 
    DM_SEX + TREAT_GRP4 + DM_AGEs + ZZ_AGEs2 + ZZ_AGEs3 + ZZ_MAL + VL_HISTORY + VL_DURATIONs + LB_BL_HGB_GRP2 + LB_BL_WBCs + LB_BL_PLATs + LB_BL_ALTs + LB_BL_CREATs + MP_BL_SPLEEN_LENGTHs2 + MB_COMBINEDs + (1 | STUDYID),
    family = binomial())

  out <- select_chi2(model, keep = c("DM_SEX", "DM_AGEs", "ZZ_AGEs2"))
  out$end_model
})

i <- 1
fit4 <- with(
  mice2,
  expr
)

list.files(path = "Analysis/MI/", pattern = "\\.rdata$", ignore.case = TRUE)
date <- format(Sys.time(), "%y%m%d")
save(fit4, file = paste0("Analysis/MI/fit4_", date, ".Rdata"))
load(file = "Analysis/MI/fit4_null_240828.Rdata")

expr <- expression({
  cat("Imputation dataset: ", i, "\n")
  i <<- i + 1
  model <- glmer(
    OUT_DC_RELAPSE ~ 
      DM_SEX + TREAT_GRP4 + DM_AGEs + ZZ_AGEs2 + ZZ_AGEs3 + ZZ_MAL + VL_HISTORY + VL_DURATIONs + LB_BL_HGB_GRP2 + LB_BL_WBCs + LB_BL_PLATs + LB_BL_ALTs + LB_BL_CREATs + MP_BL_SPLEEN_LENGTHs2 + MB_COMBINEDs + (1 | STUDYID),
    family = binomial())
  
  out <- select_chi2(model, keep = NULL)
  out$end_model
})

i <- 1
fit4_null <- with(
  mice2,
  expr
)

date <- format(Sys.time(), "%y%m%d")
#save(fit4_null, file = paste0("Analysis/MI/fit4_null_", date, ".Rdata"))

fit4 <- fit4_null
variables <- list()
for (i in seq_along(fit4$analyses)) {
  variables[[i]] <- fit4$analyses[[i]] %>% fixef() %>% names()
}

variables <- variables %>% unlist() %>% table() %>% sort()
variables
var_tib <- tibble(
  variable = names(variables),
  freq = as.integer(variables)
)

var_tib <- var_tib %>% 
  mutate(
    variable = case_when(
      variable == "ZZ_AGEs3" ~ "Age^3",
      variable == "LB_BL_ALTs" ~ "ALT (log scale)",
      variable == "ZZ_MALModerate" ~ "Malnutrition (group)",
      variable == "LB_BL_HGB_GRP2Moderate anaemia" ~ "Anaemia (group)",
      variable == "VL_HISTORY1" ~ "History of VL",
      variable == "MP_BL_SPLEEN_LENGTHs2" ~ "Spleen length (log scale)",
      variable == "TREAT_GRP4OTHER" ~ "Treatment (group)",
      variable == "DM_AGEs" ~ "Age",
      variable == "ZZ_AGEs2" ~ "Age^2",
      variable == "VL_DURATIONs" ~ "Duration of fever (log scale)",
      variable == "MB_COMBINEDs" ~ "Smear parasite count",
      variable == "DM_SEXM" ~ "Sex",
      variable == "LB_BL_CREATs" ~ "Creatinine (log scale)",
      .default = variable
    )
  ) %>% 
  filter(!variable %in% c("LB_BL_HGB_GRP2No/mild anaemia", "TREAT_GRP4SDA", "ZZ_MALSevere", "(Intercept)"))
var_tib
var_empty <- tibble(
  variable = c("White blood cells (log scale)", "Platelets (log scale)"),
  freq = rep(0, 2)
)

var_tib <- bind_rows(var_tib, var_empty) %>% arrange(freq, variable)
var_tib

var_tib %>% ggplot() + 
theme_minimal() + 
geom_bar(
  stat = "identity",
  aes(
    y = reorder(variable, freq),
    x = freq
  )
) + 
labs(
  x = "Frequency selected",
  y = "Variable"
)

## final chi2 model

# with the 3 imputations, we have
# LB_BL_HGB_GRP2, MP_BL_SPLEEN_LENGTHs2, VL_HISTORY,
# DM_SEX, I(DM_AGEs), I(DM_AGEs^2), MB_COMBINED, TREAT_GRP4, 
# VL_DURATIONs

expr <- expression({
  cat("Imputation dataset: ", i, "\n")
  i <<- i + 1
  glmer(
    OUT_DC_RELAPSE ~ 
    DM_SEX + DM_AGEs + ZZ_AGEs2 + MB_COMBINEDs + VL_DURATIONs  + MP_BL_SPLEEN_LENGTHs2 + TREAT_GRP4 + VL_HISTORY + (1 | STUDYID),
    family = binomial())
})

i <- 1
fit5 <- with(
  mice2,
  expr
)

# look at the tau squared values (between study variation)
tau <- numeric()
for (i in seq_along(fit5$analyses)) {
  tau[i] <- VarCorr(fit5$analyses[[i]])$STUDYID[1, 1] %>% sqrt()
}
tau

fit5$analyses[[1]] %>% summary()
fit5$analyses[[2]] %>% summary()
fit5$analyses[[3]] %>% summary()

pool5 <- pool(fit5)
pool5 %>% summary() %>% View()

####################
# try Rubins Rules #
####################

# the dataset is a mids object!
# Rubin's rules with Wald test
out <- select_rr(mice2, keep = NULL)

# DM_AGEs

# ZZ_AGEs2
# VL_DURATIONs
# MP_BL_SPLEEN_LENGTHs2
# MB_COMBINEDs
# TREAT_GRP4
# LB_BL_HGB_GRP2

# fit final model
fit2 <- with(
  mice1,
  glmer(
    OUT_DC_RELAPSE ~ DM_AGEs + ZZ_AGEs2 + VL_DURATIONs + MP_BL_SPLEEN_LENGTHs2 + MB_COMBINEDs + TREAT_GRP4 + LB_BL_HGB_GRP2 + (1 | STUDYID),
    family = binomial
  )
)

pool(fit2) %>% summary() %>% View()

# forcing in sex and age (linear and quadratic terms)
out2 <- select_rr(mice2, keep = c("DM_AGEs", "ZZ_AGEs2", "DM_SEX"))
out2

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