##################################
## WRAPPER FOR PREDICTION MODEL ##
##################################

#setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/R/vl_relapse_model/")
 
# LOAD PACKAGES
packages <- c(
  "lme4", "micemd", "anthro", "anthroplus", 
  "broom.mixed", "pROC", "meta", "boot", 
  "countimp", "tidyverse", "arm", "aod")

p.error <- sapply(packages, require, character.only = TRUE)

# LOAD FUNCTION SCRIPTS
source("R/mi_master.R")
source("R/mi_bootstrap.R")

# the model requires these R objects
df_scaled         <- readRDS(file = "data/ads_impute.rds")
df_scaled_values  <- readRDS(file = "data/ads_impute_scale.rds") # define this in global environment (here) to allow it to be accessed by the 'post' expression within mice()
folder            <- "20250204_test1"
keep              <- NULL
cluster           <- TRUE
boot_cond         <- TRUE

imp    <- 5                # number of imputations
iter   <- 3                # number of iterations per imputation
b      <- 5                # number of bootstraps
p_wald <- 0.10              # p-value for wald selection 
p_rr   <- 0.05              # p-value for rr selection

df_boot <- mi_bootstrap(df = df_scaled, bs_num = b, cluster = cluster)
predictors_init <- c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "ZZ_AGEs3", "TREAT_GRP4", "ZZ_MAL", 
    "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "LB_BL_WBCs", "LB_BL_PLATs", 
    "LB_BL_ALTs", "LB_BL_CREATs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")

arguments <- list(
  "keep" = keep,
  "cluster" = cluster,
  "imputations" = imp,
  "iterations" = iter,
  "p-value (Wald)" = p_wald,
  "p-value (RR)" = p_rr,
  "bootstraps" = b,
  "time_now" = Sys.time(),
  "predictors_init" <- predictors_init,
  "boot_cond" <- boot_cond 
)

datasets <- list(
  "orig_dataset" = df_scaled,
  "orig_dataset_scale_values" = df_scaled_values,
  "boot_datasets" = df_boot
)

if (!dir.exists(paste0("results/main/", folder))) {
  dir.create(paste0("results/main/", folder), recursive = TRUE)
}

saveRDS(list("arguments" = arguments, "datasets" = datasets), file = paste0("results/main/", folder, "/meta.rds"))

original <- do_model(
  data = df_scaled,
  iter = iter, 
  imp = imp, 
  seed = 1,  # to ensure reproducibility
  boot = 0,  # 0 for original
  keep = keep, 
  save_folder = folder,
  p_wald = p_wald,
  p_rr = p_rr,
  cal_cond = TRUE,
  predictors_init = predictors_init) 

# PERFORM INTERNAL VALIDATION
int_val <- list() 
for (i in 1:b) {
  int_val[[i]] <- do_model(
    data = df_boot[[i]],  
    iter = iter, 
    imp = imp, 
    seed = i + 1, # to ensure reproducibility
    boot_mids = original$mice$result_grp, 
    boot = i,
    keep = keep, 
    save_folder = folder,
    p_wald = p_wald,
    p_rr = p_rr,
    cal_cond = boot_cond,
    predictors_init = predictors_init
  )
}
