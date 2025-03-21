##################################
## WRAPPER FOR PREDICTION MODEL ##
##################################

# this is intended to be run as a script

args <- commandArgs(trailingOnly = TRUE)
# Check if arguments are provided
if (length(args) == 0) {
  stop("No arguments provided")
}

imp <-    as.numeric(args[1])               # number of imputations
iter <-   as.numeric(args[2])               # number of iterations per imputation
b <-      as.numeric(args[3])               # number of bootstraps
p <-      as.numeric(args[4])               # p-value for inclusion
folder <- as.character(args[5])             # folder for storing model

# LOAD PACKAGES
packages <- c(
  "lme4", "micemd", "anthro", "anthroplus", "tidyverse", 
  "broom.mixed", "pROC", "meta", "brms", "boot", 
  "countimp", "arm")

p.error <- sapply(packages, require, character.only = TRUE)

# LOAD FUNCTION SCRIPTS
source("R/mi_master.R")

# the model requires these R objects
df_scaled         <- readRDS(file = "data/ads_impute.rds")
df_scaled_values  <- readRDS(file = "data/ads_impute_scale.rds") # define this in global environment (here) to allow it to be accessed by the 'post' expression within mice()
keep <- c("ZZ_AGEs2", "DM_AGEs", "DM_SEX")


# INTERNAL VALIDATION (BOOTSTRAP AT CLUSTER LEVEL)
df_boot_cluster <- list()
df_scaled_boot <- df_scaled
names(df_scaled_boot)[names(df_scaled_boot) == "STUDYID"] <- "ORIG_STUDYID"

for (j in 1:b) {
  resample <- sample(seq(1:length(unique(df_scaled_boot$ORIG_STUDYID))), replace = TRUE)  # sample the study IDs with replacements
  df_list <- vector("list", length(resample))
  for (i in seq_along(resample)) {
    df_list[[i]] <- df_scaled_boot[df_scaled_boot$ORIG_STUDYID == resample[i], ]
    df_list[[i]]$STUDYID <- i
  }
  df_boot_cluster[[j]] <- do.call(rbind, df_list)
}

arguments <- list(
  "keep" = keep,
  "imputations" = imp,
  "iterations" = iter,
  "p-value" = p,
  "bootstraps" = b,
  "time_now" = Sys.time()
)

datasets <- list(
  "orig_dataset" = df_scaled,
  "orig_dataset_scale_values" = df_scaled_values,
  "boot_datasets" = df_boot_cluster
)

if (!dir.exists(paste0("results/main/", folder))) {
  dir.create(paste0("results/main/", folder), recursive = TRUE)
}

saveRDS(list("arguments" = arguments, "datasets" = datasets), file = paste0("results/main/", folder, "/meta.rds"))

original <- do_model(
  data = df_scaled,
  iter = iter, 
  imp = imp, 
  seed = NA,  # to ensure reproducibility
  boot = 0,  # 0 for original
  keep = keep, 
  save_folder = folder,
  p_select = p) 

# INTERNAL VALIDATION (BOOTSTRAP AT PATIENT LEVEL)
# df_scaled <- readRDS(file = "data/ads_impute.rds")
# df_boot_ind <- list() # initiate
# for (j in 1:b) {
#   resample <- sample(seq(1:nrow(df_scaled)), replace = TRUE)  # sample each individual with replacement
#   df_boot_ind[[j]] <- df_scaled[resample, ]
# }

# PERFORM INTERNAL VALIDATION
int_val <- list() 

for (i in 1:b) {
  df_boot_cluster[[i]] <- df_boot_cluster[[i]][, -1]
  df_boot_cluster[[i]] <- df_boot_cluster[[i]] %>% relocate(STUDYID)

  int_val[[i]] <- do_model(
    data = df_boot_cluster[[i]],  
    iter = iter, 
    imp = imp, 
    seed = NA, # to ensure reproducibility
    boot_mids = original$mice$result_grp, 
    boot = i,
    keep = keep, 
    save_folder = folder,
    p_select = p)
}
