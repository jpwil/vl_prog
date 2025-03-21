# script to run mice and variable selection

# LOAD PACKAGES
packages <- c(
  "lme4", "micemd", "anthro", "anthroplus", "tidyverse", 
  "broom.mixed", "pROC", "meta", "brms", "boot", 
  "countimp", "arm", "aod")

p.error <- sapply(packages, require, character.only = TRUE)

# the model requires these R objects
df_scaled         <- readRDS(file = "data/ads_impute.rds")
df_scaled_values  <- readRDS(file = "data/ads_impute_scale.rds") # define this in global environment (here) to allow it to be accessed by the 'post' expression within mice()
folder <- "20250115_select"
keep <- c("ZZ_AGEs2", "DM_AGEs", "DM_SEX")

imp <- 100              # number of imputations
iter <- 20             # number of iterations per imputation
p <- 0.05             # p-value for inclusion

## LOAD SCRIPTS
source("R/mi_post_grouping.R")      ## GROUP MALNUTRITION & ANAEMIA
source("R/select_wald.r")           
source("R/select_lrt.r")

## MULTIPLE IMPUTATION 
source("R/safe_mice.R")                             # load custom mice wrapper (error, warning, timing tracking)
source("R/sampler_jw.r")                            # load edited version of mice dependent function - for error tracking
environment(sampler) <- asNamespace("mice")         # ensure correct namespace
assignInNamespace("sampler", sampler, ns = "mice")

load(file = "data/mi_initialise.rdata")             ## INITIALISE MULTIPLE IMPUTATION METHODS, PRED. MATRIX, POST

prog <- list()

prog$df_scaled <- df_scaled
prog$df_scaled_values <- df_scaled_values

prog$mice <- safe_mice(                 # capture mice errors, warnings & timings with custom wrapper
  data = df_scaled,                     # dataset to perform the multiple imputation on (cleaned, scaled)
  predictorMatrix = predictor.matrix,   # defined in mi_initialise.r
  method = method,                      # defined in mi_initialise.r
  m = imp,                              # number of imputations
  maxit = iter,                         # number of iterations
  seed = 123,  
  post = post,                          # defined in mi_initialise.r
  printFlag = TRUE)     

# specify analysis dataset following imputation
prog$mice$result_grp <- mi_post_grouping(prog)                                            

time <- format(Sys.time(), "%y%m%d%H%M")
folder_path <- paste0("results/main/", folder)
if (!is.null(folder)) {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)  # Allows creating nested folders if needed
  }
  saveRDS(prog, file = paste0(folder_path, "/", time, ".rds"))
}

# variable selection ($var)
prog$var_wald <- select_wald(data = prog$mice$result_grp, keep <- c("ZZ_AGEs2", "DM_AGEs", "DM_SEX"), p_value = 0.05)
time <- format(Sys.time(), "%y%m%d%H%M")
saveRDS(prog, file = paste0(folder_path, "/", time, "_wald.rds"))

# prog$var_lrt <- select_lrt(data = prog$mice$result_grp, keep <- c("ZZ_AGEs2", "DM_AGEs", "DM_SEX"), p_value = 0.05)
# time <- format(Sys.time(), "%y%m%d%H%M")
# saveRDS(prog, file = paste0(folder_path, "/", time, "_lrt.rds"))

# test_wald_vec <- lapply(test_wald2, function(x) x[,1]) %>% unlist() %>% table() %>% sort()
# test_wald_vec %>% barplot(las = 2, cex.names = 0.6)

# tab_models(test_wald2) %>% View()

