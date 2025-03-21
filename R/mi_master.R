##############################
## HIGH LEVEL MODEL WRAPPER ##
##############################

# this function develops and evaluates a VL prognostic model with relapse as the outcome
# if boot = TRUE, need to specify where evaluation data is
do_model <- function(data, iter, imp, seed, boot = 0, boot_mids, keep, save_folder = NULL, p_wald, p_rr, scale, cal_cond, predictors_init) {   

  ## LOAD SCRIPTS
  source("R/mi_post_grouping.R")      ## GROUP MALNUTRITION & ANAEMIA
  source("R/select_rr.r")             ## VARIABLE SELECTION (RUBIN'S RULES)
  source("R/select_wald.r")
  source("R/model_fit.R")             ## FIT MODEL 
  source("R/mi_cstat_ps.r")           ## PERFORMANCE: C-INDEX (INCLUDING BOOTSTRAPPING)
  source("R/mi_cal.r")                ## PERFORMANCE: CALIBRATION
  source("R/mi_cal_pool.r")           ## BOOTSTRAP CALIBRATION
  source("R/mi_predictions.r")        ## PREDICT LP FOR AVERAGE RANDOM INTERCEPT

  ## MULTIPLE IMPUTATION 
  source("R/safe_mice.R")                             # load custom mice wrapper (error, warning, timing tracking)
  source("R/sampler_jw.r")                            # load edited version of mice dependent function - for error tracking
  environment(sampler) <- asNamespace("mice")         # ensure correct namespace
  assignInNamespace("sampler", sampler, ns = "mice")

  method <- readRDS(file = "data/mi_method.rds")             ## INITIALISE MULTIPLE IMPUTATION METHODS, PRED. MATRIX, POST
  predictor.matrix <- readRDS(file = "data/mi_pm.rds")             ## INITIALISE MULTIPLE IMPUTATION METHODS, PRED. MATRIX, POST
  post <- readRDS(file = "data/mi_post.rds")
  
  prog <- list()

  prog$df_scaled <- data
  prog$df_scaled_values <- df_scaled_values
  prog$boot <- boot

  prog$mice <- safe_mice(                 # capture mice errors, warnings & timings with custom wrapper
    data = data,                          # dataset to perform the multiple imputation on (cleaned, scaled)
    predictorMatrix = predictor.matrix,   # defined in mi_initialise.r
    method = method,                      # defined in mi_initialise.r
    m = imp,                              # number of imputations
    maxit = iter,                         # number of iterations
    seed = seed,  
    post = post,                          # defined in mi_initialise.r
    printFlag = TRUE)     

  # specify analysis dataset following imputation
  prog$mice$result_grp <- mi_post_grouping(prog)                                            

  # variable selection ($var)
  prog$var_wald <- safe_select_wald(
    data = prog$mice$result_grp,
    keep = keep,
    p_select = p_wald,
    predictors_init = predictors_init
  )
  
  prog$var_rr <- safe_select_rr(
    data = prog$mice$result_grp, 
    keep = keep, 
    p_select = p_rr,
    predictors_init = prog$var_wald$result$term_vec
  )

  # fits the final variable list to each imputed dataset and pools the results
  prog$fit <- safe_model_fit(prog) 
  
  ## PERFORMANCE 

  # c-statistics (both within cluster and overall measures: $cstat)
  if (boot > 0) {
    prog$cstat_app  <- safe_cstat(prog, data_eval = prog$mice$result_grp, boot = TRUE)
    prog$cstat_boot <- safe_cstat(prog, data_eval = boot_mids, boot = TRUE)
  } else {
    prog$cstat_app  <- safe_cstat(prog, data_eval = prog$mice$result_grp, boot = FALSE)
    prog$cstat_boot <- NA
  }

  # calibration ($cal)
  if (boot > 0) {
    prog$cal_app <- safe_cal(prog, data_eval = prog$mice$result_grp, boot = TRUE, cal_cond = cal_cond)
    prog$cal_boot <- safe_cal(prog, data_eval = boot_mids, boot = TRUE, cal_cond = cal_cond)
  } else {
    prog$cal_app  <- safe_cal(prog, data_eval = prog$mice$result_grp, boot = FALSE, cal_cond = cal_cond)
    prog$cal_boot <- NA
  } 

  # pool calibration metrics
  if (boot > 0) {
    prog$cal_pool_app <- safe_cal_pool(prog, boot = FALSE, cal_cond = cal_cond)
    prog$cal_pool_boot <- safe_cal_pool(prog, boot = TRUE, cal_cond = cal_cond)
  } else {
    prog$cal_pool_app <- safe_cal_pool(prog, boot = FALSE, cal_cond = cal_cond)
    prog$cal_pool_boot <- NA
  }

  time <- format(Sys.time(), "%y%m%d%H%M")
  folder_path <- paste0("results/main/", save_folder)
  if (!is.null(save_folder)) {
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)  # Allows creating nested folders if needed
    }
    saveRDS(prog, file = paste0(folder_path, "/", time, ".rds"))
  }

  return(prog)
}
