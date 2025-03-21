library(tidyverse)
source("R/explore_meta.R")
source("R/compile_models.R")
source("R/compile_probs.R")
source("R/dummy_to_indicator_vec.R")

#setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/R/vl_relapse_model/") 
folders <- list.dirs("results/main", full.names = TRUE, recursive = FALSE)
folders

folder <- folders[length(folders)]
folder <- folders[18]

folder

files <- sort(list.files(folder, pattern = "^[0-9]{10}\\.rds$", full.names = TRUE))
files

## EXPLORE META.RDS
explore_meta(folder)
meta <- readRDS(paste0(folder, "/meta.rds"))

## COMPILE MODELS
model <- compile_models(folder)

## EXPLORE VARIABLES SELECTED
model[[1]]$var_wald$result$term_count
model[[1]]$var_wald$result$term_vec %>% dummy_to_indicator()
model[[1]]$var_rr$result$var_final$term  %>% dummy_to_indicator()

## COMPILE ERRORS AND WARNINGS
model_probs <- compile_probs(model)
model_probs %>% names()
model_probs %>% count(errors)
model_probs %>% View()

## EXPLORE SELECTED VARIABLES
model_fit  <- list()
for (i in seq_along(model)) {
  model_fit[[i]] <- model[[i]]$fit$result$model_sum
}
model_fit[[7]]                                                # original model
model_fit_comb <- do.call(rbind, model_fit)
model_fit_comb %>% count(term) %>% arrange(desc(n), term)     # model stability

## EXPLORE ICC 
model_icc <- list()
for (i in seq_along(model)) {
  model_icc[[i]] <- model[[i]]$fit$result$icc
}
model_icc
sapply(model_icc, mean)                       # mean ICC across all imputations for each model
sapply(model_icc, function(x) sum(x == 0))    # number of imputations resulting in degenerate models 

##########################
## PERFORMANCE MEASURES ##
##########################

# c-statistic
cstat_within_apparent <- sapply(model, function(x) x[["cstat_app"]][["result"]][["cw_all"]]) %>% t()
cstat_pop_apparent <- sapply(model, function(x) x[["cstat_app"]][["result"]][["c_pop"]]) %>% t()

cstat_within_boot <- sapply(model[-1], function(x) x[["cstat_boot"]][["result"]][["cw_all"]]) %>% t()
cstat_pop_boot <- sapply(model[-1], function(x) x[["cstat_boot"]][["result"]][["c_pop"]]) %>% t()

cstat_compare <- data.frame(
  cstat_clust_app  = cstat_within_apparent[, 4],
  cstat_clust_boot = c(NA, cstat_within_boot[, 4]),
  cstat_pop_app     = cstat_pop_apparent[, 4],
  cstat_pop_boot    = c(NA, cstat_pop_boot[, 4])
)

cstat_compare <- cstat_compare %>% 
  mutate(
    pop_opt = cstat_pop_boot - cstat_pop_app,
    clust_opt = cstat_clust_boot - cstat_clust_app,
    app_diff = cstat_clust_app - cstat_pop_app,
    boot_diff = cstat_pop_boot - cstat_clust_boot
  )

cstat_compare %>% View()

mean(cstat_compare[, 5], na.rm = TRUE)   # average popuation-level optimism
mean(cstat_compare[, 6], na.rm = TRUE)   # average cluster-level optimism
mean(cstat_compare[, 7], na.rm = TRUE)   # average difference between cluster & pop c-index for apparent performance
mean(cstat_compare[, 8], na.rm = TRUE)   # average difference between cluster & pop c-index for IV performance

# calibration
cal_slope_app_clust <- sapply(model, function(x) x[["cal_pool_app"]][["result"]][["cal_slope_overall"]]) %>% t()
cal_slope_app_pop <- sapply(model, function(x) x[["cal_pool_app"]][["result"]][["cal_slope_pop"]]) %>% t() ## same

cal_int_app_clust <- sapply(model, function(x) x[["cal_pool_app"]][["result"]][["cal_intercept_overall"]]) %>% t()
cal_int_app_pop <- sapply(model, function(x) x[["cal_pool_app"]][["result"]][["cal_intercept_pop"]]) %>% t() # same

cal_slope_boot_pop <- sapply(model[-1], function(x) x[["cal_pool_boot"]][["result"]][["cal_slope_pop"]]) %>% t() ## same
cal_int_boot_pop <- sapply(model[-1], function(x) x[["cal_pool_boot"]][["result"]][["cal_intercept_pop"]]) %>% t() # same

cal_slope_boot_clust <- sapply(model[-1], function(x) x[["cal_pool_boot"]][["result"]][["cal_slope_overall"]]) %>% t() ## same

cal_int_boot_clust <- sapply(model[-1], function(x) x[["cal_pool_boot"]][["result"]][["cal_intercept_overall"]]) %>% t() ## same

cal_compare <- data.frame(
  cal_slope_app_pop     = cal_slope_app_pop[, 1],
  cal_slope_boot_pop    = c(NA, cal_slope_boot_pop[, 1]),
  cal_slope_app_clust   = cal_slope_app_clust[, 1],
  cal_slope_boot_clust  = c(NA, cal_slope_boot_clust[, 1]),
  cal_int_app_clust     = cal_int_app_clust[, 1],
  cal_int_boot_clust    = c(NA, cal_int_boot_clust[, 1]),
  cal_int_app_pop       = cal_int_app_pop[, 1],
  cal_int_boot_pop      = c(NA, cal_int_boot_pop[, 1])
)

cal_compare <- cal_compare %>% 
  mutate(
    pop_slope_opt = cal_slope_app_pop - cal_slope_boot_pop,
    cond_slope_opt = cal_slope_app_clust - cal_slope_boot_clust,
    pop_int_opt = cal_int_app_pop - cal_int_boot_pop,
    cond_int_opt = cal_int_app_clust - cal_int_boot_clust
  )

cal_summary <- cal_compare %>% 
  summarise(
    n = sum(!is.na(pop_slope_opt)), 
    mean_pop_slope_opt = mean(pop_slope_opt, na.rm = TRUE),
    mean_cond_slope_opt = mean(cond_slope_opt, na.rm = TRUE),
    mean_pop_int_opt = mean(pop_int_opt, na.rm = TRUE),
    mean_cond_int_opt = mean(cond_int_opt, na.rm = TRUE))

cal_summary %>% View()
