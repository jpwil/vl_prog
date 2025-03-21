##########################
# calculate c-statistics #
##########################
 
# MI BOOT (PS) method [Schomaker et al 2018]

# this wraps the cstat_combined function to capture & log errors/warnings
safe_cstat <- function(prog, data_eval, boot = FALSE) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- cstat_combined(prog, data_eval, boot)
          time <- list(time_start = time_start, time_end = Sys.time())
          return(list(result = result, warnings = warnings_list, error = NULL, time = time))  # Return the result if successful
        },
        warning = function(w) {
          # Capture warnings in the warnings_list vector
          cat("processing warning now")
          warnings_list <<- c(warnings_list, w$message)
          invokeRestart("muffleWarning")  # Prevent the warning from being displayed    
        } 
      )
    },
      error = function(e) {
        # Handle the error
        message("An error occurred: ", e$message)
        result <- NULL  # Set result to NULL in case of error
        time <- list(time_start = time_start, time_end = Sys.time())
        return(list(result = result, warnings = warnings_list, error = e$message, time = time))  # Return the result if successful
      },
    finally = {}
  )
}

# combined cstat measures 
cstat_combined <- function(prog, data_eval, boot) {
  if (is.null(prog$mice$result)) {
    message("\nMICE FAILED")
    return(NULL)
  }

  if (boot) {
    cw_ind <- NULL
  } else {
    cw_ind <- mi_cw_ind(prog, data_eval)          # this function calculates the cluster specific c-index
  }

  cw_all <- mi_cw_all(prog, data_eval)            # this function calculates the within-cluster c-index
  c_pop <- mi_c_pop(prog, data_eval)              # this function calculates the population (standard) c-index

  return(list(cw_ind = cw_ind, cw_all = cw_all, c_pop = c_pop))  
}

# custom function bootstrap standard error of auc
auc_se <- function(response, predictor, n_boot = 500) {
  # Initialize a vector to store AUC values
  auc_values <- numeric(n_boot)
  n_cases <- sum(response)
  n_controls <- length(response) - n_cases

  #cat("boot no:")
  # Stratified bootstrapping
  for (i in 1:n_boot) {
    #cat("\"n", i)
    # Resample data with replacement

    if (n_cases == 0) { # AUC is not defined in clusters with zero cases
      auc_values[i] <- NA
    } else {
      sample_indices_cases <- sample(1:n_cases, n_cases, replace = TRUE)            
      sample_indices_controls <- sample(1:n_controls, n_controls, replace = TRUE)  
      pred_cases <- predictor[response][sample_indices_cases]         # bootstrapped predictors for cases
      pred_controls <- predictor[!response][sample_indices_controls]  # bootstrapped predictors for controls
      # Get the bootstrapped sample 
      predictor_boot <- c(pred_cases, pred_controls)
      response_boot <- c(rep(TRUE, length(pred_cases)), rep(FALSE, length(pred_controls)))
      
      roc_obj_boot <- roc(response = response_boot, predictor = predictor_boot, quiet = TRUE, direction = "<")
      auc_values[i] <- auc(roc_obj_boot)
    }
  }
  return(auc_values) # vector of length boot_n
}

# this function calculate the cluster specific c-index for all clusters & imputations
# this function is not called when performing internal validation via bootstrapping
# bootstrapping is used to calculate the confidence intervals
# when there are zero events the auc is undefined and NA will appear
# when events are perfectly discriminated by the model, auc = 1 and confidence intervals are 1-1
mi_cw_ind <- function(prog, data_eval) {
  data_row <- nrow(complete(data_eval, 1))
  imputations <- data_eval$m
  clusters <- unique(complete(data_eval, 1)$STUDYID) %>% sort()
  clusters_index <- complete(data_eval, 1)$STUDYID

  # create dataset array of lists (dimension = number of clusters x number of imputed datasets)
  dataset_array <- array(list(), dim = c(length(clusters), imputations)) # list of length (no of clusters)
  for (i in 1:imputations) {
    for (c in clusters) {
      dataset_array[c, i] <- complete(data_eval, i) %>% filter(STUDYID == c) %>% list()
    }
  }
  # select model (this is not the usual model class that R creates from fitting a model)
  model <- prog$fit$result$model_sum

  # create c-statistics with confidence intervals
  auc_cluster_boot <- array(numeric(), dim = c(imputations, length(clusters), 500))
  predictor_array <- array(numeric(), dim = c(data_row, imputations))

  for (i in 1:imputations) {
    predictor_array[, i] <- predict_lp(data = complete(data_eval, i), model = model)
    for (c in clusters) {
      cat("\nWithin cluster Cw with bootstrapping - imputation: ", i, " cluster: ", c)
      if (sum(dataset_array[c, i][[1]]$OUT_DC_RELAPSE) != 0) {
        predictor <- predictor_array[, i][clusters_index == c]
        response <- dataset_array[c, i][[1]] %>% pull(OUT_DC_RELAPSE)
        auc_cluster_boot[i, c, ] <- auc_se(response = response, predictor = predictor)
      }
    }
  }

  auc_clust_quan <- array(numeric(), dim = c(length(clusters), 7))
  dimnames(auc_clust_quan) <- list(paste0("clust", 1:length(clusters)), c("min", "lci", "lq", "med", "uq", "uci", "max"))

  for (c in clusters) {
    auc_clust_quan[c, ] <- quantile(auc_cluster_boot[, c, ], prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = TRUE)
  }
  return(auc_clust_quan)
}

# this function calculates the overall within-cluster c-index for the imputed datasets
# this function is called for both original model and bootstrap models (internal validation)
# bootstrapping is performed with statified resampling at the cluster-level to calculate confidence intervals
mi_cw_all <- function(prog, data_eval) {

  clusters <- unique(complete(data_eval, 1)$STUDYID)  # unique clusters
  imputations <- data_eval$m

  cw_b <- array(numeric(), dim = c(500, imputations))

  for (i in 1:imputations) {
    cat("\nWithin-cluster c-statistic with bootstrapping - imputation: ", i)
    model <- prog$fit$result$model_sum
    dataset <- complete(data_eval, i) 
    lp <- predict_lp(data = complete(data_eval, i), model = model) # average random intercept predictions
    data <- data.frame(outcome = dataset$OUT_DC_RELAPSE, lp = lp, cluster = dataset$STUDYID)
    #cw <- concord_within(outcome = dataset$OUT_DC_RELAPSE, lp = lp, cluster = dataset$STUDYID)
    cluster_boot <- boot(
      data = data,
      statistic = boot_cw_overall,
      strata = as.numeric(interaction(data$cluster, data$outcome, drop = TRUE)),  # stratified sampling from cases / controls
      R = 500)
    cw_b[, i] <- cluster_boot$t
  }
  out <- quantile(cw_b, prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = TRUE)
  return(out)
}

# define bootstrap function for mi_cw_all()
boot_cw_overall <- function(data, indices) {
  resampled_data <- data[indices, ]
  cw <- concord_within(outcome = resampled_data$outcome, lp = resampled_data$lp, cluster = resampled_data$cluster)
  return(cw)
}

# calculate the overall within study c-index (no bootstrapping, estimate only)
# optimised for speed
concord_within <- function(outcome, lp, cluster) { 
  # Create a data frame and split by cluster
  data <- data.frame(outcome = outcome, lp = lp, cluster = cluster)
  split_data <- split(data, data$cluster)
  
  # Initialize total numerator and denominator
  total_num <- 0
  total_den <- 0
  
  # Loop through each cluster's data
  for (cluster_data in split_data) {
    lp_j <- cluster_data$lp
    outcome_j <- cluster_data$outcome
    n <- length(lp_j)
    
    # Vectorized comparisons using matrix subtraction and logical indexing
    lp_diff <- outer(lp_j, lp_j, ">")  # Logical matrix where lp_j[i] > lp_j[k]
    outcome_diff <- outer(outcome_j, outcome_j, function(x, y) x & !y)  # Outcome comparison matrix
    
    # Numerator: Concordant pairs where lp_j[i] > lp_j[k] and outcome_j[i] == 1, outcome_j[k] == 0
    num <- sum(lp_diff & outcome_diff)
    
    # Denominator: All pairs where outcome_j[i] == 1 and outcome_j[k] == 0
    den <- sum(outcome_diff)
    
    # Add to total numerator and denominator
    total_num <- total_num + num
    total_den <- total_den + den
  }
  
  # Return concordance within clusters (handling potential divide-by-zero)
  concord <- if (total_den == 0) NA else total_num / total_den
  #cat("\ntotal_den:", total_den, "\ntotal_num:", total_num)
  return(concord)
}

mi_c_pop <- function(prog, data_eval) {

imp <- prog$mice$result$m
auc <- array(numeric(), dim = c(500, imp))

for (i in 1:imp) {
  cat("\nCalculate population (standard) c-index for imputation:", i)
  data <- complete(data_eval, i)
  lp <- predict_lp(data = data, model = prog$fit$result$model_sum)
  response <- data$OUT_DC_RELAPSE
  auc[, i] <- auc_se(response = response, predictor = lp, n_boot = 500)
}

auc_quan <- quantile(auc, prob = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))
return(auc_quan)
}