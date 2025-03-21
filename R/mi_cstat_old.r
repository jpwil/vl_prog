##########################
# calculate c-statistics #
##########################
 
# MI BOOT method (Schomaker et al 2018)

safe_cstat_apparent <- function(prog) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- cstat_apparent(prog)
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
    finally = {
      # Code that will always run, even if there is an error or warning
      message("Completed execution of cstat_apparent")
    }
  )
}

# custom function bootstrap standard error of auc
auc_se <- function(response, predictor, n_boot = 1000) {
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
      #message(levels(response_boot), "\n")
      roc_obj_boot <- roc(response = response_boot, predictor = predictor_boot, quiet = TRUE)
      auc_values[i] <- auc(roc_obj_boot)
    }
  }

  # calculate standard error of bootstrapped AUCs
  auc_logit <- log(auc_values / (1 - auc_values))
  auc_logit_pos_inf <- sum(auc_logit == Inf, na.rm = TRUE)
  auc_logit_neg_inf <- sum(auc_logit == -Inf, na.rm = TRUE)
  auc_na <- sum(is.na(auc_logit))
  auc_logit_se <- sd(auc_logit[auc_logit != Inf & auc_logit != -Inf], na.rm = TRUE)
  return(list(auc_logit_se, auc_logit_neg_inf, auc_logit_pos_inf, auc_na))
}


# combined cstat measures
cstat_apparent <- function(prog) {
  cw_ind <- mi_cw_ind(prog)
  cw_all <- mi_cw_all(prog)
  return(list(cw_ind = cw_ind, cw_all = cw_all))
}

# this function calculate the cluster specific c-index for all clusters & imputations
# bootstrapping is used to calculate the confidence intervals
# when there are zero events the auc is undefined and NA will appear
# when events are perfectly discriminated by the model, auc = 1 and confidence intervals are 1-1
mi_cw_ind <- function(prog) {
  imputations <- prog$mice$result_grp$m
  clusters <- unique(complete(prog$mice$result_grp, 1)$STUDYID) %>% sort()

  # create dataset array of lists (dimension = number of clusters x number of imputed datasets)
  dataset_array <- array(list(), dim = c(length(clusters), imputations)) # list of length (no of clusters)
  for (i in 1:imputations) {
    for (c in clusters) {
      dataset_array[c, i] <- complete(prog$mice$result_grp, i) %>% filter(STUDYID == c) %>% list()
    }
  }

  # create model list (length = number of imputated dataset)
  model_list <- list()   # each model represents a model developed from one of the imputed datasets 
  for (i in 1:imputations) {
    model_list[[i]] <- prog$fit$result$model_mira[[i]]
  }

  # create c-statistics with confidence intervals
  auc_cluster <- array(numeric(), dim = c(length(clusters), 10, imputations)) # list of length (no of clusters)
  dimnames(auc_cluster) <- list(NULL, c("imputation", "cluster", "size", "events_present", "events_no", "auc_est", "auc_se", "auc_neg_inf", "auc_pos_inf", "auc_na"))

  for (i in 1:imputations) {
    for (c in clusters) {
      cat("\nWithin cluster Cw with bootstrapping - imputation: ", i, " cluster: ", c)
      if (sum(dataset_array[c, i][[1]]$OUT_DC_RELAPSE) != 0) {
        predictor <- predict(model_list[[i]], newdata = dataset_array[c, i][[1]], type = "link", re.form = NULL)
        response <- dataset_array[c, i][[1]] %>% pull(OUT_DC_RELAPSE)
        auc_est <- auc(roc(response ~ predictor, levels = c(FALSE, TRUE), direction = "<"))
        auc_logit_est <- log(auc_est / (1 - auc_est))
        auc_logit_se <- auc_se(response = response, predictor = predictor)
        auc_cluster[c, 6, i] <- auc_logit_est
        auc_cluster[c, 7, i] <- auc_logit_se[[1]]
        auc_cluster[c, 8, i] <- auc_logit_se[[2]]
        auc_cluster[c, 9, i] <- auc_logit_se[[3]]
        auc_cluster[c, 10, i] <- auc_logit_se[[4]]
      }
      auc_cluster[c, 1:5, i] <- c(
        i, c, nrow(dataset_array[c, i][[1]]), 
        sum(dataset_array[c, i][[1]]$OUT_DC_RELAPSE) != 0, 
        sum(dataset_array[c, i][[1]]$OUT_DC_RELAPSE))
    }
  }

  out <- array(numeric(), dim = c(imputations, 8, length(clusters)))
  # format output to make pooling simple
  for (i in clusters) {
    out[, , i] <- auc_cluster[i, c("imputation", "size", "events_no", "auc_est", "auc_se", "auc_neg_inf", "auc_pos_inf", "auc_na"), ] %>% t()
  }
  return(out)
}

# this function calculates the overall within-cluster c-index for the imputed datasets
# bootstrapping with statified resampling at the cluster-level
mi_cw_all <- function(prog) {

  clusters <- unique(complete(prog$mice$result_grp, 1)$STUDYID)  # unique clusters
  imputations <- prog$mice$result_grp$m

  output <- array(numeric(), dim = c(imputations, 6))
  dimnames(output) <- list(NULL, c("imputation", "cw_est", "cw_se",  "auc_neg_inf", "auc_pos_inf", "auc_na"))

  for (i in 1:imputations) {
    cat("\nOverall Cw with bootstrapping - imputation: ", i)
    model <- prog$fit$result$model_mira[[i]]
    dataset <- complete(prog$mice$result_grp, i) 
    lp <- predict(model, newdata = dataset, re.form = ~ (1 | STUDYID), type = "link")
    data <- data.frame(outcome = dataset$OUT_DC_RELAPSE, lp = lp, cluster = dataset$STUDYID)

    cw <- concord_within(outcome = dataset$OUT_DC_RELAPSE, lp = lp, cluster = dataset$STUDYID)
    cluster_boot <- boot(
      data = data,
      statistic = boot_cw_overall,
      strata = as.numeric(interaction(data$cluster, data$outcome, drop = TRUE)),  # stratified sampling from cases / controls
      R = 1000)
    cw_b <- cluster_boot$t
    cluster_logit_se_pos_inf <- sum(cw_b == 1, na.rm = TRUE)
    cluster_logit_se_neg_inf <- sum(cw_b == 0, na.rm = TRUE)
    auc_na <- sum(is.na(cw_b)) 
    cw_b <- cw_b[cw_b != 0 & cw_b != 1 & !is.na(cw_b)]
    cluster_logit_se <- sd(log(cw_b / (1 - cw_b)), na.rm = TRUE)  # Standard error on the logit scale

    output[i, ] <- c(i, log(cw / (1 - cw)), cluster_logit_se, cluster_logit_se_neg_inf, cluster_logit_se_pos_inf, auc_na)
  }
return(output)
}

# define bootstrap function for mi_cw_all()
boot_cw_overall <- function(data, indices) {
  resampled_data <- data[indices, ]
  cw <- concord_within(outcome = resampled_data$outcome, lp = resampled_data$lp, cluster = resampled_data$cluster)
  return(cw)
}

# calculate the overall within study c-index (no bootrapping, estimate only)
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
  return(concord)
}
