###############################
## pool performance measures ##
###############################

safe_cal_pool <- function(prog, boot, cal_cond) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- cal_pool(prog, boot, cal_cond)
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

cal_pool <- function(prog, boot, cal_cond) {

  if (is.null(prog$mice$result)) {
    message("\nMICE FAILED")
    return(NULL)
  }

  alpha <- 0.05 # for confidence intervals

  if (!boot) {
    result <- prog$cal_app$result
  } else {
    result <- prog$cal_boot$result
  }

  cal_int <- result$intercept
  n_cluster <- dim(cal_int)[3]

  cal_citl_cluster_pool <- array(numeric(), dim = c(n_cluster, 5))
  cal_cs_cluster_pool <- array(numeric(), dim = c(n_cluster, 5))

  dimnames(cal_citl_cluster_pool) <- list(paste0("clust_", 1:n_cluster), c("estimate", "variance", "df", "ci_l", "ci_u"))
  dimnames(cal_cs_cluster_pool) <- list(paste0("clust_", 1:n_cluster), c("estimate", "variance", "df", "ci_l", "ci_u"))

  cal_cs_overall_pool <- array(numeric(), dim = c(5))
  cal_citl_overall_pool <- array(numeric(), dim = c(5))

  dimnames(cal_cs_overall_pool) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))
  dimnames(cal_citl_overall_pool) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))

  cal_cs_pop_pool <- array(numeric(), dim = c(5))
  cal_citl_pop_pool <- array(numeric(), dim = c(5))

  dimnames(cal_cs_pop_pool) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))
  dimnames(cal_citl_pop_pool) <- list(c("estimate", "variance", "df", "ci_l", "ci_u"))

  if (cal_cond) {

    for (i in 1:n_cluster) {
      pool <- pool.scalar(
        Q = result$intercept[, 1, i],
        U = result$intercept[, 2, i]^2,
        n = Inf) 
      cal_citl_cluster_pool[i, 1] <- pool$qbar
      cal_citl_cluster_pool[i, 2] <- pool$t
      cal_citl_cluster_pool[i, 3] <- pool$df
      cal_citl_cluster_pool[i, 4] <- pool$qbar - qt(1 - alpha / 2, pool$df) * sqrt(pool$t)
      cal_citl_cluster_pool[i, 5] <- pool$qbar + qt(1 - alpha / 2, pool$df) * sqrt(pool$t)
    }

    for (i in 1:n_cluster) {
      pool <- pool.scalar(
        Q = result$slope[, 1, i],
        U = result$slope[, 2, i]^2,
        n = Inf) 
      cal_cs_cluster_pool[i, 1] <- pool$qbar
      cal_cs_cluster_pool[i, 2] <- pool$t
      cal_cs_cluster_pool[i, 3] <- pool$df
      cal_cs_cluster_pool[i, 4] <- pool$qbar - qt(1 - alpha / 2, pool$df) * sqrt(pool$t)
      cal_cs_cluster_pool[i, 5] <- pool$qbar + qt(1 - alpha / 2, pool$df) * sqrt(pool$t)
    }

    pool_temp <- pool.scalar(
        Q = result$slope_fixed[, 1],
        U = result$slope_fixed[, 2]^2,
        n = Inf) 
    cal_cs_overall_pool[1] <- pool_temp$qbar
    cal_cs_overall_pool[2] <- pool_temp$t
    cal_cs_overall_pool[3] <- pool_temp$df
    cal_cs_overall_pool[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
    cal_cs_overall_pool[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)

    pool_temp <- pool.scalar(
        Q = result$intercept_fixed[, 1],
        U = result$intercept_fixed[, 2]^2,
        n = Inf) 
    cal_citl_overall_pool[1] <- pool_temp$qbar
    cal_citl_overall_pool[2] <- pool_temp$t
    cal_citl_overall_pool[3] <- pool_temp$df
    cal_citl_overall_pool[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
    cal_citl_overall_pool[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
  }

  pool_temp <- pool.scalar(
      Q = result$slope_pop[, 1],
      U = result$slope_pop[, 2]^2,
      n = Inf) 
  cal_cs_pop_pool[1] <- pool_temp$qbar
  cal_cs_pop_pool[2] <- pool_temp$t
  cal_cs_pop_pool[3] <- pool_temp$df
  cal_cs_pop_pool[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
  cal_cs_pop_pool[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)

  pool_temp <- pool.scalar(
      Q = result$intercept_pop[, 1],
      U = result$intercept_pop[, 2]^2,
      n = Inf) 
  cal_citl_pop_pool[1] <- pool_temp$qbar
  cal_citl_pop_pool[2] <- pool_temp$t
  cal_citl_pop_pool[3] <- pool_temp$df
  cal_citl_pop_pool[4] <- pool_temp$qbar - qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)
  cal_citl_pop_pool[5] <- pool_temp$qbar + qt(1 - alpha / 2, pool_temp$df) * sqrt(pool_temp$t)

  return(list(cal_slope_pop = cal_cs_pop_pool, cal_intercept_pop = cal_citl_pop_pool, cal_slope_overall = cal_cs_overall_pool, cal_intercept_overall = cal_citl_overall_pool, cal_slope_cluster = cal_cs_cluster_pool, cal_intercept_cluster = cal_citl_cluster_pool))
}
