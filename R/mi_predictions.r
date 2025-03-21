# average random-intercept

# this linear predictor ignores the random effect b_j, therefore assumes a *study-average random-intercept*
# when performing internal validation this is necessary because we have new clusters
predict_lp <- function(data = complete(prog$mice$result_grp, 1), model = prog$fit$result$model_sum) {
  data_predict <- data[, as.character(model$term)]
  lp <- numeric(nrow(data))
  for (i in 1:length(lp)) {
    lp[i] <- sum(as.vector(unlist(data_predict[i, ])) * model$estimate)
  }
  return(lp)
}

safe_predict_lp <- function(data) {
  warnings_list <- character() 
  result <- NULL 
  time_start <- Sys.time()
  tryCatch(
    {
      withCallingHandlers(
        {
          result <- predict_lp(imp, data = prog$mice$result_grp, model = prog$fit$result$model_sum)
          time <- list(time_start = time_start, time_end = Sys.time())
          return(list(result = result, warnings = warnings_list, error = NULL, time = time))  
        },
        warning = function(w) {
          warnings_list <<- c(warnings_list, w$message)
          invokeRestart("muffleWarning")  
        } 
      )
    },
      error = function(e) {
        message("An error occurred: ", e$message)
        result <<- NULL 
        time <- list(time_start = time_start, time_end = Sys.time())
        return(list(result = result, warnings = warnings_list, error = e$message, time = time))  
      },
    finally = {}
  )
}
