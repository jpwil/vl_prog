# wrap the multiple imputation to easily catch errors and re-run automatically if necessary

safe_mice <- function(data, predictorMatrix, method, m, seed = NA, maxit, post, file = "default", ...) {
  warn_ind <- 1
  warnings <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(warnings) <- c("Iteration", "Imputation", "Variable", "Time", "Message")
  current_state <<- list(iter = integer(1), imp = integer(1), variable = character(1)) # need this or mice() will break!
  result <- NULL
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        { 
            result <- mice(
            data = data, 
            predictorMatrix = predictorMatrix, 
            #visitSequence = visit.sequence,
            method = method,
            m = m,
            maxit = maxit,
            seed = seed,
            post = post,
            ...
          )
          time <- list(time_start = time_start, time_end = Sys.time())
          return(list(result = result, warnings = warnings, error = NULL, time = time))
      },
        warning = function(w) {
          cat("\nCaught a warning: ", w$message) # display warning
          warnings[warn_ind, 1] <<- current_state[[1]]
          warnings[warn_ind, 2] <<- current_state[[2]]
          warnings[warn_ind, 3] <<- current_state[[3]]
          warnings[warn_ind, 4] <<- Sys.time()
          warnings[warn_ind, 5] <<- as.character(w)
          #warnings[warn_ind, 6] <<- list(cnd$call)
          warn_ind <<- warn_ind + 1
        }
      )
    },
    error = function(e) {
      #Handle the error
      message("An error occurred: ", e$message)
      result <<- NULL # Set result to NULL in case of error
      time <- list(time_start = time_start, time_end = Sys.time())
      return(list(result = NULL, warnings = warnings, error = e$message, time = time))  # Return error message

    },
    finally = {
      cat("\nCompleted execution of mice\n")
    }
  )
}
