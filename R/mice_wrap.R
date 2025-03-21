# wrap the multiple imputation to easily catch errors and re-run automatically if necessary

safe_mice <- function(data, predictorMatrix, method, m, seed = NA, maxit, post, file = "default", ...) {
  warn_ind <- 1 # warning indicator
  warnings <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(warnings) <- c("Iteration", "Imputation", "Variable", "Time", "Message")
  current_state <<- list(iter = integer(1), imp = integer(1), variable = character(1)) # need this or mice() will break!

  time_start <- Sys.time()
  output2 <- tryCatch(
    error = function(cnd) {
      message("Caught an error: ", cnd$message)
      message("File not saved")
      output1 <- list()
      output1$error <- TRUE
      output1$cnd <- cnd
      output1$state <- current_state
      output1$warnings <- warnings
      output1
    },
    {
      withCallingHandlers(
        warning = function(cnd) {
          message("Caught a warning: ", cnd$message)
          warnings[warn_ind, 1] <<- current_state[[1]]
          warnings[warn_ind, 2] <<- current_state[[2]]
          warnings[warn_ind, 3] <<- current_state[[3]]
          warnings[warn_ind, 4] <<- Sys.time()
          warnings[warn_ind, 5] <<- as.character(cnd)
          #warnings[warn_ind, 6] <<- list(cnd$call)
          warn_ind <<- warn_ind + 1
        },
      { 
          mice1 <- mice(
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
        mice1$error <- FALSE
        mice1$warnings <- warnings
        mice1$time_start <- time_start
        mice1$time_end <- time_end <- Sys.time()
        mice1$time_dur <- time_start - time_end
        save(mice1, file = paste0("Analysis/MI/", file, ".Rdata")) 
        message("File saved")

        output1 <- list()
        output1$error <- FALSE
        output1$cnd <- NA
        output1$state <- NA
        output1$warnings <- warnings
        output1
      })
    }
  )
}
