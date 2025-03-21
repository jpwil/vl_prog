# this function compiles the errors and warnings in the different models and returns as a dataframe

compile_probs <- function(model) {
  
  results_boot <- c("mice", "var_wald", "var_rr", "fit", "cstat_app", "cstat_boot", "cal_app", "cal_boot", "cal_pool_app", "cal_pool_boot")
  files <- sort(list.files(folder, pattern = "^[0-9]{10}\\.rds$", full.names = TRUE))

  model_error <- list()

  for (i in seq_along(model)) {
    for (j in results_boot) {
      new_row <- data.frame(
        model_num = model[[i]]$boot,
        file_name = files[[i]],
        fun = j,
        errors =
          ifelse(
            "error" %in% names(model[[i]][[j]]), 
              ifelse(length(model[[i]][[j]]$error) == 0 & is.null(model[[i]][[j]]$error), FALSE, TRUE),
              NA),
        warnings = 
          ifelse(
            "warnings" %in% names(model[[i]][[j]]), 
              ifelse(j == "mice", nrow(model[[i]][[j]]$warnings), length(model[[i]][[j]]$warnings)),
              NA),
        warnings_mice = 
          ifelse(j == "mice" & "le" %in% names(model[[i]][[j]]), nrow(model[[i]][[j]]$le), NA)
      )
      model_error[[length(model_error) + 1]] <- new_row
    }
  } 
  out <- do.call(rbind, model_error)
  return(out)
}
