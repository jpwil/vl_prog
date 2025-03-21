#####################################
## FIT MODEL TO IMPUTATION DATASET ##
#####################################

model_fit <- function(data) {
  
  if (is.null(data$mice$result)) {
    message("\nMICE FAILED")
    return(NULL)
  }

  # define formula from variable selection
  formula <- paste(data$var_rr$result$var_final$term, collapse = " + ")
  formula <- paste0("OUT_DC_RELAPSE ~ ", formula, " + (1 | STUDYID)")
  formula <- as.formula(formula)
  
  # fit the models
  model <- list()
  tau <- numeric(0)
  icc <- numeric(0)

  for (i in 1:data$mice$result_grp$m) {
    cat("\nFitting model to imputed dataset: ", i)
    model[[i]] <- glmer(
      data = complete(data$mice$result_grp, i),
      formula = formula, 
      family = binomial(),
      control = glmerControl(optimizer = "bobyqa")
    )
    tau[i] <- VarCorr(model[[i]])$STUDYID[1, 1]
    icc[i] <- tau[i] / (tau[i] + pi^2 / 3)
  }

  model_sum = pool(model) %>% summary() %>% as_tibble() %>% 
    mutate(term = as.character(term)) %>% 
    mutate(term = ifelse(term == "(Intercept)", "INT", term))

  return(list(model_mira = model, model_sum = model_sum, tau2 = tau, icc = icc))
}

# function that takes the model, in format 'model_sum' (above), and outputs the predictor
# the supplied dataset must have same variable names as prog$mice$result_grp

safe_model_fit <- function(data) {

  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- model_fit(data)
          time <- list(time_start = time_start, time_end = Sys.time())
          return(list(result = result, warnings = warnings_list, error = NULL, time = time))  # Return the result if successful
        },
        warning = function(w) {
          # Capture warnings in the warnings_list vector
          warnings_list <<- c(warnings_list, w$message)
          invokeRestart("muffleWarning")  # Prevent the warning from being displayed    
        } 
      )
    },
      error = function(e) {
        # Handle the error
        message("An error occurred: ", e$message)
        result <<- NULL  # Set result to NULL in case of error
        time <- list(time_start = time_start, time_end = Sys.time())
        return(list(result = result, warnings = warnings_list, error = e$message, time = time))  # Return the result if successful
      },
    finally = {
      # Code that will always run, even if there is an error or warning
      message("Completed execution of model_fit")
    }
  )
}