# this function performs backwards stepwise for the fixed terms of a glmm (glmer object), 
# by applying Rubin's Rules at each stage of variable selection

# let us assume that the categorical variables are always 'TREAT_GRP4', 'LB_BL_HGB_GRP2', 'ZZ_MAL'
# we want 'combined' p-values for each variable in the model
# for non-categorical variables - this is easy, just pool the estimates and standard errors using the pool() function in mice
# for categorical variables - apply D1 pooling as described in van Buuren's book
# then, we combine all variables and p-values, and remove the variable with the largest p-value, given p > 0.10 (or some other predetermined cutoff value)


select_rr <- function(
  data,  # mids object
  keep = NULL, 
  p_select = 0.1,
  predictors_init) {

  if (is.null(data)) {
    message("\nMICE FAILED")
    return(NULL)
  }

  if (any(grepl("^ZZ_MAL", predictors_init))) {
    predictors_init <- c(predictors_init[!grepl("^ZZ_MAL", predictors_init)], "ZZ_MAL")
  }

  if (any(grepl("^TREAT_GRP4", predictors_init))) {
    predictors_init <- c(predictors_init[!grepl("^TREAT_GRP4", predictors_init)], "TREAT_GRP4")
  }

  var_track <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(var_track) <- c("Step number", "Variable", "removal p.value") 

  outcome <- "OUT_DC_RELAPSE"
  cluster <- "STUDYID"
  cat <- c("TREAT_GRP4", "ZZ_MAL")

  formula_str <- paste0(outcome, "~", paste0(predictors_init, collapse = " + "), "+ (1 |", cluster, ")")
  formula_new <- as.formula(formula_str)
  
  i <- 1
  stop <- FALSE

  
  while (!stop) {
    cat("\n***************************\n** Starting while loop", i, "**\n***************************\n\n")
    models_fit <- list()
    for (j in 1:data$m) {
      cat("Full imputation:", j, "\n")
      models_fit[[j]] <- data %>% 
        complete(j) %>%  
        glmer(formula = formula_new, family = binomial, control = glmerControl(optimizer = "bobyqa")) 
    }
    models_pool <- pool(models_fit)
    model_terms <- as.character(summary(models_pool)$term)          # univariate Wald tests combined with Rubin's Rules
    select_table <- summary(models_pool) %>% dplyr::select(term, p.value)

    # reduced model for D1 multivariate Wald test - TREAT_GRP4
    if (any(grepl("^TREAT_GRP4", model_terms))) {
      models_fit_comp <- list()
      formula_temp <- update(formula_new, . ~ . - TREAT_GRP4) # need to fit model without categorical variable

      for (j in 1:data$m) {
        cat("TREAT_GRP multivariate Wald model:", j, "\n")
        models_fit_comp[[j]] <- data %>% 
          complete(j) %>% 
          glmer(formula = formula_temp, family = binomial, control = glmerControl(optimizer = "bobyqa")) 
      }

      comparison <- D1(models_fit, models_fit_comp)
      add_row <- data.frame(term = "TREAT_GRP4", p.value = summary(comparison)$comparisons$p.value)
      select_table <- rbind(select_table, add_row)
      select_table <- select_table %>% filter(!term %in% c("TREAT_GRP4SDA", "TREAT_GRP4OTHER"))
    }

    # reduced model for D1 multivariate Wald test - ZZ_MAL
    if (any(grepl("^ZZ_MAL", model_terms))) {
      models_fit_comp <- list()
      formula_temp <- update(formula_new, . ~ . - ZZ_MAL) # fit model without categorical variable
      for (j in 1:data$m) {
        cat("ZZ_MAL multivariate Wald model:", j, "\n")
        models_fit_comp[[j]] <- data %>% 
          complete(j) %>% 
          glmer(formula = formula_temp, family = binomial, control = glmerControl(optimizer = "bobyqa")) 
      }
      comparison <- D1(models_fit, models_fit_comp)
      add_row <- data.frame(term = "ZZ_MAL", p.value = summary(comparison)$comparisons$p.value)
      select_table <- rbind(select_table, add_row)
      select_table <- select_table %>% filter(!term %in% c("ZZ_MALModerate", "ZZ_MALSevere"))
    }

    # clean up remaining terms so they can be used to update formula
    select_table <- select_table %>% 
      mutate(
        # term = case_when(
        #   term == "DM_SEXM" ~ "DM_SEX",
        #   term == "VL_HISTORY1" ~ "VL_HISTORY",
        #   .default = term
        # ),
        term = as.character(term)
      ) %>% 
      filter(term != "(Intercept)")

    # force only highest polynomial terms to be considered for removal

    select_table_available <- select_table 
    if ("ZZ_AGEs3" %in% select_table[, 1]) {
      select_table_available <- select_table_available %>% filter(!term %in% c("DM_AGEs", "ZZ_AGEs2"))
    } else if ("ZZ_AGEs2" %in% select_table[, 1]) {
      select_table_available <- select_table_available %>% filter(!term %in% c("DM_AGEs"))
    } 

    # force certain variables to stay in the model
    select_table_available <- select_table_available %>% 
      filter(!(term %in% keep))

    highest_p <- select_table_available[which.max(select_table_available$p.value), ]
    remove_term <- paste("~ . -", highest_p[[1]])

    if (highest_p[[2]] > p_select) {
      var_track[i, 1] <- i
      var_track[i, 2] <- highest_p[[1]]
      var_track[i, 3] <- highest_p[[2]]
      cat("\nRemoving predictor: ", highest_p[[1]], ", with p-value of: ", highest_p[[2]], "\n", sep = "")
      formula_new <- update(formula_new, as.formula(remove_term))
    } else {
      cat("\nThere are no further terms with p > ", p_select, "\n")
      stop <- TRUE
    }
    i <- i + 1
  }
  return(list("var_final" = select_table, "var_track" = var_track, "keep" = keep))
}

safe_select_rr <- function(data, keep, p_select, predictors_init) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          # Call the select_rr function
          result <- select_rr(data, keep, p_select, predictors_init)
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
        return(list(result = NULL, warnings = warnings_list, error = e$message, time = time))  # Return error message
      },
    finally = {
      # Code that will always run, even if there is an error or warning
      message("\n\nCompleted execution of select_rr.")
    }
  )
}
