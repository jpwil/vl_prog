
# this function performs backwards stepwise for the 
# fixed terms of a glmm (glmer object), using Wald test 

# data must be a mids object from mice package
select_wald <- function(data, keep, p_select, predictors_init) {
 
# define daughter function here
tab_models <- function(model_list) {
  ordered_list <- lapply(model_list, function(x) sort(x[[1]]))
  col_names <- unique(unlist(ordered_list))
  df <- data.frame(matrix(ncol = length(col_names), nrow = 0))

  for (i in seq_along(ordered_list)) {
    new_row <- sort(col_names) %in% ordered_list[[i]]
    df <- rbind(df, new_row)
    colnames(df) <- sort(col_names)
  }

  df_reduced <- df %>% group_by(across(everything())) %>% 
  summarise(count = n(), .groups = "drop")

  return(df_reduced)
}

outcome <- "OUT_DC_RELAPSE"
cluster <- "STUDYID"
#predictors_init <- c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "ZZ_AGEs3", "TREAT_GRP4", "ZZ_MAL", "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "LB_BL_WBCs", "LB_BL_PLATs", "LB_BL_ALTs", "LB_BL_CREATs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
cat <- c("TREAT_GRP4", "ZZ_MAL")

formula_str <- paste0(outcome, "~", paste0(predictors_init, collapse = " + "), "+ (1 |", cluster, ")")
formula_new <- as.formula(formula_str)

i <- 1
out <- list()

for (i in 1:data$m) {
  formatted_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat("** Imputation dataset:", i, "\nSys.time():", formatted_time, "\n\n")
  j <- 1
  formula_new <- as.formula(formula_str)
  stop <- FALSE
  while (!stop) {
    cat("Wald: Starting While loop", j, "\n")
    model <- glmer(
      control = glmerControl(optimizer = "bobyqa"),
      formula = formula_new,
      family = binomial,
      data = complete(data, i))
    table_select <- tibble(
      term = summary(model)$coefficients %>% rownames(),
      estimate = summary(model)$coefficients[, 1],
      p_value = summary(model)$coefficients[, 4]
    )
      
  # need to account for the categorical variables
    if (any(grepl("^ZZ_MAL", table_select$term))) {
      test <- wald.test(  # requires aod library
        Sigma = vcov(model),
        b = summary(model)$coefficients[, 1],
        Terms = grep("^ZZ_MAL", names(summary(model)$coefficients[, 1])))
      table_select <- table_select %>% filter(!str_detect(term, "^ZZ_MAL"))
      add_row <- tibble(
        term = "ZZ_MAL",
        estimate = NA,
        p_value = test$result[[1]][3])
      table_select <- rbind(table_select, add_row)
    }

    if (any(grepl("^TREAT_GRP4", table_select$term))) {
      test <- wald.test(
        Sigma = vcov(model),
        b = summary(model)$coefficients[, 1],
        Terms = grep("^TREAT_GRP4", names(summary(model)$coefficients[, 1]))
      )
      table_select <- table_select %>% filter(!str_detect(term, "^TREAT_GRP4"))
      add_row <- tibble(
        term = "TREAT_GRP4",
        estimate = NA,
        p_value = test$result[[1]][3]
      )
      table_select <- rbind(table_select, add_row)
    }

    table_select <- table_select %>% 
      mutate(
        term = case_when(
          term == "DM_SEXM" ~ "DM_SEX",
          term == "VL_HISTORY1" ~ "VL_HISTORY",
          .default = term
        )
      ) %>% 
      filter(!term %in% "(Intercept)")

    # ensure age polynomials removed in correct order
    table_select_available <- table_select
    if ("ZZ_AGEs3" %in% table_select_available$term) {
        table_select_available <- table_select_available %>% filter(!term %in% c("DM_AGEs", "ZZ_AGEs2"))
      } else if ("ZZ_AGEs2" %in% table_select_available$term) {
        table_select_available <- table_select_available %>% filter(!term %in% c("DM_AGEs"))
      } 

    # force certain variables to stay in the model
    table_select_available <- table_select_available %>% 
      filter(!(term %in% keep))

    highest_p <- table_select_available[which.max(table_select_available$p_value), ]
    remove_term <- paste("~ . -", highest_p[[1]])

    if (highest_p[[3]] > p_select) {
      cat("Removing predictor: ", highest_p[[1]], ", with p-value of: ", highest_p[[3]], "\n", sep = "")
      formula_new <- update(formula_new, as.formula(remove_term))
      j <- j + 1
    } else {
      cat("There are no further terms with p >", p_select, "\n\n")
      out[[i]] <- summary(model)$coefficients %>% as_tibble(rownames = "term")
      stop <- TRUE
    }
  }
}
  
  term_count <- do.call(rbind, out) %>% 
    group_by(term) %>% 
    summarise(n = n(), prop = 100 * n() / length(out)) %>% 
    filter(prop >= 0) 

  model_count <- tab_models(out)
  term_vec <- term_count[term_count$prop >= 25, 1][[1]][-1]

  return(list(model_list = out, term_count = term_count, model_count = model_count, term_vec = term_vec))
}


safe_select_wald <- function(data, keep, p_select, predictors_init) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          # Call the select_rr function
          result <- select_wald(data, keep, p_select, predictors_init)
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
      message("\n\nCompleted execution of select_wald.")
    }
  )
}
