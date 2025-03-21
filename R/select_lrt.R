# this function performs backwards stepwise for the 
# fixed terms of a glmm (glmer object), using LRT 

# model <- readRDS(files[[1]])
select_lrt <- function(data, keep = NULL, p_value) {
  outcome <- "OUT_DC_RELAPSE"
  cluster <- "STUDYID"
  predictors_init <- c("DM_SEX", "DM_AGEs", "ZZ_AGEs2", "ZZ_AGEs3", "TREAT_GRP4", "ZZ_MAL", "LB_BL_HGB_GRP3", "VL_HISTORY", "VL_DURATIONs", "LB_BL_WBCs", "LB_BL_PLATs", "LB_BL_ALTs", "LB_BL_CREATs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs")
  cat <- c("TREAT_GRP4", "ZZ_MAL")

  formula_str <- paste0(outcome, "~", paste0(predictors_init, collapse = " + "), "+ (1 |", cluster, ")")

  # initiate terms
  improved <- TRUE
  temp <- list()
  output <- data.frame(
    iter = integer(),              # iteration
    var = character(),             # variable name dropped
    chi2 = numeric())               # chi2 value
  table_select <- list()

  for (j in 1:data$m) {

    improved <- TRUE
    i <- 1
    formula_new <- as.formula(formula_str)
    cat("Starting imputation", j, "\n\n")

    while (improved) {
      cat("LRT: Started while loop ", i, "\n")

      # Use drop1 to assess the impact of dropping each fixed effect
      current_model <- glmer(
        formula = formula_new,
        family = binomial,
        data = complete(data, j)) 

      model_drop <- drop1(current_model, test = "Chisq")
      
      # edge case - if no variables to drop
      if (nrow(model_drop) == 0) {
        cat("No more variables to drop.", "\n")
        improved <- FALSE 
        next
      }

      # Create a list of valid terms to drop (can only remove the higher polynomial terms for age)
      valid_terms_to_drop <- setdiff(rownames(model_drop), keep)
      #cat("Valid terms to drop (before):", valid_terms_to_drop, "\n")
      if ("ZZ_AGEs3" %in% valid_terms_to_drop) {
        valid_terms_to_drop <- setdiff(valid_terms_to_drop, c("ZZ_AGEs2", "DM_AGEs"))
      }

      if ("ZZ_AGEs2" %in% valid_terms_to_drop && !("ZZ_AGEs3)" %in% valid_terms_to_drop)) {
        valid_terms_to_drop <- setdiff(valid_terms_to_drop, c("DM_AGEs"))
      }

      #cat("Valid terms to drop (after):", valid_terms_to_drop, "\n")
      model_drop <- model_drop[rownames(model_drop) %in% valid_terms_to_drop, , drop = FALSE]

      # Find the variable with the highest Chi2 probability
      max_pr_chi2 <- which.max(model_drop[["Pr(Chi)"]])

      # Check if the p improves
      if (model_drop[["Pr(Chi)"]][max_pr_chi2] > p_value) {
        # Identify the variable to drop
        drop_var <- rownames(model_drop)[max_pr_chi2]
        
        # Update the formula to exclude that variable
        remove_term <- paste("~ . -", drop_var)
          
        # Create output
        #output[i, "iter"] <- i
        #output[i, "var"] <- drop_var
        #output[i, "chi2"] <- model_drop[["Pr(Chi)"]][max_pr_chi2]

        cat("Completed while loop", i, "\n")
        cat("Variable dropped:", drop_var, "\n")
        cat("Chi squared prob:", model_drop[["Pr(Chi)"]][max_pr_chi2], "\n") 

        i <- i + 1
        # Refit the model without the dropped variable
        formula_new <- update(formula_new, as.formula(remove_term))
      } else {
        # No improvement in model, remember current model and exit while loop
        cat("Completing imputation:", j, "\n\n")
        improved <- FALSE
        table_select[[j]] <- summary(current_model)$coefficients %>% as_tibble(rownames = "term")
      }
    }
  }
  return(table_select)
}

