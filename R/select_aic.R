# this function performs backwards stepwise selection for the 
# fixed terms of a glmm (glmer object), using the AIC. 

select_aic <- function(initial_model, keep = NULL) {

# initiate terms
current_model <- initial_model
current_aic <- AIC(initial_model)
improved <- TRUE
i <- 1

temp <- list()
output <- data.frame(
  iter = integer(0),              # iteration
  var = character(0),             # variable name dropped
  aic_with = numeric(0),          # aic with variable in the model
  aic_without = numeric(0)        # aic with variable dropped
)

while (improved) {
  print(paste0("Started while loop ", i))

  # Use drop1 to assess the impact of dropping each fixed effect
  model_drop <- drop1(current_model, test = "Chisq")
  
  # edge case - if no variables to drop
  if (nrow(model_drop) == 0) {
    print("No more variables to drop.")
    improved <- FALSE 
    next
  }

  # Create a list of valid terms to drop (can only remove the higher polynomial terms for age)
  valid_terms_to_drop <- setdiff(rownames(model_drop), keep)

  if("I(DM_AGEs^3)" %in% valid_terms_to_drop) {
    valid_terms_to_drop <- setdiff(valid_terms_to_drop, c("I(DM_AGEs^2)", "I(DM_AGEs)"))
  }

  if("I(DM_AGEs^2)" %in% valid_terms_to_drop && !("I(DM_AGEs^3)" %in% valid_terms_to_drop)) {
    valid_terms_to_drop <- setdiff(valid_terms_to_drop, c("I(DM_AGEs)"))
  }

  model_drop <- model_drop[rownames(model_drop) %in% valid_terms_to_drop, , drop = FALSE]

  # Find the variable with the smallest AIC when dropped
  min_aic <- which.min(model_drop$AIC)

  # Check if the AIC improves
  if (model_drop$AIC[min_aic] < current_aic) {
    # Update the AIC to the improved value
    current_aic <- model_drop$AIC[min_aic]
    
    # Identify the variable to drop
    drop_var <- rownames(model_drop)[min_aic]
    
    # Update the formula to exclude the variable that improved AIC the most
    new_formula <- as.formula(
      paste(". ~ . -", drop_var)
    )
      
    # Create output
    output[i, "iter"] <- i
    output[i, "var"] <- drop_var
    output[i, "aic_with"] <- AIC(current_model)
    output[i, "aic_without"] <- current_aic


    print(paste0("AIC with variable: ", AIC(current_model)))
    print(paste0("AIC without variable: ", current_aic))
    print(paste0("Variable removed: ", rownames(model_drop)[min_aic]))
    print(paste0("Iteration ", i, " completed"))
    print(paste0(improved))

    i <- i + 1
    # Refit the model without the dropped variable
    current_model <- update(current_model, new_formula)

  } else {
    # No improvement, exit the loop
    improved <- FALSE
  }
}
  temp$iterations <- output
  temp$start_model <- initial_model
  temp$end_model <- current_model
  return(temp)
}