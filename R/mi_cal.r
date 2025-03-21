###########################
## calculate calibration ##
###########################

# this function calculates 
# (i) cluster-specific CITL: random intercept logistic regression model, with LP as offset term and CITL is the random intercept 
# (ii) cluster-specific CS: random intercept and slope 

safe_cal <- function(prog, data_eval, boot, cal_cond) {
  warnings_list <- character() # initialise empty vector to store warnings
  result <- NULL 
  time_start <- Sys.time()

  tryCatch(
    {
      withCallingHandlers(
        {
          result <- cal(prog, data_eval, boot, cal_cond)
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
    finally = {
      # Code that will always run, even if there is an error or warning
    }
  )
}

cal <- function(prog, data_eval, boot, cal_cond) { 

  if (is.null(prog$mice$result)) {
    message("\nMICE FAILED")
    return(NULL)
  }

  imputations <- data_eval$m
  clust_no <- length(unique(complete(data_eval, 1)$STUDYID))
  model <- prog$fit$result$model_sum

## CLUSTER LEVEL / CONDITIONAL CALIBRATION MEASURES
## USING AVERAGE RANDOM-INTERCEPT PREDICTIONS
## ONLY DO THIS FOR ORIGINAL MODEL

  model_citl <- list()
  model_cs <- list()

  intercept <- array(numeric(), dim = c(imputations, 2, clust_no))
  slope <- array(numeric(), dim = c(imputations, 2, clust_no))

  intercept_fixed <- array(numeric(), dim = c(imputations, 2))
  slope_fixed <- array(numeric(), dim = c(imputations, 2))

  dimnames(intercept) <- list(paste0("imp", 1:imputations), c("estimate", "se"), paste0("clust", 1:clust_no))
  dimnames(slope) <- dimnames(intercept)

  dimnames(intercept_fixed) <- list(paste0("imp", 1:imputations), c("estimate", "se"))
  dimnames(slope_fixed) <- dimnames(intercept_fixed)

  if (cal_cond) {
    for (i in 1:imputations) {
      cat("\nwithin-cluster calibration modelling, imputation: ", i, "\n")
      dataset <- complete(data_eval, i)
      lp <- predict_lp(data = dataset, model) # average random intercept (set b_j = 0)
      dataset <- cbind(dataset, lp)

      model_citl[[i]] <- glmer(
        formula = OUT_DC_RELAPSE ~ offset(lp) + (1 | STUDYID),
        data = dataset,
        family = binomial(),
        glmerControl(optimizer = "bobyqa")
      )

      model_cs[[i]] <- glmer(  # the glmer method does not converge, use multilevel Bayesian model
        formula = OUT_DC_RELAPSE ~  1 + lp + (1 + lp | STUDYID),
        data = dataset,
        family = binomial(),
        glmerControl(optimizer = "bobyqa")
      )

    intercept[i, 1, ] <- lme4::ranef(model_citl[[i]])$STUDYID[[1]] + lme4::fixef(model_citl[[i]])[[1]]
    intercept[i, 2, ] <- sqrt(attr(lme4::ranef(model_citl[[i]])$STUDYID, "postVar")[1, 1, ])
    intercept_fixed[i, 1] <- summary(model_citl[[i]])$coefficients[1]
    intercept_fixed[i, 2] <- summary(model_citl[[i]])$coefficients[2]

    slope[i, 1, ] <- lme4::fixef(model_cs[[i]])[2] + lme4::ranef(model_cs[[i]])$STUDYID[, 2]  # Bayes' estimates for cluster-specific slope
    slope[i, 2, ] <- sqrt(attr(lme4::ranef(model_cs[[i]])$STUDYID, "postVar")[2, 2, ])  # Standard error for cluster-specific slope
    slope_fixed[i, 1] <- summary(model_cs[[i]])$coefficients[2, 1]
    slope_fixed[i, 2] <- summary(model_cs[[i]])$coefficients[2, 2]
    }
  }

## POPULATION LEVEL / STANDARD CALIBRATION MEASURES
## USING AVERAGE RANDOM-INTERCEPT PREDICTIONS
## ALWAYS DO THIS

  model_citl_pop <- list()
  model_cs_pop <- list()

  intercept_pop <- array(numeric(), dim = c(imputations, 2))
  slope_pop <- array(numeric(), dim = c(imputations, 2))

  dimnames(intercept_pop) <- list(paste0("imp", 1:imputations), c("estimate", "se"))
  dimnames(slope_pop) <- dimnames(intercept_fixed)

  for (i in 1:imputations) {
    cat("\nStandard calibration modelling, imputation: ", i, "\n")
    dataset <- complete(data_eval, i)
    lp <- predict_lp(data = dataset, model) # average random intercept (set b_j = 0)
    dataset <- cbind(dataset, lp)

    model_citl_pop[[i]] <- glm(
      formula = OUT_DC_RELAPSE ~ offset(lp),
      data = dataset,
      family = binomial()
    )

    model_cs_pop[[i]] <- glm(
      formula = OUT_DC_RELAPSE ~  lp,
      data = dataset,
      family = binomial(),
    )

    intercept_pop[i, 1] <- coef(model_citl_pop[[i]]) 
    intercept_pop[i, 2] <- sqrt(vcov(model_citl_pop[[i]]))
    slope_pop[i, 1] <- coef(model_cs_pop[[i]])[2]
    slope_pop[i, 2] <- sqrt(vcov(model_cs_pop[[i]])[2, 2])
  }

return(result = 
  list(
    intercept = intercept, 
    intercept_fixed = intercept_fixed, 
    slope = slope, 
    slope_fixed = slope_fixed, 
    slope_pop = slope_pop, 
    intercept_pop = intercept_pop))
}
