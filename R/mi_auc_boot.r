## create a generic function to perform stratified bootstrapping of the AUC
## this is only needed for calculating overall c_within (because we sum numerator + denominator across clusters)

# load test parameters
model <- prog$fit$result$model_mira[[1]]
dataset <- complete(prog$mice$result_grp, 1)

predictor <- predict(model, re.form = NA, type = "link")
outcome <- dataset[, "OUT_DC_RELAPSE"]

dataset <- data.frame(dataset, predictor)
dataset %>% names()

# perform bootstrapping for overall within-cluster c-index
# we need to maintain the same number of patients in each cluster
# and within each cluster, maintain the number of cases and controls

st1 <- system.time(boot_auc_total_wrap(dataset, outcome = "OUT_DC_RELAPSE", R = 20, parallel = "no"))
st2 <- system.time(boot_auc_total_wrap(dataset, outcome = "OUT_DC_RELAPSE", R = 20, parallel = "multicore"))
st3 <- system.time(boot_auc_total_wrap(dataset, outcome = "OUT_DC_RELAPSE", R = 20, parallel = "multicore"))

boot_auc_total <- function(data, indices, outcome, predictor) {
  resampled_data <- data[indices, ]
  auc_tot <- auc_total(resampled_data, outcome = outcome, predictor = predictor)
  return(auc_tot)
}

boot_auc_total_wrap <- function(dataset, R = 50, parallel = "no", outcome = "outcome", predictor = "predictor") {
  boot_out <- boot(
    data = dataset, 
    statistic = boot_auc_total, 
    R = R, 
    strata = as.integer(dataset[, outcome]),
    parallel = parallel,
    outcome = outcome,
    predictor = predictor)
  return(boot_out)
}

# start with a function that simply calculates the auc (outputs numerator and denominator, 
# so we can use for both cluster and overall 'within' cluster concordance estimate

auc_total <- function(dataset, outcome, predictor) {
  return(auc_num_den(dataset[, outcome], dataset[, predictor])[[1]] / auc_num_den(dataset[, outcome], dataset[, predictor])[[2]])
  }

auc_num_den <- function(outcome, predictor) { # this is still slower than pROC method
  pred_diff <- outer(predictor, predictor, ">")  
  outcome_diff <- outer(outcome, outcome, function(x, y) x & !y)  
  return(list(num = sum(pred_diff & outcome_diff), den = sum(outcome_diff)))
  }
