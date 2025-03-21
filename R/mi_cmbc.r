
#############################
## MODEL BASED CONCORDANCE ##
#############################
 
# there is poor correlation between c-mbc and within cluster auc estimates
# likely due to poor estimates to within cluster slopes & intercept?
# either way, opt for within cluster auc estimates

# van Klaveren 2019 (appendix)
# van Klaveren 2016 (formula for calculation of c-bc variance)

c_mbc <- function(lp) {

  n <- length(lp)
  numerator <- array(0, dim = c(n, n))
  denominator <- array(0, dim = c(n, n))

  for (i in 1:n) {
    for (j in 1:i) {
      if (i != j) {
        lp_i <- lp[i]
        lp_j <- lp[j]
        prob_i <- 1 / (1 + exp(-lp_i))            # probability that patient i has the outcome 
        prob_j <- 1 / (1 + exp(-lp_j))            # probability that patient j has the outcome
        numerator[i, j] <- case_when(
          lp_i <= lp_j ~ prob_j * (1 - prob_i),
          lp_i > lp_j ~ prob_i * (1 - prob_j),
          .default = NA)                           
        denominator[i, j] <- (prob_j * (1 - prob_i)) + (prob_i * (1 - prob_j))
      } else {
        numerator[i, j] <- 0
        denominator[i, j] <- 0
      }
    }
    cat("\n", i)
  }
  return(result = list(lp = lp, mbc = sum(numerator) / sum(denominator), numerator = numerator, denominator = denominator))
}

var1_cmbc <- function(cmbc) { # requires cmbc object (output from c_mbc function)
  
  n <- length(cmbc$lp)
  U1 <- array(0, dim = c(n, n))
  U2 <- array(0, dim = c(n, n))
  U1_i <- array(numeric(), dim = c(n))
  U2_i <- array(numeric(), dim = c(n))

  for (i in 1:n) {
    U1_i[i] <- (1 / (n - 1)) * sum(cmbc$numerator[, i])
    U2_i[i] <- (1 / (n - 1)) * sum(cmbc$denominator[, i])
  }

  U1_total <- (1 / n) * (1 / (n - 1)) * sum(cmbc$numerator)
  U2_total <- (1 / n) * (1 / (n - 1)) * sum(cmbc$denominator)

  nu_11 <- sum((1 / (n - 1)) * ((U1_i - U1_total)^2))
  nu_12 <- sum((1 / (n - 1)) * ((U1_i - U1_total) * (U2_i - U2_total)))
  nu_22 <- sum((1 / (n - 1)) * ((U2_i - U2_total)^2))

  s2 <- 4 * (U2_total^2 * nu_11 - 2 * U1_total * U2_total * nu_12 + U1_total^2 * nu_22) / (U2_total^4)

  var1 <- s2 / n
  return(var1)
}

var2_cmbc <- function(cmbc, vcov) { # requires cmbc object (output from c_mbc function) and cluster specific vcov matrix
  D <- numeric(2)
  D[1] <- (c_mbc(cmbc$lp + sqrt(vcov[1, 1]))$mbc - c_mbc(cmbc$lp - sqrt(vcov[1, 1]))$mbc) / (2 * sqrt(vcov[1, 1]))
  D[2] <- (c_mbc(cmbc$lp + sqrt(vcov[2, 2]))$mbc - c_mbc(cmbc$lp - sqrt(vcov[2, 2]))$mbc) / (2 * sqrt(vcov[2, 2]))
  var2 <- t(D) %*% vcov %*% D
  return(result = list(var = var2[[1]], D = D))
}

i <- 1 # imputation

model <- prog$fit$result$model_mira[[i]]
intercept <- fixef(model)[[1]]
dataset <- complete(prog$mice$result_grp, i)
lp1 <- predict(model, newdata = dataset, re.form = ~0, type = "link")
lp2 <- predict(model, newdata = dataset, re.form = NULL, type = "link")
dataset <- cbind(dataset, lp2, lp1)

model_bayes1 <- brms::brm( 
  formula = OUT_DC_RELAPSE ~  1 + lp1 + (1 + lp1 | STUDYID),
  data = dataset,
  family = bernoulli(),
  control = list(adapt_delta = 0.95)
)

summary(model_bayes1)

lp_refit <- predict(model_bayes1, newdata = dataset, re.formula = NULL, summary = TRUE)
lp_refit %>% dim()
fit_prob <- lp_refit[, 1]
fit_logit <- log(fit_prob / (1 - fit_prob))
hist(fit_logit)
dataset <- cbind(dataset, fit_logit)

dataset %>% filter(STUDYID == "6") %>% 
  ggplot() +
  geom_histogram(
    aes(
      x = fit_logit,
      fill = OUT_DC_RELAPSE
    ),
    alpha = 0.5
  )



# vcov <- attributes(ranef(model_cmbc, condVar = TRUE)$STUDYID)$postVar
clusters <- dataset %>% count(STUDYID) %>% nrow()

cmbc_final <- array(numeric(), dim = c(clusters, 7))
dimnames(cmbc_final) <- list(NULL, c("estimate", "var1", "var2", "var_tot", "se", "ci_lower", "ci_upper"))

mbc_est <- numeric(clusters)
for (c in 1:clusters) {
  lp_cluster <- dataset[dataset$STUDYID == c, "fit_logit"] 
  mbc_est[c] <- c_mbc(lp = lp_cluster)$mbc
}

auc_cluster %>% dimnames()
auc <- cbind(auc_cluster[, , 1][, c("cluster", "size", "events_no", "auc_est")], mbc_est)
#auc %>% View()
auc
