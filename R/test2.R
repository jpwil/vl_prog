library(mice)
library(dplyr)

# # generate data
# set.seed(201)
# b_small <-
#   brandsma %>%
#   filter(!complete.cases(.)) %>%
#   slice_sample(n = 50) %>%
#   select(ses, iqv, iqp)

formula <- paste(prog$var$result$var_final$term, collapse = " + ")
formula <- paste0("OUT_DC_RELAPSE ~ ", formula, " + (1 | STUDYID)")

imp <- prog$mice$result_grp
fit <- with(imp, glmer(formula, family = binomial()))

merBoot <- bootMer(fit$analyses[[1]], predict, nsim = 100, re.form = NA)

# obtain predictions Q and prediction variance U
df_test <- data.frame(
  DM_AGEs = 1, ZZ_AGEs2 = 1, VL_HISTORY = 0, VL_DURATIONs = 0.5, MP_BL_SPLEEN_LENGTHs2 = 4, MB_COMBINEDs = 2, TREAT_GRP4 = "SDA", OUT_DC_RELAPSE = 0)

test <- predictSE.mer(fit$analyses[[1]], newdata = complete(prog$mice$result_grp, 1), se.fit = TRUE, re.form = ~0)

predm <- lapply(getfit(fit), predict, se.fit = TRUE, re.form = ~0, newdata = data)
Q <- sapply(predm, `[[`, "fit")
U <- sapply(predm, `[[`, "se.fit")^2
#dfcom <- predm[[1]]$df

# pool predictions
pred <- matrix(NA, nrow = nrow(Q), ncol = 3,
                 dimnames = list(NULL, c("fit", "se.fit", "df")))
for(i in 1:nrow(Q)) {
  pi <- pool.scalar(Q[i, ], U[i, ], n = Inf)
  pred[i, 1] <- pi[["qbar"]]
  pred[i, 2] <- sqrt(pi[["t"]])
  pred[i, 3] <- pi[["df"]]
}


data

with(pred, plot())
