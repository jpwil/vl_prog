library(tidyverse)
library(lme4)
library(micemd)

source("R/explore_meta.R")
source("R/compile_models.R")
source("R/compile_probs.R")

setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/R/vl_relapse_model/") 
folders <- list.dirs("results/main", full.names = TRUE, recursive = FALSE)
folders
folder <- folders[6]

files <- sort(list.files(folder, pattern = "^[0-9]{10}\\.rds$", full.names = TRUE))
files

model_test <- readRDS(files[1])
model$mice$result_grp$data %>% names()

model_test <- list()
tau2 <- list()
icc <- list()

# MODEL WITH NO COVARIATES
for (i in 1:model$mice$result_grp$m) {
  print(i)
  model_test[[i]] <- glmer(
    data = complete(model$mice$result_grp , i),
    formula = OUT_DC_RELAPSE ~ (1 | STUDYID), 
    family = binomial()
  )
  tau2[[i]] <- VarCorr(model_test[[i]])$STUDYID[1, 1]
  icc[[i]] <- tau2[[i]] / (tau2[[i]] + pi^2 / 3)
}

# MODEL WITH COVARIATES

# define formula from variable selection
formula <- paste(model$var$result$var_final$term, collapse = " + ")
formula <- paste0("OUT_DC_RELAPSE ~ ", formula, " + (1 | STUDYID)")
formula <- as.formula(formula)
formula

model_test <- list()
tau2 <- list()
icc <- list()

# MODEL WITH NO COVARIATES
for (i in 1:model$mice$result_grp$m) {
  print(i)
  model_test[[i]] <- glmer(
    data = complete(model$mice$result_grp , i),
    formula = formula, 
    family = binomial()
  )
  tau2[[i]] <- VarCorr(model_test[[i]])$STUDYID[1, 1]
  icc[[i]] <- tau2[[i]] / (tau2[[i]] + pi^2 / 3)
}

icc
