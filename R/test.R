
######################
## LOAD PROG OBJECT ##
######################

rm(list = ls())
packages <- c("lme4", "micemd", "anthro", "anthroplus", "tidyverse", "broom.mixed", "pROC", "meta", "boot")
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}

files <- list.files(path = "Analysis/MI/", pattern = "^[0-9]{10}_MICE.rdata$", full.names = TRUE) 
files
load(file = files[[6]])
mi_summarise(prog)

source("Analysis/MI/mi_prepare.R")        ## PREPARE DATASET
source("Analysis/MI/mi_initialise.R")     ## INITIALISE MI PARAMETERS
source("Analysis/MI/mi_summarise.R")      ## INITIALISE MICE SUMMARY
source("Analysis/MI/mi_post_grouping.R")  ## GROUP MALNUTRITION & ANAEMIA
source("Analysis/MI/select_rr.r")         ## VARIABLE SELECTION (RUBIN'S RULES)
source("Analysis/MI/model_fit.R")         ## FIT MODEL 
source("Analysis/MI/mi_cal.r")            ## PERFORMANCE: CALIBRATION
source("Analysis/MI/mi_cstat_ps.r")          ## PERFORMANCE: CSTAT

# look at random intercept and random slope models

data <- complete(prog$mice$result_grp, "long")


df_cal <- data.frame(data[, c("OUT_DC_RELAPSE", "STUDYID")], lp)
colnames(df_cal) <- c("response", "dataset", "predictor")

# calibration intercept for STUDYID = 16 (ie we are not interested in calibration-in-the-large)
model_adjust <- glm(
  formula = response ~ offset(predictor),
  data = df_cal[df_cal$dataset == 16, ],
  family = binomial()
)
citl_adjust <- coef(model_adjust)[[1]]

df_cal[, "predictor"] <- df_cal[, "predictor"] + citl_adjust
df_cal[, "prob"] <- 1 / (1 + exp(-df_cal[, "predictor"]))

#hist(df_cal[, "predictor"])
#hist(df_cal[, "prob"])
plot(df_cal[, "predictor"],df_cal[, "prob"] )

