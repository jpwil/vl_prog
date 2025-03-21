
rm(list = ls())
packages <- c("lme4", "micemd", "anthro", "anthroplus", "tidyverse", "broom.mixed", "pROC", "meta", "boot")
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}

files <- list.files(path = "Analysis/MI/", pattern = "^[0-9]{10}_MICE.rdata$", full.names = TRUE) 
files
load(file = files[[15]])

source("Analysis/MI/mi_prepare.R")        ## PREPARE DATASET
source("Analysis/MI/mi_initialise.R")     ## INITIALISE MI PARAMETERS
source("Analysis/MI/mi_summarise.R")      ## INITIALISE MICE SUMMARY
source("Analysis/MI/mi_post_grouping.R")  ## GROUP MALNUTRITION & ANAEMIA
source("Analysis/MI/select_rr.r")         ## VARIABLE SELECTION (RUBIN'S RULES)
source("Analysis/MI/model_fit.R")         ## FIT MODEL 
source("Analysis/MI/mi_cal.r")            ## PERFORMANCE: CALIBRATION
source("Analysis/MI/mi_cstat_ps.r")       ## PERFORMANCE: CSTAT

# look


prog$cstat <- safe_cstat_apparent(prog)