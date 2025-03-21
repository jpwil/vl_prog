###########################
## TI DOMAIN EXPLORATION ##
###########################

# Trial inclusion / exclusion criteria domain

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","TI.RData"))

missingness("TI")
colnames(TI)
