
###########################
## RP DOMAIN EXPLORATION ##
###########################

# The Reproductive System Findings (RP) Domain contains information about the 
# subject's reproductive ability (like Last Menstrual Period or Childbearing Potential) 
# and history (like Number of Pregnancies and Pregnant Indicator). This information 
# is collected with a single row for each finding (named in RPTESTCD and RPTEST) 
# and the result for that finding (in raw form within RPORRES and in standardized
# form within RPSTRESC and RPSTRESN).

# "A findings domain that contains physiological and morphological findings 
# related to the male and female reproductive systems. One record per finding
# or result per time point per visit per subject."

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","RP.RData"))

missingness("RP")

# number of USUBJID per STUDYID with non-consecutive RPSEQ entries (all consecutive!)
RP_nonsec <- RP %>% 
  arrange(USUBJID, RPSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = RPSEQ == row_number()) %>% 
  mutate(nonsec = !all(test == TRUE)) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(select = all(nonsec == FALSE))
RP_nonsec %>% 
  filter(select==FALSE) %>% 
  count(STUDYID, USUBJID, nonsec) %>% count(STUDYID, nonsec) 

RP %>% count(RPTESTCD, RPTEST)
RP %>% count(RPCAT)
RP %>% count(RPORRES) # can identify some positive pregnancy tests here
