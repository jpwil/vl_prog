
###########################
## PT DOMAIN EXPLORATION ##
###########################

# Per-protocol treatments and interventions domain

# "An interventions domain that contains information about protocol-specified 
# study treatment administrations, as collected (Exposure as Collected Domain).
# One record per intervention occurrence per subject."
# 
# This is an IDDO-created Custom Domain (non-standard SDTM implementation). 
# Information on the per-protocol treatment administrations that subjects are 
# supposed to be given are found in this custom domain. THIS IS NOT "REAL" DATA 
# AND IS PULLED FROM STUDY DOCUMENTATION.

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","PT.RData"))

missingness("PT")

# number of USUBJID per STUDYID with non-consecutive PTSEQ entries
PT_nonsec <- PT %>% 
  arrange(USUBJID, PTSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = PTSEQ == row_number()) %>% 
  mutate(nonsec = !all(test == TRUE)) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(select = all(nonsec == FALSE))
PT_nonsec %>% 
  filter(select==FALSE) %>% 
  count(STUDYID, USUBJID, nonsec) %>% count(STUDYID, nonsec) 

PT %>% count(PTGRPID) %>% arrange(desc(n)) %>% print(n = Inf)
PT %>% count(PTTRT) %>% arrange(desc(n)) %>% print(n = Inf)
