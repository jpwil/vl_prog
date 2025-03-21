
###########################
## QS DOMAIN EXPLORATION ##
###########################

# "A findings domain that contains data for named, stand-alone instruments 
# designed to provide an assessment of a concept. Questionnaires have a defined 
# standard structure, format, and content; consist of conceptually related items 
# that are typically scored; and have documented methods for administration and 
# analysis. One record per questionnaire per question per time point per visit 
# per subject."

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","QS.RData"))

missingness("QS")

# number of USUBJID per STUDYID with non-consecutive QSSEQ entries
QS_nonsec <- QS %>% 
  arrange(USUBJID, QSSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = QSSEQ == row_number()) %>% 
  mutate(nonsec = !all(test == TRUE)) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(select = all(nonsec == FALSE))
QS_nonsec %>% 
  filter(select==FALSE) %>% 
  count(STUDYID, USUBJID, nonsec) %>% count(STUDYID, nonsec) 

QS %>% count(QSTESTCD, QSTEST) # these are all KPSS (Karnofsky Performance Status)
QS %>% count(QSCAT)
QS %>% count(QSORRES)
