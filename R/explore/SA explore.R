
###########################
## SA DOMAIN EXPLORATION ##
###########################

# "An events domain that contains data describing untoward medical occurrences in 
# a patient or subjects that are administered a pharmaceutical product and which 
# may not necessarily have a causal relationship with the treatment (Adverse 
# Events Domain). An events domain that contains clinical events of interest 
# that would not be classified as adverse events (Clinical Events Domain). One 
# record per event per subject."

# This is an IDDO-created Custom Domain (non-standard SDTM implementation). All 
# variables found in the Adverse Events (AE) and Clinical Events (CE) domains are 
# found in this custom domain.

# The Symptoms and Adverse Events (SA) Domain contains information on clinical 
# events that may or may not be defined as Adverse Events by the contributor as 
# well as previous events (i.e., medical history). This information is collected
# as a single row for each event described in SATERM with information about that
# event on the same row (e.g., a categorization of the event (SACAT, SASCAT), 
# whether a pre-specified event occurred (SAPRESP=Y, SAOCCUR), the severity or 
# seriousness of the event (SASEV, SASER), and the timing of the event 
# (SASTDTC, SAENDTC, SADUR, SASTRF, etc.).

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","SA.RData"))

missingness("SA")

# number of USUBJID per STUDYID with non-consecutive SASEQ entries
SA_nonsec <- SA %>% 
  arrange(USUBJID, SASEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = SASEQ == row_number()) %>% 
  mutate(nonsec = !all(test == TRUE)) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(select = all(nonsec == FALSE))
SA_nonsec %>% 
  filter(select==FALSE) %>% 
  count(STUDYID, USUBJID, nonsec) %>% count(STUDYID, nonsec) 

SA %>% count(STUDYID, SAREFID) %>% print(n = Inf) # only VVNGOE
SA %>% count(SATERM, SACAT) %>% arrange(desc(n)) %>% print(n = 100)
SA %>% count(SAPRESP)

# whether the question was part of the CRF, and response
SA %>% count(SAPRESP, SAOCCUR)
SA %>% count(SAPRESP, SASTAT) # NOT DONE = question was not answered
SA %>% count(SASTAT, SAREASND) # Reason for question not being answered never provided
SA %>% count(SACAT, SAPRESP)

