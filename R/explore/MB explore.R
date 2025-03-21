
###########################
## MB DOMAIN EXPLORATION ##
###########################

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","MB.RData"))

# The Microbiology Specimen Test Results (MB) Domain contains information about 
# microbiology specimen tests collected (e.g., malaria blood smears, blood cultures, 
# COVID-19 PCR tests). This information is collected with a single row for each test 
# (named in MBTESTCD and MBTEST) and the result for that test (in raw form within 
# MBORRES and in standardized form within MBSTRESC and MBSTRESN).
# 
# "The microbiology domains consist of Microbiology Specimen (MB) and Microbiology 
# Susceptibility (MS). The MB domain is used for the detection, identification, 
# quantification, and other characterizations of microorganisms in subject samples, 
# except for drug susceptibility testing. MS is used for representing data from 
# drug susceptibility testing on the organisms identified in MB. All non-host 
# infectious organisms, including bacteria, viruses, fungi, and parasites are 
# appropriate for the microbiology domains. One record per microbiology specimen 
# finding per time point per visit per subject"

MB %>% colnames()
missingness("MB")

# number of USUBJID per STUDYID with non-consecutive MBSEQ entries
MB_nonsec <- MB %>% 
  arrange(USUBJID, MBSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = MBSEQ == row_number()) %>% 
  mutate(nonsec = !all(test == TRUE)) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(select = all(nonsec == FALSE))
MB_nonsec %>% 
  filter(select==FALSE) %>% 
  count(STUDYID, USUBJID, nonsec) %>% count(STUDYID, nonsec) 

# four studies have non-sequential entries
# VGKSTG, VLJNYC, VLTTPJC, VQKRHN

MB %>% count(MBTESTCD, MBTEST) %>% arrange(desc(n))
MB %>% count(MBMODIFY) # for VL parasite counts 
MB %>% count(MBTSTDTL) # further details on test
MB %>% count(MBCAT) # mostly missing
MB %>% count(MBSPEC) %>% arrange(desc(n))
MB %>% count(MBLOC)
MB %>% count(MBSPEC, MBLOC) %>% arrange(desc(n))
MB %>% count(MBMETHOD) %>% arrange(desc(n))

