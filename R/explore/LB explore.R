
###########################
## LB DOMAIN EXPLORATION ##
###########################

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","LB.RData"))

# laboratory test results domain
# "A findings domain that contains laboratory test data such as hematology, clinical chemistry and urinalysis. This domain does not include microbiology or pharmacokinetic data, which are stored in separate domains. One record per lab test per time point per visit per subject."

# The Laboratory Test Results (LB) Domain contains information about laboratory 
# tests collected (e.g, Hemoglobin, Bilirubin, pH, G6PD Activity). This 
# information is collected with a single row for each test (named in LBTESTCD and 
# LBTEST) and the result for that test (in raw form within LBORRES and in 
# standardized form within LBSTRESC and LBSTRESN).

LB %>% colnames()
missingness("LB")

LB_nonsec <- LB %>% 
  arrange(USUBJID, LBSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = LBSEQ == row_number())
LB_nonsec %>% group_by(USUBJID) %>% 
  filter(!all(test == TRUE)) %>% 
  ungroup() %>% 
  arrange(USUBJID, LBSEQ) %>% count(STUDYID, test)

# none of VAQMOU are sequential (LBSEQ goes from 1 to 1428, i.e. it is a unique identifier)
# only some of VGKSTG are sequential (but unique witin USUBJID)

LB %>% filter(STUDYID=="VAWMOU" | STUDYID == "VGKSTG") %>% View()
#LB_nonsec %>% filter(STUDYID=="VAQMOU" | STUDYID == "VGKSTG") %>% arrange(test) %>% View()

LB %>% count(LBTESTCD) %>% arrange(desc(n)) %>% print(n = 150)

LB %>% count(LBCAT)
LB %>% count(LBSPEC)

LB %>% count(LBSTRESC) %>% arrange(desc(n))
LB %>% count(LBSTRESN) %>% arrange(desc(n))
LB %>% count(LBSTRESU) %>% arrange(desc(n))
