###########################
## IN DOMAIN EXPLORATION ##
###########################

# Treatments and Interventions Domain

# "An interventions domain that contains concomitant and prior medications used 
# by the subject, such as those given on an as needed basis or condition-appropriate
# medications (Concomitant and Prior Medications Domain).​ An interventions domain 
# that contains the details of a subject's exposure to protocol-specified study 
# treatment. Study treatment may be any intervention that is prospectively defined
# as a test material within a study, and is typically but not always supplied to 
# the subject (Exposure Domain). An interventions domain that contains information
# about protocol-specified study treatment administrations, as collected (Exposure
# as Collected Domain). One record per recorded intervention occurrence per 
# subject."

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","IN.RData"))

missingness("IN")

# not all INSEQ are in sequence. 
IN_nonsec <- IN %>% 
  arrange(USUBJID, INSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = INSEQ == row_number())
IN_nonsec %>% group_by(USUBJID) %>% 
  filter(!all(test == TRUE)) %>% 
  ungroup() %>% 
  arrange(USUBJID, INSEQ) %>% count(STUDYID, test)

IN_nonsec %>% filter(test==FALSE) %>% View()

# VKRFLS_HUMERA_249 only has one row and INSEQ = 2
# VQKRHN - USUBJID includes dose information

## INDECOD - inconsistently coded
# This is not required for all INTRT terms but should be completed for all 
# interventional treatments and for any concomitant treatment where more than 
# 10 patients receive the treatment.

IN %>% count(INTRT, INDECOD) %>% arrange(desc(n))

## INCAT
IN %>% count(INTRT, INCAT) %>% arrange(desc(n)) %>% print(n = Inf)
IN %>% count(INCAT) %>% arrange(desc(n)) %>% print(n = Inf)
