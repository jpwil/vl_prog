############
## VCNXEB ##
############

rm(list = ls())
library(tidyverse)

# MSF Observational study
# Uganda
# Amudat: 180

# Mueller Y, Nguimfack A, Cavailler P, Couffignal S, Rwakimari JB, Loutan L, Chappuis F. 
# Safety and effectiveness of amphotericin B deoxycholate for the treatment of visceral leishmaniasis in Uganda. 
# Ann Trop Med Parasitol. 2008 Jan;102(1):11-9. doi: 10.1179/136485908X252142. PMID: 18186974.

# Observational study of amphotericin B use in Amudat (most patients actually from Kenya)
# Comparison with historical cohort from same hospital treated with meglumine
# MSF study
# Not prospective - no consent
# 180/210 amphotericin B deoxycholate patients in the dataset

rm(list = ls())
source("R/definitions.R")
load_domains("VCNXEB")

## DM
DM %>% count()

DM %>% names()
DM %>% count(ARM)


#######
# NOT PROSPECTIVE
# NO ACTIVE PATIENT FOLLOW-UP
# THERE WERE 10 DEATHS AND 6 TREATMENT FAILURES 
# MANUSCRIPT DOES NOT DISTINGUISH BETWEEN RELAPSE AND TREATMENT FAILURE