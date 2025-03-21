# EXCLUDE due to data provenance concerns

##########
# VHNNMS #
##########

# PUBLICATION

# Sundar S, Singh A, Agrawal N, Chakravarty J. Effectiveness of Single-Dose Liposomal Amphotericin B in Visceral Leishmaniasis in Bihar. 
# Am J Trop Med Hyg. 2019 Oct;101(4):795-798. doi: 10.4269/ajtmh.19-0179. PMID: 31436156; PMCID: PMC6779195.


# This listed is the same study as VSGPDL

# DATASET

# In CVBE, this trial is described as the ABSG study (non-trial). "ABSG Study for the use of Ambisome among VL Patients (non-Trial)" and 
# "Data published together with Jiv Daya Foundation Study". Although it appears the described patients in the dataset were recruited after the 
# reported trial dates of the (above) publication

# VHNNMS: May 2016 - Feb 2018 
# VSGPDL: June 2013 - Jan 2015

# From publication - study performed: June 2013 - Feb 2017 (implying latest RFSTDTC of Feb 2016)
# So patients in VHNNMS can't have been included in the publication
# Also, there are no relapse / deaths / adverse outcomes reported in VHNNMS (would expect at least some adverse outcomes / relapses over 6 months if n = 274)
# This raises the question about actve follow-up, especially if described as 'non-trial' (also implies no patent consent which is an entry criteria for this IPD-MA)
# For these reasons, probably best to exclude from the analysis - unless KAMRC state clearly they used the same protocol & follow-up. 
# Also, there are no outcomes except for CURE / COMPLETE RECOVERY (not a single relapse documented). 

rm(list = ls())
source("definitions.R")
#load_domains("VSGPDL")


mg <- ld_missingness("VHNNMS")
mg %>%
  count_na() %>%
  print(width = Inf)


# check whether same patient IDs as VSGPDL
load_domains("VHNNMS")
DM_VHNNMS <- DM
DM_VHNNMS %>% dim()

load_domains("VSGPDL")
DM_VSGPDL <- DM
DM_VSGPDL %>% dim()

# none of the IDs match
DM_VHNNMS %>% full_join(DM_VSGPDL) %>% dim() 
DM_VHNNMS %>% pull(USUBJID) %>% head(10)
DM_VSGPDL %>% pull(USUBJID) %>% head(10)

# all outcomes are CURE / COMPLETE RECOVERY in RS domain
# all outcomes are CURE / Missing data in DS domain
# no deaths in DM domain, and no DD domain
load_domains("VHNNMS")
RS %>% count()
RS %>% count_na() %>% print(width = Inf) # 2 patients have RSSTRTPT and RSSTTPT variables populated

RS %>% names()
RS %>% count_dup2(VISIT, RSSTTPT)

# these are the 2 duplicate patients
RS %>% select(USUBJID, RSSTTPT) %>% arrange(RSSTTPT)

DS %>% names()
DS %>% filter(is.na(DSSTTPT)) %>%  count(VISIT, DSTERM) 

DM %>% names()

# let's see if we can match the patients using SEX, AGE, and RFSTDTC
DM_VSGPDL %>% select(USUBJID, RFSTDTC, AGE, SEX) %>% mutate(RFSTDTC = dmy(RFSTDTC)) %>% arrange(RFSTDTC, AGE, SEX) %>% View()
DM_VHNNMS %>% select(USUBJID, RFSTDTC, AGE, SEX) %>% mutate(RFSTDTC = dmy(RFSTDTC)) %>% arrange(RFSTDTC, AGE, SEX) %>% View()

# VSGPDL - RFSTDTC from 15th June 2013 - 26th January 2015
# VHNNMS - RFSTDTC from 23rd May 2016 - 8th February 2018

# If RFSTDTC is correct; these patients do not correspond to the Sundar 2019 single dose amphotericin study
DM_VHNNMS %>% count(ARM)
PT %>% names()
PT %>% count(STUDYID)
PT %>% count(PTTRT) # all Ambisome
PT %>% count(PTDOSE, PTDOSU) # all 10 mg
PT %>% count(PTDOSFRQ)

TS #%>% View()
SA %>% count(VISIT, SATERM, SAOCCUR) 
