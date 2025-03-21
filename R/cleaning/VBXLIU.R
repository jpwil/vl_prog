############
## VBXLIU ##
############

rm(list = ls())
library(tidyverse)

# DNDi RCT
# Ethiopia, Sudan
# Arba Minch: 50
# Gondar: 38
# Kassab: 36

# Khalil EA, Weldegebreal T, Younis BM, Omollo R, Musa AM, Hailu W, Abuzaid AA, Dorlo TP, Hurissa Z, Yifru S, Haleke W, Smith PG, Ellis S, Balasegaram M, EL-Hassan AM, Schoone GJ, Wasunna M, Kimutai R, Edwards T, Hailu A. 
# Safety and efficacy of single dose versus multiple doses of AmBisome for treatment of visceral leishmaniasis in eastern Africa: a randomised trial. 
# PLoS Negl Trop Dis. 2014 Jan 16;8(1):e2613. doi: 10.1371/journal.pntd.0002613. PMID: 24454970; PMCID: PMC3894173.

# 124 randomised (correct number)
# DNDi study looking at SDA vs MDA, published 2014
# Relatively small study, terminated early due to poor efficacy

# Arm 1: MDA
# Arm 2: SDA 7.5
# Arm 3: SDA 10

# 92 achieved initial cure (one pt excluded in MDA arm as not VL diagnosis, 9 failed in MDA arm, 11 in SDA 7.5 arm, and 11 in SDA 10 arm)
# 77 achieved definitive cure (8 patients LTFU in MDA arm, but do these include those who failed treatment at initial cure?, ? relapses in MDA arm, 2 relapses SDA 7.5 arm, 1 non-VL death SDA 7.5 arm, 6 relapses SDA 10 arm)
 
# at least 8 relapses (2 in SDA 7.5 arm and 6 in SDA 10 arm), perhaps more relapses in MDA arm, but impossible to distinguish between relapse and LTFU
# 1 non-VL death in SDA 7.5 arm

## SLOW RESPONDERS - not desrcibed in the dataset

rm(list = ls())
source("R/definitions.R")
load_domains("VBXLIU")

## DM

anyDuplicated(DM$SUBJID)
anyDuplicated(DM$USUBJID)

DM %>% count(DTHDTC) # this must be the non-VL death between IC and DC
DM %>% count(ARMCD, ARM)
DM %>% count(SITEID)
DM %>% count(SEX)
DM %>% count(is.na(AGE))

DM_CLEAN <- DM %>%
  rename(
    DM_SEX = SEX,
    DM_SITE = SITEID,
    DM_AGE = AGE,
    DM_ARM = ARMCD
  ) %>%
  dplyr::select(starts_with("DM_"), USUBJID, RFSTDTC)

## Planned treatments

PT %>% count()
PT %>% count(PTDOSRGM, PTDOSE)
PT %>% count(USUBJID, PTDOSRGM, PTDOSE)  %>% count(PTDOSE)

# need to distinguish between 7.5mg/kg and 10mg/kg L-AmB single dose. 
PT %>% select(USUBJID, PTDOSE) %>% mutate(PTDOSE = as.character(PTDOSE))
DM_CLEAN <- DM_CLEAN  %>% full_join(PT %>% select(USUBJID, PTDOSE) %>% mutate(PTDOSE = as.character(PTDOSE)) %>% unique()) %>% 
  mutate(DM_ARM = str_c(DM_ARM, PTDOSE)) %>% select(-PTDOSE) 

### OUTCOMES

# RS - no data for two patients
unique(RS$USUBJID) %>% length()
RS %>% group_by(RSTESTCD) %>% summarise(dup = sum(duplicated(USUBJID)))
RS %>% count(RSTESTCD, RSORRES, RSSTRESC)

OUT <- RS %>% pivot_wider(
  id_cols = USUBJID,
  names_from = c(RSTESTCD),
  names_glue = "RS_{RSTESTCD}",
  values_from = RSORRES) 

# DM - merge with one death
OUT <- OUT %>% merge(DM %>% dplyr::select(USUBJID, DTHFL), all = TRUE)

# PT - merge with planned treatment regimen
OUT <- OUT %>% merge(PT %>% count(USUBJID, PTDOSRGM, PTDOSE) %>% select(USUBJID, PTDOSE), all = TRUE)

# IN - merge with relapse medication
OUT <- OUT %>% merge(IN %>% filter(EPOCH == "FOLLOW-UP") %>% count(USUBJID, INTRT, INSTDY) %>% filter(!is.na(INSTDY)) %>% select(-n), all = TRUE)

# DS - one entry per patient
sum(duplicated(DS$USUBJID))
DS %>% count(DSTERM, DSDECOD)

OUT <- OUT %>% merge(DS %>% dplyr::select(USUBJID, DSTERM), by = "USUBJID", all = TRUE)

# MB

anyDuplicated(MB$USUBJID)

MB %>% group_by(VISIT) %>% summarise(sum(duplicated(USUBJID)))
OUT <- OUT %>% merge(
  MB %>% pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(MBLOC, MBORRES, MBDY),
    names_vary = "slowest"
  ))

OUT <- OUT %>% merge(DM_CLEAN %>% select(USUBJID, DM_SITE), all = TRUE) %>% arrange(DM_SITE, PTDOSE, RS_TOC) %>% 
  relocate(USUBJID, PTDOSE, DM_SITE) 

OUT %>% arrange(PTDOSE, RS_TOC, DSTERM)  #%>% View()

OUT <- OUT %>% 
  mutate(
    OUT_IC = ifelse((!is.na(RS_TOC) & RS_TOC == "TREATMENT FAILURE") | (!is.na(RS_TOC) & RS_TOC == "SLOW RESPONDER"), FALSE, NA),
    OUT_IC = ifelse(RS_TOC == "RESPONSIVE TO TREATMENT", TRUE, OUT_IC),
    OUT_IC = ifelse(is.na(RS_TOC), FALSE, OUT_IC),
    OUT_IC_FAIL = ifelse(OUT_IC == FALSE & !is.na(RS_TOC), TRUE, FALSE),
    OUT_IC_EXCLUDE = is.na(RS_TOC),
    OUT_IC_LTFU = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_IC_SR = ifelse(RS_TOC == "SLOW RESPONDER" & !is.na(RS_TOC), TRUE, FALSE) # slow responder at TOC
  )

OUT <- OUT %>% 
  mutate(
    OUT_IC_DEATH = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE
  )

OUT <- OUT %>% 
  mutate(
    OUT_DC_LTFU = (RS_TOC == "RESPONSIVE TO TREATMENT" & !is.na(RS_TOC))  & DSTERM == "LTFU",
    OUT_DC_DEATH = DSTERM == "Death"
  )

# 2 relapses in MDA, 2 relapses in 7.5, 4 relapses in 10 (difference from publication)
# of the 6 slow responders, 3 ended up requiring rescue treatment
OUT <- OUT %>% 
  mutate(
    OUT_DC_RELAPSE = ((RS_TOC == "RESPONSIVE TO TREATMENT" & !is.na(RS_TOC)) & !is.na(INTRT) | USUBJID %in% c("VBXLIU_GONDAR_1", "VBXLIU_GONDAR_17", "VBXLIU_GONDAR_30")),
    OUT_DC = (OUT_IC == TRUE | OUT_IC_SR == TRUE) & !OUT_DC_RELAPSE & !OUT_DC_DEATH & !OUT_DC_LTFU,
    OUT_IC_NA = FALSE,
    OUT_DC_NA = FALSE,
    OUT_NA = FALSE)

## OUTCOME NOTES

# NOT ENTIRELY CONSISTENT WITH PUBLICATION

# There are 9 LTFU recorded, 
## 6 in ther MDA arm (5 with initial treatment success, 1 with initial treatment failure, confirmed from MB)
## 3 in the SDA arm 7.5, all of whom have treatment failure, confirmed from MB

# There were 6 slow responders, 3 of which are treatment failures (confirmed with MB and PT medication), the other 3 are IC successes
# The OUT_NA = TRUE patients (x2) are major protocol violations

#OUT %>% count(across(starts_with("OUT"))) %>% View()

## LB DOMAIN

LB %>% count(LBTESTCD, VISIT, EPOCH, LBSTRF) 

# all blood tests unique at USUBJID for BASELINE level
LB %>% filter(EPOCH == "BASELINE") %>% count(USUBJID, LBTESTCD) %>% filter(n > 1)

names(LB)
LB_CLEAN <- LB %>% filter(EPOCH == "BASELINE") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = LBTESTCD,
    values_from = LBSTRESC,
    names_glue = "LB_BL_{LBTESTCD}"
  ) 

MP_CLEAN <- MP %>% filter(VISIT == "Day 0") %>% select(USUBJID, MPORRES) %>% rename(MP_BL_SPLEEN_LENGTH = MPORRES)
MB %>% names()

MB %>% filter(!is.na(VISIT) & VISIT == "Day 0") %>% count(USUBJID) %>% filter(n > 1)
MB %>% filter(!is.na(VISIT) & VISIT == "Day 0") %>% count(MBORRES)

MB_CLEAN <- MB %>% filter(VISIT == "Day 0" & !is.na(VISIT)) %>% 
  mutate(MBLOC = case_when(
    MBLOC == "BONE MARROW" ~ "BONE",
    MBLOC == "LYMPH NODE" ~ "LYMPH",
    .default = MBLOC
  )) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = MBLOC,
    values_from = MBORRES,
    names_glue = "MB_BL_LSHMANIA_{MBLOC}"
  )

VS %>% names()
VS %>% count(VSTESTCD, VSLOC, VISIT, EPOCH) 

VS_CLEAN <- VS %>% filter(
  EPOCH == "BASELINE") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSSTRESC,
    names_glue = "VS_BL_{VSTESTCD}"
  )

SA %>% names()
#SA %>% filter(SACAT == "MEDICAL HISTORY") %>% count(SATERM, SACONTRT, SASTDY, SAENDY, SADUR) %>% View()
# let's include all pre-specified terms in the medical history
# pretty much all the PMH have durations

SA %>% names()

SA_CLEAN1 <- SA %>% filter(SACAT == "MEDICAL HISTORY", !SATERM %in% c("ELEPHANTIASIS", "TINEA VERSICOLORIS")) %>% 
  mutate(SATERM = str_replace_all(SATERM, " ", "_")) %>%   
  mutate(SAOCCUR_BOOL = ifelse(SAOCCUR == "Y", TRUE, FALSE)) %>% 
  select(USUBJID, SATERM, SAOCCUR_BOOL) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR_BOOL,
    names_glue = "SA_{SATERM}"
  ) 

SA_CLEAN2 <- SA %>% filter(SACAT == "MEDICAL HISTORY", SATERM == "FEVER") %>% 
  rename(SA_HX_FEV_DUR = SADUR) %>% select(USUBJID, SA_HX_FEV_DUR)

VBXLIU <- DM_CLEAN    %>% 
  full_join(VS_CLEAN) %>% 
  full_join(MB_CLEAN) %>% 
  full_join(MP_CLEAN) %>% 
  full_join(SA_CLEAN1) %>% 
  full_join(SA_CLEAN2) %>% 
  full_join(LB_CLEAN) %>% 
  full_join(OUT %>% select(USUBJID, starts_with("OUT_")))

saveRDS(VBXLIU, "data/cleaned_rds/VBXLIU.rds")
VBXLIU %>% View()
OUT %>% names()
