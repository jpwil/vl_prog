############
## VOAYGC ##
############

# we do not have 6 month outcomes

rm(list = ls())
library(tidyverse)
source("R/definitions.R")
load_domains("VOAYGC")

DM %>% names()
DM %>% count()
DM %>% count(SEX)
DM %>% count(ARMCD)
DM %>% count(SITEID)
DM %>% summary(AGE)

# 22 deaths recorded in DM
DM %>% count(DTHFL, DTHDTC)

DM_CLEAN <- DM %>% 
  mutate(
    DM_SEX = SEX,
    DM_SITE = SITEID,
    DM_ARM = ARMCD,
    DM_AGE = AGE
  ) %>% 
  select(USUBJID, RFSTDTC, starts_with("DM_"))

DM_CLEAN %>% names()

# OUTCOMES
RS %>% names()
RS %>% count(USUBJID, VISIT, RSSCAT, RSTESTCD) %>% filter(n > 1)

RS_CLEAN <- RS %>% 
  mutate(
    RSSCAT = ifelse(is.na(RSSCAT), "N", "AOP"),
    VISIT = str_replace(VISIT, " ", "")
  ) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, RSTESTCD, RSSCAT),
    values_from = RSORRES,
    names_glue = "RS_{VISIT}_{RSTESTCD}_{RSSCAT}",
    names_vary = "slowest"
  )

# only has IC outcomes
DS %>% names()
DS %>% count(EPOCH, DSTERM)
DS %>% count(USUBJID, DSTERM) %>% filter(n > 1)

DS_CLEAN <- DS %>% select(USUBJID, DSTERM)

# MB
MB %>% count()
MB %>% names()
MB %>% count(MBLOC)

MB %>% count(VISIT, EPOCH, MBLOC, MBSPEC)
MB %>% filter(MBSPEC == "TISSUE") %>% count(VISIT, MBLOC)
MB %>% filter(MBSPEC == "TISSUE") %>% count(USUBJID, VISIT, MBLOC) %>% filter(n > 1) # a number of patients have > 1 screening aspirates
MB %>% filter(MBSPEC == "TISSUE") %>% count(USUBJID, VISIT, MBLOC, MBORRES) %>% count(n)

MB %>% filter(MBSPEC == "TISSUE") %>% count(MBORRES)

MB_CLEAN <- MB %>% filter(MBSPEC == "TISSUE") %>% group_by(USUBJID, VISIT, MBLOC) %>% arrange(MBDY) %>% mutate(N = row_number()) %>% ungroup() %>% 
  mutate(
    VISIT = str_replace(VISIT, " ", ""),
    MBORRES_CL = case_match(MBORRES,
      "0" ~ 0,
      "1+" ~ 1,
      "2+" ~ 2,
      "3+" ~ 3,
      "4+" ~ 4,
      "5+" ~ 5,
      "Poor material" ~ -99,
      NA ~ -99,
      .default = -100),
    MBLOC = case_when(
      MBLOC == "BONE MARROW" ~ "BONE",
      MBLOC == "LYMPH NODE" ~ "LYMPH",
      .default = MBLOC)) %>% 
  pivot_wider(
    id_cols = USUBJID, 
    names_from = c(VISIT, MBLOC, N),
    values_from = MBORRES_CL,
    names_glue = "DS_{VISIT}_{MBLOC}_{N}",
    values_fill = -99
  )

MB_CLEAN <- MB_CLEAN %>%
  mutate(
    DS_SCREENING = pmax(DS_SCREENING_LYMPH_1, DS_SCREENING_LYMPH_2, DS_SCREENING_SPLEEN_1),
    DS_DAY30 = pmax(DS_DAY30_LYMPH_1, DS_DAY30_BONE_1, DS_DAY30_SPLEEN_1, DS_DAY30_NA_1),
    DS_DAY37 = DS_DAY37_LYMPH_1,
    DS_DAY44 = pmax(DS_DAY44_LYMPH_1, DS_DAY44_SPLEEN_1),
    DS_DAY51 = DS_DAY51_LYMPH_1
  ) %>% select(USUBJID, DS_SCREENING, DS_DAY30, DS_DAY37, DS_DAY44, DS_DAY51)#%>% View()

MB_CLEAN <- MB_CLEAN %>% 
  mutate(across(starts_with("DS_"), ~ifelse(.x == -99, NA, .x)))

# DM
DM_OUT <- DM %>% 
  mutate(DTHDY = dmy(DTHDTC) - dmy(RFSTDTC)) %>% 
  select(USUBJID, DTHFL, DTHDY)

# IN and PT - no relapse information here (no rescue treatment, no history of relapse)
PT %>% names()
PT %>% count(VISIT) %>% print(n = Inf)

IN %>% names()
IN %>% count(VISIT)
IN %>% count(INTRT, INPRESP)

# DD
DD %>% names()
DD %>% count(USUBJID, DDTEST, DDORRES) %>% filter(n > 1)
DD_CLEAN <- DD %>% select(USUBJID, DDTEST, DDORRES)

OUTCOME <- DD_CLEAN  %>% 
  full_join(DM_OUT) %>% 
  full_join(RS_CLEAN) %>% 
  full_join(DS_CLEAN) %>% 
  full_join(MB_CLEAN) 

OUTCOME %>% count(DTHFL, RS_DAY30_TOC_N, RS_DAY30_OVRLRESP_N, DS_DAY30)

OUTCOME_CLEAN <- OUTCOME %>% 
  mutate(
    OUT_IC_DEATH = DSTERM == "death" & !is.na(DSTERM),
    OUT_IC = ifelse(is.na(DTHFL) & !is.na(DS_DAY30) & DS_DAY30 == 0, TRUE, FALSE),
    OUT_IC_OTHER = ifelse(USUBJID == "VOAYGC_UM-EL-KHER_8621", TRUE, FALSE), # 1 x defaulter
    OUT_IC_NA = ifelse(is.na(DTHFL) & is.na(DS_DAY30) & RS_DAY30_OVRLRESP_N == "Discharged 30d Neg TOC" & !is.na(RS_DAY30_OVRLRESP_N), TRUE, FALSE),
    OUT_DC = FALSE,
    OUT_IC_FAIL = (DTHFL == "Y" & !is.na(DTHFL)) | (DS_DAY30 > 0 & !is.na(DS_DAY30))
  )

OUTCOME_CLEAN %>% count(pick(starts_with("OUT_")))

OUTCOME %>% filter(is.na(DTHFL) & is.na(RS_DAY30_TOC_N) & is.na(RS_DAY30_OVRLRESP_N) & is.na(DS_DAY30)) #%>% View()

# SA
SA %>% count()
SA %>% names()  
SA %>% count(SATERM)
SA %>% count(VISIT, EPOCH, SATERM, SAOCCUR) %>% print(n = Inf)
SA %>% filter(SATERM == "PKDL") %>% count(SAPRESP, SAOCCUR)

SA %>% count(SAOCCUR)
SA_CLEAN <- SA %>% filter(SATERM %in% c("PNEUMONIA", "VOMITTING", "TB", "NEUROLOGICAL COMPLICATION", "MALARIA", "JAUNDICE", "EDEMA", "DIARRHEA", "BLEEDING", "ASCITES")) %>% 
  mutate(SATERM = str_replace(SATERM, " ", "_")) %>% 
  mutate(SAOCCUR_CL = ifelse(SAOCCUR == "N", FALSE, TRUE)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR_CL,
    names_glue = "SA_BL_{SATERM}"
  )

# LB
LB %>% count()
LB %>% names()
LB %>% count(VISIT, LBTESTCD)
LB_CLEAN <- LB %>% filter(VISIT == "SCREENING") %>% select(USUBJID, LBSTRESN) %>% rename(LB_BL_HGB = LBSTRESN)
LB_CLEAN %>% count(USUBJID) %>% filter(n > 1)
LB_CLEAN #%>% View()

# VS
VS %>% count()
VS %>% names()
VS %>% count(VISIT, VSTESTCD)

VS %>% filter(VISIT == "SCREENING", VSTESTCD != "BMI") %>% count(USUBJID, VSTESTCD) %>% filter(n > 1)
VS_CLEAN <- VS %>% filter(VISIT == "SCREENING", VSTESTCD != "BMI") %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSSTRESN,
    names_glue = "VS_BL_{VSTESTCD}"
  )

# MP
MP %>% count()
MP %>% names()
MP %>% count(VISIT, MPLOC)
MP %>% filter(VISIT == "SCREENING") %>% count(USUBJID, MPLOC) %>% filter(n > 1)

MP_CLEAN <- MP %>% filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = MPLOC,
    values_from = MPORRES,
    names_glue = "MP_BL_{MPLOC}_LENGTH"
  )

# MB
MB_CLEAN %>% names()
MB_CLEAN <- MB %>% filter(MBSPEC == "TISSUE") %>% group_by(USUBJID, VISIT, MBLOC) %>% arrange(MBDY) %>% mutate(N = row_number()) %>% ungroup() %>% 
  mutate(
    VISIT = str_replace(VISIT, " ", ""),
    MBORRES_CL = case_match(MBORRES,
      "0" ~ 0,
      "1+" ~ 1,
      "2+" ~ 2,
      "3+" ~ 3,
      "4+" ~ 4,
      "5+" ~ 5,
      "Poor material" ~ -99,
      NA ~ -99,
      .default = -100),
    MBLOC = case_when(
      MBLOC == "BONE MARROW" ~ "BONE",
      MBLOC == "LYMPH NODE" ~ "LYMPH",
      .default = MBLOC)) %>% 
  pivot_wider(
    id_cols = USUBJID, 
    names_from = c(VISIT, MBLOC, N),
    values_from = MBORRES_CL,
    names_glue = "MB_{VISIT}_{MBLOC}_{N}",
    values_fill = -99
  )

MB_CLEAN %>% View()
MB_CLEAN <- MB_CLEAN %>% 
  mutate(MB_SCREENING_LYMPH = pmax(MB_SCREENING_LYMPH_1, MB_SCREENING_LYMPH_2))
MB_CLEAN %>% names()

MB_CLEAN2 <- MB_CLEAN %>% select(
  USUBJID,
  MB_BL_LSHMANIA_LYMPH = MB_SCREENING_LYMPH,
  MB_BL_LSHMANIA_SPLEEN = MB_SCREENING_SPLEEN_1,
  MB_IC_LSHMANIA_LYMPH = MB_DAY30_LYMPH_1,
  MB_IC_LSHMANIA_SPLEEN = MB_DAY30_SPLEEN_1,
  MB_IC_LSHMANIA_BONE = MB_DAY30_BONE_1,
  MB_IC_LSHMANIA_NA = MB_DAY30_NA_1
)

MB_CLEAN2 %>% View()
MB_CLEAN2 <- MB_CLEAN2 %>% 
  mutate(across(starts_with("MB_"), ~ ifelse(.x == -99, NA, .x)))

MB %>% names()
MB %>% count(MBSPEC)

MB %>% filter(MBSPEC == "SERUM") %>% count(MBORRES) 
MB_SER1 <- MB %>% filter(MBSPEC == "SERUM") %>% group_by(USUBJID) %>% arrange(MBDY) %>% 
  mutate(N = row_number()) %>% ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = N,
    values_from = MBORRES,
    names_glue = "MB_SEROLOGY_{N}"
  )

MB_SER1 %>% count(MB_SEROLOGY_1, MB_SEROLOGY_2)
MB_SER2 <- MB_SER1 %>%  # pick the most 'positive' test result
  mutate(
    MB_BL_SER = case_when(
      is.na(MB_SEROLOGY_2) ~ MB_SEROLOGY_1,
      MB_SEROLOGY_2 == "POSITIVE" & !is.na(MB_SEROLOGY_2) ~ "POSITIVE",
      MB_SEROLOGY_2 == "NEGATIVE" & !is.na(MB_SEROLOGY_2) ~ MB_SEROLOGY_1,
      MB_SEROLOGY_1 == "BORDERLINE" & MB_SEROLOGY_2 == "BORDERLINE" ~ "BORDERLINE",
      MB_SEROLOGY_1 == "NEGATIVE" & MB_SEROLOGY_2 == "BORDERLINE" ~ "BORDERLINE",
      MB_SEROLOGY_1 == "POSITIVE" & MB_SEROLOGY_2 == "BORDERLINE" ~ "POSITIVE"
    )
  ) %>% 
  select(USUBJID, MB_BL_SER)

# RP

RP %>% names()
RP %>% count(RPTESTCD)
RP_CLEAN <- RP %>% filter(RPTESTCD == "PREGIND") %>% 
  mutate(RP_PREG = case_when(
    RPORRES == "Y" ~ TRUE,
    RPORRES == "N" ~ FALSE,
    .default = NA
  )) %>% 
  select(USUBJID, RP_PREG)

VOAYGC <- DM_CLEAN %>% 
  full_join(OUTCOME_CLEAN %>% select(USUBJID, starts_with("OUT_"))) %>% 
  full_join(SA_CLEAN) %>% 
  full_join(LB_CLEAN) %>% 
  full_join(VS_CLEAN) %>% 
  full_join(MP_CLEAN) %>% 
  full_join(MB_SER2) %>% 
  full_join(MB_CLEAN2) %>% 
  full_join(RP_CLEAN)

saveRDS(VOAYGC, "data/cleaned_rds/VOAYGC.rds")