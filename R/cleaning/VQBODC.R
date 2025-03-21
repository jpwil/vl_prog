##########
# VQBODC #
##########

# Wasunna M, Njenga S, Balasegaram M, Alexander N, Omollo R, Edwards T, Dorlo TP, Musa B, Ali MH, Elamin MY, Kirigi G, Juma R, Kip AE, Schoone GJ, Hailu A, Olobo J, Ellis S, Kimutai R, Wells S, Khalil EA, Strub Wourgaft N, Alves F, Musa A. 
# Efficacy and Safety of AmBisome in Combination with Sodium Stibogluconate or Miltefosine and Miltefosine Monotherapy for African Visceral Leishmaniasis: Phase II Randomized Trial. 
# PLoS Negl Trop Dis. 2016 Sep 14;10(9):e0004880. doi: 10.1371/journal.pntd.0004880. PMID: 27627654; PMCID: PMC5023160.

rm(list = ls())
library(tidyverse)
source("R/definitions.R")
load_domains("VQBODC")

# DM
DM %>% count()
DM %>% names()

DM %>% count(SITEID)
DM %>% count(SEX)
DM %>% count(AGE)
DM %>% count(DTHFL, DTHDTC) # 2 deaths identified as per paper
DM %>% mutate(DTHDY = dmy(DTHDTC) - dmy(RFSTDTC)) %>% count(DTHFL, DTHDY)

DM_CLEAN <- DM %>% 
  mutate(
    DM_DEATH = DTHFL,
    DM_DTHDY = dmy(DTHDTC) - dmy(RFSTDTC)
  ) %>% 
  select(
    USUBJID,
    RFSTDTC,
    DM_STE = SITEID,
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_ARM = ARMCD,
    DM_DEATH, DM_DTHDY
  ) 

## OUT

# DS
DS %>% names()
DS %>% count(VISIT, DSTERM)
DS %>% count(USUBJID, VISIT) %>% filter(n > 1)

DS_CLEAN <- DS %>% 
  mutate(VISIT_NEW = str_replace(VISIT, " ", "")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT_NEW,
    values_from = c(DSTERM, DSSTDY),
    names_glue = "DS_{VISIT_NEW}_{.value}",
    names_vary = "slowest"
  ) 
DS_CLEAN #%>% View()

# RS
RS %>% names()
RS %>% count(RSTESTCD, VISIT)
RS %>% count(USUBJID, VISIT) %>% filter(n > 1)

RS_CLEAN1 <- RS %>% 
  group_by(USUBJID, VISIT) %>% arrange(RSDY) %>% 
  mutate(N = row_number()) %>% 
  mutate(VISIT_NEW = if_else(is.na(VISIT), "NA", str_replace(VISIT, " ", ""))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT_NEW, N),
    values_from = c(RSORRES, RSDY),
    names_glue = "RS_{VISIT_NEW}_{N}_{.value}",
    names_vary = "slowest"
  )

RS_CLEAN1 #%>% View()

# PT
PT %>% names()
PT %>% count()
PT %>% count(VISIT, PTTRT) %>% print(n = Inf)

# IN - we have access to rescue treatment here 
IN %>% names()
IN %>% count(VISIT, INCAT, ININDC)
IN %>% count(VISIT, INCAT) %>% print(n = Inf)
IN_CLEAN1 <- IN %>% filter(INCAT %in% c("CONCOMITANT MEDICATION", "RESCUE MEDICATION"))
IN_CLEAN1 %>% count(INCAT, INTRT) %>% print(n = Inf)

IN_CLEAN1 <- IN %>% filter(INCAT %in% c("RESCUE MEDICATION"))
IN_CLEAN1 %>% count(USUBJID) %>% filter(n > 1)

IN_CLEAN1 %>% names()
IN_CLEAN1 %>% count(INTRT, INSTDY, INENDY) %>% print(n = 50)
IN_CLEAN2 <- IN_CLEAN1 %>% select(USUBJID, INTRT, INSTDY)

# MB
MB %>% names()
MB %>% count(MBTESTCD)
MB %>% filter(MBTESTCD == "LSHMANIA") %>% 
  count(VISIT, MBLOC)
MB %>% filter(is.na(VISIT)) %>% count(MBLOC, MBDY)
MB_CLEAN <- MB %>% 
  filter(MBTESTCD == "LSHMANIA") %>% 
  mutate(
    VISIT = case_when(
      is.na(MBLOC) ~ "SCREENING_NA",
      is.na(VISIT) & is.na(MBDY) ~ "FOLLOW_UP",
      .default = str_replace(VISIT, " ", "")
    )
  )

MB_CLEAN %>% count(VISIT)
MB_CLEAN %>% count(USUBJID, VISIT, MBORRES)
MB_CLEAN %>% count(USUBJID, VISIT) %>% filter(n > 1)
MB_CLEAN %>% filter(USUBJID == "VQBODC_DOOKA_537") #%>% View()

# VQBODC_DOOKA_537 has two identical MB entries 2+ lymph aspirate during follow-up (MBSEQ = 4 and 5)
MB_CLEAN <- MB_CLEAN %>% filter(!(USUBJID == "VQBODC_DOOKA_537" & MBSEQ == 5)) # now VISIT is unique

MB_CLEAN2 <- MB_CLEAN %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}"
  )

#MB_CLEAN2 %>% View()
DM_CLEAN %>% names()
OUT <- DM_CLEAN %>% select(USUBJID, DM_DEATH, DM_DTHDY) %>% 
  full_join(RS_CLEAN1) %>% 
  full_join(DS_CLEAN) %>% 
  full_join(MB_CLEAN2) %>% 
  full_join(IN_CLEAN2) 

OUT_CLEAN <- OUT %>% 
  mutate(
    OUT_IC_DEATH = USUBJID == "VQBODC_DOOKA_505",
    OUT_DC_DEATH = USUBJID == "VQBODC_DOOKA_556",
    OUT_IC_SR = USUBJID %in% c("VQBODC_DOOKA_553", "VQBODC_DOOKA_517", "VQBODC_DOOKA_523", "VQBODC_DOOKA_527", "VQBODC_DOOKA_530", "VQBODC_DOOKA_541"),
    OUT_IC = if_else(USUBJID %in% c("VQBODC_KIMALEL_321", "VQBODC_KASSAB_710", "VQBODC_KIMALEL_347", "VQBODC_DOOKA_502", "VQBODC_DOOKA_526", "VQBODC_DOOKA_529", "VQBODC_DOOKA_505"), FALSE, TRUE),
    OUT_IC = if_else(OUT_IC_SR, FALSE, OUT_IC), 
    OUT_DC_RELAPSE = USUBJID %in% c("VQBODC_DOOKA_556", "VQBODC_KASSAB_705", "VQBODC_DOOKA_549", "VQBODC_DOOKA_545", "VQBODC_DOOKA_539", "VQBODC_DOOKA_537", "VQBODC_DOOKA_520", "VQBODC_DOOKA_513", "VQBODC_DOOKA_506", "VQBODC_DOOKA_503", "VQBODC_DOOKA_501", "VQBODC_KIMALEL_369", "VQBODC_KIMALEL_358", "VQBODC_KIMALEL_323", "VQBODC_KIMALEL_318", "VQBODC_KIMALEL_303"),
    OUT_DC = if_else(RS_DAY210_1_RSORRES == "Treatment Success" & !is.na(RS_DAY210_1_RSORRES), TRUE, FALSE),
    OUT_DC_NA = FALSE,
    OUT_IC_NA = FALSE,
    OUT_NA = FALSE
  )

OUT_CLEAN %>% select(starts_with("OUT_")) %>% count(pick(everything())) #%>% View()
OUT_CLEAN %>% filter(if_all(starts_with("OUT_"), ~ .x==FALSE)) #%>% View()

# IC FAIL CAN BE DEDUCED

# MB for mergings
MB_CLEAN <- MB %>% filter(VISIT %in% c("DAY 28", "SCREENING"), MBTESTCD == "LSHMANIA") %>% 
  mutate(
    VISIT = case_when(
      VISIT == "SCREENING" ~ "BL",
      VISIT == "DAY 28" ~ "IC",
      .default = NA
    ),
    MBLOC = case_when(
      MBLOC == "BONE MARROW" ~ "BONE",
      MBLOC == "LYMPH NODE" ~ "LYMPH", 
      .default = MBLOC
    )) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}_LSHMANIA_{MBLOC}"
  )
MB_CLEAN #%>% View()

# SEROLOGY
MB %>% count(VISIT, MBMETHOD, MBLOC)
MB %>% filter(MBMETHOD == "RAPID IMMUNOASSAY") %>% count(USUBJID) %>% filter(n > 1) # no duplicates
MB_SER <- MB %>% filter(MBMETHOD == "RAPID IMMUNOASSAY")  %>% select(USUBJID, MB_BL_SER = MBORRES)

# HIV
MB %>% count(MBTESTCD)
MB %>% filter(MBTESTCD == "HIV12AB") %>% count(USUBJID) %>% filter(n > 1) 
MB %>% filter(MBTESTCD == "HIV12AB") %>% count(MBORRES)
MB_HIV <- MB %>% filter(MBTESTCD == "HIV12AB") %>% mutate(MB_BL_HIV12AB = FALSE) %>% select(USUBJID, MB_BL_HIV12AB)

# SA
SA #%>% View()
SA %>% names()
SA %>% count(EPOCH)

# 
SA_VEC <- SA %>% filter(EPOCH == "BASELINE" & is.na(SASTAT)) %>% count(SATERM, SAPRESP, SASTAT, SAREASND) %>% filter(n > 1) %>% pull(SATERM)
SA_CLEAN <- SA %>% filter(EPOCH == "BASELINE" & is.na(SASTAT) & SATERM %in% SA_VEC) %>% 
  mutate(SAOCCUR_NEW = if_else(SAOCCUR == "Y", TRUE, FALSE)) %>% 
  mutate(SATERM = toupper(str_replace(SATERM, " ", "_")))
SA_CLEAN1 <- SA_CLEAN %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR_NEW,
    names_glue = "SA_BL_{SATERM}"
  )

SA_CLEAN1 #%>% View()

# MP
MP %>% names()
MP %>% count(VISIT, MPTEST)
MP %>% filter(VISIT == "SCREENING") %>% count(USUBJID, MPLOC) %>% filter(n > 1)

MP_CLEAN <- MP %>% filter(VISIT == "SCREENING") %>% pivot_wider(
  id_cols = USUBJID,
  names_from = MPLOC,
  values_from = MPORRES,
  names_glue = "MP_BL_{MPLOC}_LENGTH"
) 

# LB
LB %>% filter(VISIT == "SCREENING") %>% count(USUBJID, LBTESTCD) %>% filter(n > 1)
LB_CLEAN <- LB %>% 
  filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = LBTESTCD,
    values_from = LBSTRESN,
    names_glue = "LB_BL_{LBTESTCD}"
  )
LB_CLEAN #%>% View()

# VS
VS %>% names()
VS %>% count(VISIT)

# each patient has ~ 5 repeated screening HR / DIABP / SYSBP / TEMP....
# choose the 
VS %>% filter(VISIT == "SCREENING") %>% group_by(USUBJID, VSTESTCD) %>% arrange(VSDTC) %>% mutate(N = row_number()) %>% 
  filter(N == 1) %>% ungroup() %>% count(USUBJID, VSTESTCD) %>% filter(n > 1)

VS_CLEAN <- VS %>% filter(VISIT == "SCREENING") %>% group_by(USUBJID, VSTESTCD) %>% arrange(VSDTC) %>% mutate(N = row_number()) %>% 
  filter(N == 1) %>% ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSORRES,
    names_glue = "VS_BL_{VSTESTCD}"
  )

#  
DM_CLEAN %>% names()
VQBODC <- DM_CLEAN %>% select(-c(DM_DEATH, DM_DTHDY)) %>% 
  full_join(OUT_CLEAN %>% select(USUBJID, starts_with("OUT_"))) %>% 
  full_join(MB_CLEAN) %>% 
  full_join(MB_SER) %>% 
  full_join(MB_HIV) %>% 
  full_join(SA_CLEAN1) %>% 
  full_join(VS_CLEAN) %>% 
  full_join(LB_CLEAN) %>% 
  full_join(MP_CLEAN) 

VQBODC #%>% View()
