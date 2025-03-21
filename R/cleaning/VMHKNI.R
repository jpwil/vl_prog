############
## VMHKNI ##
############

rm(list = ls())
library(tidyverse)
source("R/definitions.R")
load_domains("VMHKNI")

DM %>% count()
DM %>% names()

DM %>% count(SITEID)
DM %>% count(SEX)
DM %>% count(AGE)
DM %>% count(COUNTRY)
DM %>% count(RFSTDTC)
DM %>% count(ARMCD)

DM_CLEAN <- DM %>% 
  rename(
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  mutate(
    RFSTDTC = dmy(RFSTDTC)
  )

# OUTCOMES

PT %>% names()
PT %>% count(VISIT, PTTRT) %>% print(n = Inf)

IN %>% names()
IN %>% count(EPOCH, INCAT, INTRT, INPRESP) %>% print(n = Inf)
IN %>% filter(INCAT == "RESCUE MEDICATION") %>% select(USUBJID, INSTDY, INENDY)

# RS
RS %>% names()
RS %>% count(VISIT, RSCAT, RSSCAT, RSTESTCD, RSORRES)

# 
DS #%>% View()

# VMHKNI_AMUDAT_605: did not achieve initial cure
# VMHKNI_KACHELIBA_915, VMHKNI_AMUDAT_604: relapsed (corresponds with PT data)

# RS: all initial cure 
OUT_CLEAN <- DM %>% select(USUBJID) %>% 
  mutate(
    OUT_IC = !(USUBJID == "VMHKNI_AMUDAT_605"),
    OUT_IC_FAIL = USUBJID == "VMHKNI_AMUDAT_605",
    OUT_IC_NA = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_NA = FALSE,
    OUT_DC = !(USUBJID %in% c("VMHKNI_KACHELIBA_915", "VMHKNI_AMUDAT_605", "VMHKNI_AMUDAT_604")),
    OUT_DC_RELAPSE = USUBJID %in% c("VMHKNI_KACHELIBA_915", "VMHKNI_AMUDAT_604"),
    OUT_DC_DEATH = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_DC_NA = FALSE
  )

OUT_CLEAN %>% count(pick(-USUBJID)) %>% print(n = Inf) #%>% View()

# LB
LB %>% names()
LB %>% count(VISIT, LBTESTCD)
LB_CLEAN <- LB %>% filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = LBTESTCD,
    values_from = LBSTRESN,
    names_glue = "LB_BL_{LBTESTCD}"
  )

# SA - no symptom duration information here
SA %>% names()
SA %>% count(SAOCCUR)
SA %>% count(EPOCH, SATERM, SAPRESP, SASTAT, SAACN) %>% arrange(desc(n)) %>% print(n = Inf)
SA %>% count(EPOCH, SASTRTPT, SASTTPT)

SA_CLEAN <- SA %>% filter(SASTTPT == "SCREENING") %>% 
  mutate(SATERM = toupper(str_replace(SATERM, " ", "_"))) %>% 
  mutate(SAO = case_when(
    SAOCCUR == "N" ~ FALSE,
    SAOCCUR == "Y" ~ TRUE,
    is.na(SAOCCUR) ~ NA,
    .default = NA
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAO,
    names_glue = "SA_BL_{SATERM}"
  ) 

# MP
MP %>% names()
MP %>% count(VISIT, MPLOC, MPTESTCD)

MP_CLEAN <- MP %>% filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = MPLOC,
    values_from = MPORRES,
    names_glue = "MP_BL_{MPLOC}_LENGTH"
  )

# VS
VS %>% names()
VS %>% count(VSLOC)
VS %>% count(VISIT)
VS %>% count(VISIT, VSTESTCD, VSLOC)

VS_CLEAN <- VS %>% filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSSTRESN,
    names_glue = "VS_BL_{VSTESTCD}"
  )

# MB
# All HIV tests are negative
MB %>% count()
MB %>% names()
MB %>% count(MBTESTCD, MBORRES)
MB %>% filter(MBTESTCD == "HIV12AB") %>% count(VISIT, MBTESTCD, MBLOC)

MB_CLEAN <- MB %>% filter(
    VISIT == "SCREENING", 
    MBTESTCD == "LSHMANIA", 
    MBTSTDTL == "QUANTIFICATION"
  ) %>% 
  mutate(MBLOC = ifelse(MBLOC == "BONE MARROW", "BONE", MBLOC)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = MBLOC,
    values_from = MBORRES,
    names_glue = "MB_BL_LSHMANIA_{MBLOC}"
  ) 

VMHKNI <- DM_CLEAN %>% 
  full_join(MB_CLEAN) %>% 
  full_join(SA_CLEAN) %>% 
  full_join(MP_CLEAN) %>% 
  full_join(VS_CLEAN) %>% 
  full_join(OUT_CLEAN) %>% 
  full_join(LB_CLEAN)

VMHKNI %>% View()
