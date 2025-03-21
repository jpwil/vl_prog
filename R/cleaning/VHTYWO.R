############
## VHTYWO ##
############

# LTFU patients NOT identified
# Slow responders NOT identified
# NO DS DOMAIN
# OUTCOMES NOT ALWAYS CONSISTENT WITH MB AND IN DOMAIN INFORMATION
# NO FEVER / ILLNESS DURATION MARKERS

rm(list = ls())
library(dplyr)
library(lubridate)

source("R/definitions.R")
load_domains("VHTYWO")

# 405 IPD
DM %>% count()
DM %>% count(DTHDTC) # same as DD

# all from November 2004 to October 2007 (approximately)
DM_CLEAN <- DM %>% mutate(DM_RFSTDTC = dmy(RFSTDTC)) %>% relocate(DM_RFSTDTC) 
DM_CLEAN %>% group_by(ARMCD) %>% 
  summarise(
    earliest = min(DM_RFSTDTC), 
    latest = max(DM_RFSTDTC)
  ) #%>% View()

DM_CLEAN <- DM %>% 
  mutate(ARMCD = ifelse(ARMCD == "PM", "PM15", ARMCD)) %>% 
  rename(
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID,
    DM_ARM = ARMCD
    ) %>% 
  select(USUBJID, starts_with("DM_"), RFSTDTC)

## OUTCOMES

# There is no DS domain

# RS
names(RS)
RS %>% count(USUBJID, RSTESTCD, VISIT) %>% filter(n > 1)
RS %>% names()

RS %>% count(VISIT, RSORRES) %>% print(n = Inf)
RS %>% count(USUBJID, VISIT) %>% filter(n > 1)

RS_CLEAN <- RS %>% 
  mutate(VISIT = toupper(str_replace_all(VISIT, " ", ""))) %>% 
  pivot_wider(
  id_cols = USUBJID,
  names_from = VISIT,
  values_from = c(RSORRES, RSDY),
  names_vary = "slowest",
  names_glue = "{VISIT}_{.value}"
  ) %>% 
  mutate(
    RS_TOC_RSDY = case_when(
      !is.na(DAY18_RSDY) ~ DAY18_RSDY,
      !is.na(DAY22_RSDY) ~ DAY22_RSDY,
      !is.na(DAY31_RSDY) ~ DAY31_RSDY,
      .default = NA
    ),
    RS_TOC_RSORRES = case_when(
    !is.na(DAY18_RSORRES) ~ DAY18_RSORRES,
    !is.na(DAY22_RSORRES) ~ DAY22_RSORRES,
    !is.na(DAY31_RSORRES) ~ DAY31_RSORRES,
    .default = "all missing"
    )
  ) %>% 
  select(-starts_with("DAY18_"), -starts_with("DAY22"), -starts_with("DAY31")) %>% 
  full_join(DM_CLEAN %>% select(USUBJID, DM_ARM, DM_SITE)) %>% 
  relocate(USUBJID, DM_ARM, RS_TOC_RSORRES, RS_TOC_RSDY, DAY90_RSORRES, DAY90_RSDY, DAY180_RSORRES, DAY180_RSDY) 

# MB
MB %>% names()
MB %>% filter(MBTESTCD != "HIV") %>% count(USUBJID, MBTESTCD, VISIT, MBLOC, MBORRES) %>% filter(n > 1)
MB_CLEAN <- MB %>% filter(MBTESTCD != "HIV") %>% 
  mutate(VISIT = toupper(str_replace_all(VISIT, " ", ""))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(MBORRES, MBDY),
    names_vary = "slowest",
    names_glue = "{VISIT}_{.value}"
  ) %>%
  mutate(
    MB_TOC_MBDY = case_when(
      !is.na(DAY18_MBDY) ~ DAY18_MBDY,
      !is.na(DAY22_MBDY) ~ DAY22_MBDY,
      !is.na(DAY31_MBDY) ~ DAY31_MBDY,
      .default = NA
    ),
    MB_TOC_MBORRES = case_when(
    !is.na(DAY18_MBORRES) ~ DAY18_MBORRES,
    !is.na(DAY22_MBORRES) ~ DAY22_MBORRES,
    !is.na(DAY31_MBORRES) ~ DAY31_MBORRES,
    .default = "all missing"
    )
  ) %>% 
  select(-c(DAY18_MBORRES, DAY22_MBORRES, DAY31_MBORRES, DAY18_MBDY, DAY22_MBDY, DAY31_MBDY)) %>% 
  relocate(USUBJID, DAY0_MBORRES, DAY0_MBDY, MB_TOC_MBORRES, MB_TOC_MBDY, DAY90_MBORRES, DAY90_MBDY, DAY180_MBORRES, DAY180_MBDY)

# IN
IN %>% names()
IN_CLEAN <- IN %>% 
  filter(INCAT == "RESCUE MEDICATION" & !is.na(INCAT)) %>% 
  group_by(USUBJID, INTRT) %>% 
  summarise(INSTDY = min(INSTDY, na.rm = TRUE)) %>% 
  select(USUBJID, INSTDY)

OUT <- IN_CLEAN %>% full_join(RS_CLEAN) %>% full_join(MB_CLEAN)


OUT %>% filter(!(RS_TOC_RSORRES == "Initial Cure")) %>% arrange(!is.na(INSTDY), RS_TOC_RSORRES) #%>%  View()

OUT %>% names()
OUT %>% select(RS_TOC_RSORRES, INSTDY, MB_TOC_MBORRES, MB_TOC_MBDY, DAY90_MBDY, DAY180_MBDY) %>% 
  mutate(DIFF = INSTDY - MB_TOC_MBDY) %>% arrange(MB_TOC_MBORRES, DIFF) %>% filter(RS_TOC_RSORRES != "Iniitial Cure") #%>% View()

OUT_CLEAN <- OUT %>% 
  mutate(
    OUT_IC          = ifelse(RS_TOC_RSORRES == "Initial Cure", TRUE, FALSE),
    OUT_IC          = ifelse(is.na(OUT_IC), FALSE, OUT_IC),
    OUT_NA          = ifelse(USUBJID %in% c("VHTYWO_KEMRI_392") | is.na(RS_TOC_RSORRES), TRUE, FALSE),
    OUT_IC_SR       = ifelse(is.na(INSTDY) & RS_TOC_RSORRES == "Initial Failure", TRUE, FALSE),
    OUT_IC_SR       = ifelse(!is.na((INSTDY - MB_TOC_MBDY)) & (INSTDY - MB_TOC_MBDY) >= 20 & RS_TOC_RSORRES != "Initial Cure", TRUE, OUT_IC_SR),
    OUT_IC_SR       = ifelse(is.na(OUT_IC_SR), FALSE, OUT_IC_SR),
    OUT_DC          = ifelse(DAY180_RSORRES == "Cured" & !is.na(DAY180_RSORRES), TRUE, FALSE),
    OUT_DC          = ifelse(OUT_NA, FALSE, OUT_DC),
    OUT_DC_RELAPSE  = ifelse(!is.na(INSTDY) & RS_TOC_RSORRES == "Initial Cure", TRUE, FALSE),                                                        ## initial success who relapse
    OUT_DC_RELAPSE  = ifelse(!is.na((INSTDY - MB_TOC_MBDY)) & (INSTDY - MB_TOC_MBDY) >= 20 & RS_TOC_RSORRES != "Initial Cure", TRUE, OUT_DC_RELAPSE), ## slow responders who relapse
    OUT_DC_RELAPSE  = ifelse(USUBJID %in% c("VHTYWO_UM EL KHER_526", "VHTYWO_GONDAR_128"), TRUE, OUT_DC_RELAPSE),
    OUT_IC_FAIL     = str_detect(RS_TOC_RSORRES, "Failure"),
    OUT_IC_SR       = ifelse(!OUT_IC & !OUT_NA & !OUT_IC_SR & OUT_DC & OUT_IC_FAIL & !is.na(MB_TOC_MBORRES), TRUE, OUT_IC_SR),
    OUT_IC_DEATH    = USUBJID %in% c("VHTYWO_KASSAB_677"),
    OUT_DC_DEATH    = USUBJID %in% c("VHTYWO_GONDAR_27", "VHTYWO_GONDAR_47", "VHTYWO_ARBA MINCH_246", "VHTYWO_ARBA MINCH_265")
    )

OUT_CLEAN <- OUT_CLEAN %>% ungroup()
#OUT_CLEAN %>% View()
OUT_CLEAN %>% ungroup() %>% count(DAY180_RSORRES)

OUT_CLEAN %>% count(across(starts_with("OUT_")))  
#OUT_CLEAN %>% count(RS_TOC_RSORRES, across(starts_with("OUT_"))) %>% View()

OUT_CLEAN %>% filter(OUT_IC & !OUT_DC_RELAPSE & !OUT_DC) # some of these may be LTFU patients - leave for now
#OUT_CLEAN %>% filter(!OUT_IC & !OUT_NA & OUT_IC_SR & !OUT_DC & !OUT_DC_RELAPSE &OUT_IC_FAIL) %>% View()


OUT_CLEAN <- OUT_CLEAN %>% select(USUBJID, starts_with("OUT_"))

# DD
DD %>% select(USUBJID, EPOCH, DDDY) %>% full_join(OUT_CLEAN) #%>% View()

# LB

LB %>% names()
LB %>% count(VISIT)
LB %>% count(LBSPEC)
LB %>% filter(LBSPEC != "URINE") %>% count(VISIT, LBTESTCD, LBSPEC)
LB %>% filter(LBSPEC != "URINE") %>% count(USUBJID, VISIT, LBTESTCD) %>% filter(n > 1)

LB_CLEAN <- LB %>% filter(LBSPEC != "URINE" | is.na(LBSPEC), VISIT == "Day 0") %>% 
  pivot_wider(
    id_cols = "USUBJID",
    names_from = LBTESTCD,
    values_from = LBSTRESN,
    names_glue = "LB_BL_{LBTESTCD}"
  ) #%>% View()

MB %>% count(MBLOC)
# MB
MB %>% filter(VISIT == "Day 0" | is.na(VISIT))  %>% count(USUBJID, MBTESTCD) %>% filter(n > 1)
MB_CLEAN <- MB %>% filter(VISIT == "Day 0" | is.na(VISIT)) %>% 
  mutate(
    MBLOC = case_when(
      MBLOC == "BONE MARROW" ~ "BONE",
      MBLOC == "LYMPH NODE" ~ "LYMPH",
      is.na(MBLOC) ~ "NA",
      MBLOC == "SPLEEN" ~ "SPLEEN",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MBTESTCD, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_BL_{MBTESTCD}_{MBLOC}"
  ) %>% rename(MB_BL_HIV = MB_BL_HIV_NA)

# VS

VS %>% filter(VISIT == "Day 0", VSTESTCD == "TEMP") %>% pull(VSSTRESN) %>% hist()
VS %>% names()

VS_CLEAN <- VS %>% filter(VISIT == "Day 0") %>% pivot_wider(
  id_cols = USUBJID,
  names_from = VSTESTCD,
  values_from = VSSTRESN,
  names_glue = "VS_BL_{VSTESTCD}"
) 

# MP
MP %>% names()
MP_CLEAN <- MP %>% filter(VISIT == "Day 0") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MPTESTCD, MPLOC),
    values_from = MPSTRESN,
    names_glue = "MP_BL_{MPLOC}_{MPTESTCD}"
  ) 

# SA

## NO FEVER DURATION!
SA %>% names()
SA %>% count(VISIT, EPOCH)
SA_CLEAN <- SA %>% filter(VISIT == "Day 0", SATERM != "Pneumonia") %>% 
  mutate(SATERM = toupper(str_replace_all(SATERM, " ", "_"))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR,
    names_glue = "SA_BL_{SATERM}"
  ) 

# FINAL MERGE

VHTYWO <- DM_CLEAN %>% 
  full_join(OUT_CLEAN) %>% 
  full_join(SA_CLEAN) %>% 
  full_join(MP_CLEAN) %>% 
  full_join(MB_CLEAN) %>% 
  full_join(VS_CLEAN) %>% 
  full_join(LB_CLEAN)

VHTYWO #%>% View()
VHTYWO %>% count(OUT_IC, OUT_IC_SR, OUT_DC_RELAPSE, OUT_DC)



saveRDS(VHTYWO, "data/cleaned_rds/VHTYWO.rds")