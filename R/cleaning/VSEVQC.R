
############
## VSEVQC ##
############

# IN THE STUDY PUBLICATION (2012) THEY REPORT 

# SSG: 386 (combined VHTYWO and VSEVQC)
# PM: 205 (all VSEVQC)
# SSG & PM: 381 (combined VHTYWO and VSEVQC)

# the 2012 describes data already described in 2010 study
# I am confident that we have no overlap in IPD

## VSEVQC
# SSG 250 (described in 2012 paper entirely)
# PM: 206 (described in 2012 paper entirely +/-1)
# SSG & PM: 246 (described in 2012 paper entirely)

## VHTYWO
# SSG 135 (described in 2010 & 2012 papers)
# PM: 135 (described in 2010 paper entirely)
# SSG & PM: 135 (described in 2010 and 2012 papers)

#

# LTFU are not explicitly documented
# If MBORRES at TOC is positive and rescue treatment starts > 20 afterwards, describe as SLOW RESPONDER & SUBSEQUENT RELAPSE, otherwise NOT a slow responder (just IC failure)

rm(list = ls())
library(tidyverse)

source("R/definitions.R")
load_domains("VSEVQC")
DM %>% count()
DM %>% count(SITEID, ARMCD)
DM %>% count(ARMCD)

# all from November 2006 to June 2009 (approximately)
DM_TIMING <- DM %>% mutate(DM_RFSTDTC = dmy(RFSTDTC)) %>% relocate(DM_RFSTDTC) 
DM_TIMING %>% group_by(ARMCD) %>% 
  summarise(
    earliest = min(DM_RFSTDTC), 
    latest = max(DM_RFSTDTC)
  ) #%>% View()

DM %>% names()
DM_CLEAN <- DM %>% 
  mutate(
    DM_AGE = AGE,
    DM_SEX = SEX,
    DM_ARM = ifelse(ARMCD == "PM", "PM20", ARMCD),
    DM_SITE = SITEID
  ) %>% 
  select(USUBJID, RFSTDTC, starts_with("DM"))

# DS
DS %>% names()
DS %>% count(VISIT, EPOCH, DSTERM, DSDECOD)
DS %>% count(VISIT)
DS %>% count(DSTERM, DSDECOD)
DS %>% count(USUBJID) %>% filter(n > 1) # one entry per patient

DS_CLEAN <- DS %>% 
  mutate(DSTERM_OUT = str_c(DSTERM, ifelse(is.na(DSSTDY), "_NA", str_c("_", as.character(DSSTDY))))) %>% 
  select(USUBJID, DSTERM_OUT)

# RS
RS %>% names()
RS %>% count(VISIT, EPOCH, RSCAT, RSTESTCD)
RS %>% count(RSDY)

RS_CLEAN <- RS %>% 
  mutate(RSORRES_OUT = str_c(RSORRES, ifelse(is.na(RSDY), "_NA", str_c("_", as.character(RSDY))))) %>% 
  mutate(VISIT = toupper(str_replace_all(VISIT, " ", ""))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = RSORRES_OUT,
    names_glue = "RS_{VISIT}"
  ) %>% 
  mutate(
    RS_TOC = case_when(
      !is.na(RS_DAY18) ~ RS_DAY18,
      !is.na(RS_DAY22) ~ RS_DAY22,
      !is.na(RS_DAY31) ~ RS_DAY31,
      .default = "MISSING"
    )) %>% 
    select(-c(RS_DAY18, RS_DAY22, RS_DAY31)) 

# IN

IN %>% names()
IN %>% count(ININDC) %>% arrange(desc(n)) %>% print(n = Inf)

IN_CLEAN <- IN %>% 
  filter(INTRT %in% c("AMPHOTERICIN B", "PENTOSTAM")) %>% 
  group_by(USUBJID, INTRT, INSTRTPT, INSTTPT, INCDSTDY) %>% summarise(INSTDY = min(INSTDY, na.rm = TRUE)) %>% 
  ungroup()

IN_CLEAN <- IN_CLEAN %>% 
  mutate(IN_TIME = ifelse(is.na(INSTDY) | is.infinite(INSTDY), str_c(INSTRTPT, "_", INSTTPT), as.character(INSTDY))) %>% 
  select(USUBJID, INTRT, IN_TIME)

# MB
MB %>% names() 
MB %>% filter(MBTESTCD != "HIV") %>% count(VISIT, EPOCH, MBEVINTX, MBDY, MBTESTCD) %>% print(n = Inf)

MB_CLEAN <- MB %>% 
  filter(MBTESTCD != "HIV") %>% 
  mutate(
    VISIT = ifelse(is.na(VISIT), "Day NA", VISIT),
    VISIT = toupper(str_replace_all(VISIT, " ", ""))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(MBORRES, MBDY),
    names_glue = "MB_{VISIT}_{.value}",
    names_vary = "slowest"
  ) %>% 
  mutate(
    MB_TOC = case_when(
      !is.na(MB_DAY22_MBORRES) ~ MB_DAY22_MBORRES,
      !is.na(MB_DAY18_MBORRES) ~ MB_DAY18_MBORRES,
      !is.na(MB_DAY31_MBORRES) ~ MB_DAY31_MBORRES,
      .default = "MISSING"
    ),
    MB_TOC_MBDY = case_when(
      !is.na(MB_DAY22_MBDY) ~ MB_DAY22_MBDY,
      !is.na(MB_DAY18_MBDY) ~ MB_DAY18_MBDY,
      !is.na(MB_DAY31_MBDY) ~ MB_DAY31_MBDY,
      .default = -Inf
    )
  ) %>% 
  select(-c(MB_DAY22_MBORRES, MB_DAY18_MBORRES, MB_DAY31_MBORRES, MB_DAY22_MBDY, MB_DAY18_MBDY, MB_DAY31_MBDY)) %>% 
  relocate(c(MB_TOC, MB_TOC_MBDY), .after = MB_DAY0_MBDY)

# DEATH
DD_CLEAN <- DM %>% filter(!is.na(DTHDTC) | !is.na(DTHFL)) %>% 
  mutate(DTHDY = dmy(DTHDTC) - dmy(RFSTDTC)) %>% 
  select(USUBJID, DTHDY) 

# OUT MERGE

OUT <- DM_CLEAN %>% select(USUBJID, DM_ARM) %>% 
  full_join(DD_CLEAN) %>% 
  full_join(IN_CLEAN) %>% 
  full_join(DS_CLEAN) %>% 
  full_join(RS_CLEAN) %>% 
  full_join(MB_CLEAN) 

OUT %>% select(-MB_DAY0_MBORRES) %>% 
  relocate(DSTERM_OUT, .after = RS_TOC) %>% 
  arrange(RS_TOC, INTRT, IN_TIME, MB_TOC) #%>% View()

OUT_CLEAN <- OUT %>% 
  mutate(
    OUT_IC = !is.na(RS_TOC) & RS_TOC == "Complete cure_NA",
    OUT_NA = USUBJID %in% c("VSEVQC_Kassab_1024", "VSEVQC_Kassab_1328", "VSEVQC_Kassab_1400"),
    OUT_IC_FAIL = !(OUT_IC | OUT_NA),
    OUT_IC_SR = (MB_TOC != "0" & !is.na(MB_TOC)) & (RS_TOC == "Slow Responder_NA" | 
      USUBJID %in% c(
        "VSEVQC_Kassab_1027", "VSEVQC_Kassab_1085", "VSEVQC_Kassab_1091", "VSEVQC_Kassab_1106",
        "VSEVQC_Kassab_1170", "VSEVQC_Kassab_1259", "VSEVQC_Kassab_1260", "VSEVQC_Kassab_1262",
        "VSEVQC_Kassab_1313", "VSEVQC_Kassab_1409", "VSEVQC_Kassab_1301")
    ),
    OUT_DC_RELAPSE = 
      USUBJID %in% c("VSEVQC_Kassab_1336", "VSEVQC_CCR KEMRI_2089", "VSEVQC_Arba Minch_3009", "VSEVQC_Gondar_4034", "VSEVQC_Kassab_1301", "VSEVQC_Kassab_1409") |
      (INTRT == "AMPHOTERICIN B" & !is.na(INTRT)) & RS_TOC == "Complete cure_NA",
    OUT_DC = DSTERM_OUT == "Complete cure_NA" & is.na(INTRT),
    OUT_IC_DEATH = !is.na(DTHDY),
    OUT_DC_DEATH = FALSE,
    OUT_DC = ifelse(is.na(OUT_DC), FALSE, OUT_DC),
    OUT_IC_SR = ifelse(is.na(OUT_IC_SR), FALSE, OUT_IC_SR),    
  )

OUT_CLEAN %>% names()
OUT_MERGE <- OUT_CLEAN %>% select(USUBJID, MB_DAY0_MBORRES, starts_with("OUT_"))

MB_MERGE <- MB %>% filter(MBTESTCD == "HIV") %>% select(USUBJID, MB_HIV = MBORRES)
SA %>% names()
SA %>% filter(VISIT == "Day 0" & SATERM == "Fever") %>% count(USUBJID) %>% filter(n > 1)
SA_MERGE <- SA %>% filter(VISIT == "Day 0" & SATERM == "Fever") %>% select(USUBJID, SADUR)
SA_MERGE %>% count(SADUR)

VSEVQC <- OUT_MERGE %>% 
  full_join(SA_MERGE) %>% 
  full_join(MB_MERGE) %>% 
  full_join(DM_CLEAN)
VSEVQC %>% names()

saveRDS(VSEVQC, "data/cleaned_rds/VSEVQC.rds")
