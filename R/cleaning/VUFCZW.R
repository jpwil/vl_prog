############
## VUFCZW ##
############

rm(list = ls())
library(tidyverse)
source("R/definitions.R")
load_domains("VUFCZW")

DM %>% names()
DM %>% count()
DM %>% count(SITEID)
DM %>% count(SEX)
DM %>% count(ARMCD)
DM %>% count(DTHFL, DTHDTC)

DM_CLEAN <- DM %>% 
  mutate(
    DM_DEATH = if_else(DTHFL == "Y" & !is.na(DTHFL), TRUE, FALSE)
  ) %>% 
  select(
    USUBJID,
    DM_AGE = AGE,
    DM_SITE = SITEID,
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    DM_DEATH,
    RFSTDTC
  )

## OUTPUT

# DD
DD %>% count(USUBJID) %>% filter(n > 1)
DD %>% count(DDTEST, VISIT)
DD_CLEAN <- DD %>% 
  mutate(DD_OUT = if_else(VISIT == "DAY 210", "DEATH AFTER EOT", "DEATH BEFORE EOT")) %>% 
  select(USUBJID, DD_OUT)

# RS
RS %>% names()
RS %>% count(RSCAT, RSSCAT, VISIT, RSTESTCD)
RS %>% count(USUBJID, RSTESTCD) %>% filter(n > 1)

RS_CLEAN <- RS %>% 
  mutate(
    RS_OUT = case_when(
      is.na(VISIT) ~ "ADDITIONAL",
      VISIT == "DAY 210" ~ "D210",
      VISIT == "DAY 30" ~ "D30",
      .default = NA
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = RS_OUT,
    values_from = RSORRES,
    names_glue = "RS_{RS_OUT}"
  )

# DS
DS %>% names()
DS %>% count(VISIT, EPOCH, DSTERM)
DS %>% count(USUBJID, VISIT) %>% filter(n > 1)

DS %>% filter(VISIT == "DAY 210" & USUBJID == "VUFCZW_DENSHA_016")# %>% View()

DS_CLEAN1 <- DS %>% 
  mutate(
    DS_OUT = case_when(
      VISIT == "DAY 30" ~ "D30",
      VISIT == "DAY 210" & USUBJID != "VUFCZW_DENSHA_016" ~ "D210",
      VISIT == "DAY 210" & USUBJID == "VUFCZW_DENSHA_016" & DSDECOD == "COMPLETED" ~ "D210",
      VISIT == "DAY 210" & USUBJID == "VUFCZW_DENSHA_016" & DSDECOD == "DEATH" ~ "D210XTRA",
      .default = NA
    ))

DS_CLEAN1 %>% count(USUBJID, DS_OUT) %>% filter(n > 1)


DS_CLEAN2 <- DS_CLEAN1 %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = DS_OUT,
    values_from = DSTERM,
    names_glue = "DS_{DS_OUT}"
  )
DS_CLEAN2 # View()

# PT
PT %>% names()
PT %>% count(VISIT, EPOCH, PTTRT)

# IN # no information on rescue treatment here or PMHx
IN %>% count()
IN %>% names()
IN %>% count(EPOCH, VISIT)
IN %>% count(INTRT)

# MB
MB %>% count()
MB %>% names()
MB %>% count(EPOCH, VISIT, MBTESTCD, MBLOC)
MB %>% filter(MBTESTCD == "LSHMANIA") %>% count(USUBJID, VISIT) %>% filter(n > 1)

MB %>% filter(EPOCH == "BASELINE" & MBTESTCD == "LSHMANIA") #%>% View()

MB %>% filter(is.na(MBSTAT), MBTESTCD == "LSHMANIA") %>% 
  count(VISIT, MBTESTCD, MBLOC)

MB %>% filter(is.na(MBSTAT), MBTESTCD == "LSHMANIA") %>% 
  count(USUBJID, VISIT, MBTESTCD, MBLOC) %>% filter(n > 1)

MB_CLEAN1 <- MB %>% filter(is.na(MBSTAT), MBTESTCD == "LSHMANIA", MBSPEC == "TISSUE" & !is.na(MBSPEC)) %>% 
  mutate(
    MBLOC_OUT = case_when(
      MBLOC == "LYMPH NODE" ~ "LYMPH",
      is.na(MBLOC) ~ "NA",
      .default = MBLOC
    ),
    VISIT_OUT = case_when(
      VISIT == "DAY 30" ~ "D30",
      VISIT == "DAY 37" ~ "D37",
      VISIT == "SCREENING" ~ "D1",
      .default = VISIT
    )) %>% 
  group_by(USUBJID, MBLOC_OUT, VISIT_OUT) %>% 
  arrange(MBDY) %>% 
  mutate(N = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT_OUT, MBLOC_OUT, N),
    values_from = MBORRES,
    names_glue = "MB_{VISIT_OUT}_{MBLOC_OUT}_{N}"
  )

MB_CLEAN2 <- MB_CLEAN1 %>% 
  select(-MB_D1_NA_2) #%>% View()

# OUT
OUT <- DM_CLEAN %>% 
  full_join(DD_CLEAN) %>% 
  full_join(RS_CLEAN) %>% 
  full_join(DS_CLEAN2) %>% 
  full_join(MB_CLEAN2)

OUT %>% select(-DM_DEATH)  %>% View()

## Initial cure:
# 1 patient with initial TOC positive and subsequent negative: VUFCZW_DENSHA_011
# 1 patient defaulted before initial: TOC VUFCZW_DENSHA_348
# remaining patients discharged after EOT (either with negative TOC or with no TOC)

## Definitive cure:
# of the 4 patients who died after TOC - 1 relapsed (VUFCZW_DENSHA_016)
# there are 2 other relapses (3 relapses in total)
# unable to identify the 32 patients who were LTFU after initial cure


OUT_CLEAN <- OUT %>% 
  mutate(
    OUT_IC_DEATH = if_else(DD_OUT == "DEATH BEFORE EOT" & !is.na(DD_OUT), TRUE, FALSE),
    OUT_DC_DEATH = if_else(DD_OUT == "DEATH AFTER EOT" & !is.na(DD_OUT), TRUE, FALSE),
    OUT_IC_OTHER = if_else(USUBJID == "VUFCZW_DENSHA_348", TRUE, FALSE), # defaulter
    OUT_IC_SR = if_else(USUBJID == "VUFCZW_DENSHA_011", TRUE, FALSE), # slow responder (treatment extended and repeat aspirate negative a week later)
    OUT_IC = if_else(OUT_IC_SR | OUT_IC_OTHER | OUT_IC_DEATH, FALSE, TRUE),
    OUT_DC_RELAPSE = if_else(DS_D210 == "Relapse" & !is.na(DS_D210), TRUE, FALSE),
    OUT_DC = FALSE, # unable to know which patients were LTFU (n = 32)
    OUT_DC_NA = if_else(OUT_DC_RELAPSE | OUT_DC_DEATH, FALSE, TRUE)
  ) %>% select(USUBJID, starts_with("OUT_"))

OUT_CLEAN %>% count(pick(starts_with("OUT_"))) #%>% View()

# MB

MB %>% names()
MB %>% count(MBTESTCD)

MB_HIV <- MB %>% filter(MBTESTCD == "HIV12AB", !is.na(MBORRES))
MB_HIV %>% count(MBTSTDTL, MBORRES, MBMETHOD)
MB_HIV2 <- MB_HIV %>% group_by(USUBJID) %>% arrange(MBDY) %>% mutate(N = row_number()) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = N, 
    values_from = MBORRES,
    names_glue = "MB_HIV12AB_{N}"
  )  %>% ungroup() #%>% View()

MB_HIV2 %>% names()
MB_HIV2 %>% count(MB_HIV12AB_1, MB_HIV12AB_2)

# 145 patients have 2 HIV tests
# 5 patients have 1 HIV test (all negative)

# label as positive if at least one test is positive
MB_HIV3 <- MB_HIV2 %>% 
  mutate(
    HIVAB = if_else((MB_HIV12AB_1 == "POSITIVE" & !is.na(MB_HIV12AB_1)) | (MB_HIV12AB_2 == "POSITIVE" & !is.na(MB_HIV12AB_2)), TRUE, FALSE, missing = NA)
  ) %>% select(USUBJID, MB_BL_HIVAB = HIVAB)

# serology
MB_SER <- MB %>% filter(MBSPEC == "SERUM") #%>% View()
MB_SER %>% count(USUBJID) %>% filter(n > 1)

# one patient had two DATs performed: VUFCZW_DENSHA_341. One borderline and one negative. Choose the borderline one. 
MB_SER <- MB %>% filter(
  MBSPEC == "SERUM",
  !(USUBJID == "VUFCZW_DENSHA_341" & MBORRES == "NEGATIVE")) 
MB_SER %>% count(USUBJID) %>% filter(n > 1)
MB_SER <- MB_SER %>% 
  select(USUBJID, MB_BL_SER = MBORRES)

# tissue. VUFCZW_DENSHA_347 and VUFCZW_DENSHA_295 have 2 x tissue aspirates recorded (all 4 aspirates are 2+)
MB_CLEAN1 %>% View()
MB_CLEAN1 %>% count(MB_D1_SPLEEN_1, MB_D1_NA_1, MB_D1_NA_2)
MB_CLEAN2 <- MB_CLEAN1 %>% select(-MB_D1_NA_2, -MB_D37_NA_1)
MB_CLEAN3 <- MB_CLEAN2 %>% 
  select(
    USUBJID,
    MB_BL_LSHMANIA_SPLEEN = MB_D1_SPLEEN_1,
    MB_BL_LSHMANIA_NA = MB_D1_NA_1,
    MB_IC_LSHMANIA_SPLEEN = MB_D30_SPLEEN_1,
    MB_IC_LSHMANIA_LYMPH = MB_D30_LYMPH_1
  )

MB_CLEAN3 %>% count(pick(starts_with("MB_")))

# MP
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP %>% count(USUBJID, VISIT, MPTESTCD, MPLOC) %>% filter(n > 1)

MP_CLEAN <- MP %>% 
  filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MPTESTCD, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_BL_{MPTESTCD}_{MPLOC}"
  )

MP_CLEAN # %>% View()

# LB
LB %>% names()
LB %>% count(LBTESTCD)
LB %>% count(VISIT, LBTESTCD)
LB %>% filter(VISIT == "SCREENING") %>% count(USUBJID) %>% filter(n > 1)
LB_CLEAN <- LB %>% filter(VISIT == "SCREENING") %>% select(USUBJID, LB_BL_HGB = LBSTRESN) 

# SA
# no illness duration here
SA %>% names()
SA %>% count(VISIT, SATERM, SAPRESP) %>% filter(SAPRESP == "Y") %>% print(n = Inf)
SA %>% count(VISIT, SATERM, SAPRESP) %>% filter(VISIT == "SCREENING") %>% print(n = Inf)
SA %>% filter(VISIT == "SCREENING") #%>% View()
SA %>% filter(VISIT == "SCREENING") %>% count(SATERM, SAPRESP, SAOCCUR) %>% arrange(SATERM) %>% View()

SA %>% filter(VISIT == "SCREENING") %>% count(SATERM)  %>% arrange(desc(n)) %>% print(n = Inf)
SA_CLEAN <- SA %>% filter(VISIT == "SCREENING", SATERM %in% c("Chronic Diarrhea", "EDEMA/ASCITES", "Herpes Zoster", "Lymphadenopathy")) %>% 
  mutate(
    SATERM_NEW = toupper(str_replace(SATERM, " ", "_")),
    SATERM_NEW = str_replace(SATERM_NEW, "/", "_"),
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM_NEW,
    values_from = SAOCCUR,
    names_glue = "SA_BL_{SATERM_NEW}"
  )

# VS
VS %>% names()
VS %>% count(VISIT, VSTESTCD)
VS_CLEAN <- VS %>% 
  filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSORRES,
    names_glue = "VS_BL_{VSTESTCD}"
  )

VS_CLEAN %>% head()

# RP
RP %>% names()
RP %>% count(VISIT, RPTESTCD, RPORRES)
RP_CLEAN <- RP %>% 
  filter(RPTESTCD == "PREGIND") %>% 
  select(
    USUBJID,
    RP_PREG = RPORRES
  )

VUFCZW <- DM_CLEAN %>% 
  full_join(OUT_CLEAN) %>% 
  full_join(MB_HIV3) %>% 
  full_join(MB_SER) %>% 
  full_join(MB_CLEAN3) %>% 
  full_join(MP_CLEAN) %>% 
  full_join(LB_CLEAN) %>% 
  full_join(SA_CLEAN) %>% 
  full_join(VS_CLEAN) %>% 
  full_join(RP_CLEAN) %>% 
  select(-c(DM_DEATH))

#VUFCZW %>% View()
