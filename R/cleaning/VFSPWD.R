############
## VFSPWD ##
############

rm(list = ls())
library(tidyverse)

source("R/definitions.R")
load_domains("VFSPWD")

#DM

DM %>% count()
DM %>% names()
DM %>% count(AGE)

#DM$AGE %>% hist()


DM_CLEAN <- DM %>% 
  mutate(
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_AGE = AGE
  ) %>% 
  select(USUBJID, starts_with("DM_"), RFSTDTC) 

# OUTCOMES

# MB
# patients with MB smears at VISIT = 90 - all 5 are relapses

names(MB)
MB #%>% View()
MB %>% count(MBTESTCD, MBMODIFY, MBTSTDTL, MBORRES, MBSTAT, MBREASND, MBLOC, VISIT, MBDY) #%>% View()
MB %>% filter(MBTESTCD == "LSHMANIA") %>% count(USUBJID, MBTESTCD, VISIT) %>% filter(n > 1)
MB_CLEAN <- MB %>% filter(MBTESTCD == "LSHMANIA") %>% 
  mutate(VISIT = str_replace_all(VISIT, " ", "_")) %>% 
  pivot_wider(
    id_cols = USUBJID, 
    names_from = c(MBTESTCD, VISIT),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}"
  ) %>% 
  relocate(USUBJID, MB_Day_0, MB_Day_22, MB_Day_29, MB_Day_90, MB_Day_180)

RS %>% count(USUBJID, RSTEST, RSSCAT, VISIT) # not uniquely identifying
RS_CLEAN <- RS %>% group_by(USUBJID, RSTEST, RSSCAT, VISIT) %>% 
  arrange(RSDY) %>% 
  mutate(n_unique = row_number())

RS_CLEAN %>% count(USUBJID, RSTEST, RSSCAT, VISIT, n_unique) %>% ungroup() %>% count(n)
RS_CLEAN <- RS_CLEAN %>% ungroup() %>% 
  mutate(result = str_c(ifelse(is.na(RSORRES), "NA", RSORRES), "_", as.character(ifelse(is.na(RSDY), "NA", RSDY)))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(RSTESTCD, RSSCAT, VISIT, n_unique),
    values_from = result,
    names_glue = "RS_{RSTESTCD}_{RSSCAT}_{VISIT}_{n_unique}"
) 

# one entry per patient in DS (final disposition)
DS_CLEAN <- DS %>% 
  mutate(DS_OUT = str_c(DSTERM, "_", as.character(ifelse(is.na(DSSTDY), "NA", DSSTDY)))) %>% 
  select(USUBJID, DS_OUT) 

# IN
IN %>% count(INCAT, ININDC, INTRT)
IN_CLEAN <- IN %>% filter(INCAT == "RESCUE MEDICATION") %>% select(USUBJID, INTRT, INSTDY) %>% distinct() # 8 patients with rescue treatment

OUT <- DM_CLEAN %>% select(USUBJID, DM_ARM) %>% full_join(DS_CLEAN) %>% full_join(IN_CLEAN) %>% full_join(RS_CLEAN) %>% full_join(MB_CLEAN)
names(OUT)
OUT %>% relocate(
  USUBJID, DM_ARM, DS_OUT, INTRT, INSTDY, "RS_TOC_NA_Day 22_1", "RS_TOC_NA_Day 29_1", "RS_TOC_ADDITIONAL OUTCOMES PROVIDED_Day 90_1", "RS_TOC_ADDITIONAL OUTCOMES PROVIDED_Day 90_2",
  "RS_TOC_NA_NA_1", "RS_OVRLRESP_NA_Day 180_1", "RS_OVRLRESP_ADDITIONAL OUTCOMES PROVIDED_Day 180_1", "RS_OVRLRESP_ADDITIONAL OUTCOMES PROVIDED_Day 180_2"
) %>% arrange(DM_ARM, DS_OUT) #%>% View()

OUT_CLEAN <- OUT %>% 
  mutate(
    OUT_IC = 
      (`RS_TOC_NA_Day 22_1` == "Complete cure_NA" & !is.na(`RS_TOC_NA_Day 22_1`)) |
      (`RS_TOC_NA_Day 29_1` == "Complete cure_NA" & !is.na(`RS_TOC_NA_Day 29_1`)),
    OUT_IC_SR = 
      (`RS_TOC_NA_Day 22_1` == "Slow responder_NA" & !is.na(`RS_TOC_NA_Day 22_1`)) |
      (`RS_TOC_NA_Day 29_1` == "Slow responder_NA" & !is.na(`RS_TOC_NA_Day 29_1`)),
    OUT_IC_DEATH = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_LTFU = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_IC_NA = FALSE,
    OUT_IC_EXCLUDE = FALSE,
    OUT_DC = DS_OUT == "Success_NA",
    OUT_DC_LTFU = USUBJID == "VFSPWD_Sudan Kassab Hospital_10",
    OUT_DC_RELAPSE = (DS_OUT != "Success_NA" & USUBJID != "VFSPWD_Sudan Kassab Hospital_10"),
    OUT_DC_EXCLUDE = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_DC_DEATH = FALSE,
    OUT_DC_NA = FALSE,
    OUT_NA = FALSE   
  )

OUT_CLEAN %>% count(across(starts_with("OUT_")))# %>% View()
OUT_CLEAN <- OUT_CLEAN %>% select(USUBJID, starts_with("OUT_"))
OUT_CLEAN #%>% View()

# for final merge
MB_CLEAN <- MB_CLEAN %>% rename(MB_BL_LSHMANIA_BONE = MB_Day_0) %>% select(USUBJID, MB_BL_LSHMANIA_BONE)

#
LB %>% names()
LB %>% filter(is.na(LBSPEC)) %>% count(USUBJID, LBTESTCD, VISIT) %>% filter(n > 1)
LB %>% count(VISIT)

LB_CLEAN <- LB %>% filter(VISIT == "Day 0", is.na(LBSPEC)) %>% pivot_wider(
  id_cols = USUBJID,
  names_from = LBTESTCD,
  values_from = LBSTRESN, 
  names_glue = "LB_{LBTESTCD}"
)

LB_CLEAN #%>% View()

# 
MP %>% names()
MP %>% count(MPTESTCD)
MP %>% filter(VISIT == "Day 0") %>% count(MPTESTCD, MPORRES, MPLOC, VISIT) 
MP_CLEAN <- MP %>% filter(VISIT == "Day 0") %>% pivot_wider(
  id_cols = USUBJID,
  names_from = MPLOC,
  values_from = MPORRES,
  names_glue = "MP_BL_{MPLOC}_LENGTH"
)

# 
VS %>% names()
VS %>% count(USUBJID, VSTESTCD, VISIT) %>% filter(n > 1)

VS_CLEAN <- VS %>% filter(VISIT == "Day 0") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_BL_{VSTESTCD}"
  )

# 
# no fever duration
SA %>% names()
SA %>% filter(VISIT == "Day 0") %>% count(SACAT, SAPRESP, SATERM, SAOCCUR,VISIT, !is.na(SASTDY), !is.na(SAENDY)) #%>% View()
SA %>% filter(VISIT == "Day 0", SATERM == "Fever")  #%>% View()

SA_CLEAN <- SA %>% filter(VISIT == "Day 0", !is.na(SAPRESP)) %>% 
  mutate(SATERM = toupper(str_replace_all(SATERM, " ", "_"))) %>% 
  select(USUBJID, SATERM, SAOCCUR) %>% 
  mutate(SAOCCUR_BOOL = ifelse(SAOCCUR == "Y", TRUE, FALSE)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR_BOOL,
    names_glue = "SA_BL_{SATERM}"
  )

VFSPWD <- DM_CLEAN %>% 
  full_join(MB_CLEAN) %>% 
  full_join(OUT_CLEAN) %>% 
  full_join(LB_CLEAN) %>% 
  full_join(VS_CLEAN) %>% 
  full_join(MP_CLEAN) %>% 
  full_join(SA_CLEAN)

#VFSPWD %>% View()

saveRDS(VFSPWD, "data/cleaned_rds/VFSPWD.rds")