# PUBLICATION

# According to IDDO repository, this study is contributed by DNDi
# On the protocol there is no mention of DNDi (TDR funded)

# no publication (why?)
# there is a protocol: https://anzctr.org.au/Trial/Registration/TrialReview.aspx?id=335131&isReview=true
# Registration number: ACTRN12610000130066 (Australian New Zealand Clinical Trials Registry)

# DATABASE

# n = 35
# 1 relapse identified
# 1 treatment failure identified
# 1 SAE and 1 patient withdrew consent BL-IC

rm(list = ls())
source("definitions.R")
load_domains("VAQMOU")

mg <- ld_missingness("VAQMOU")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----
DM %>% names()
DM %>% count(SEX)
DM %>% count(ARMCD)
DM %>% count(AGE) %>% print(n = 50)

DM_merge <- DM %>% 
  rename(
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  select(USUBJID, RFSTDTC, starts_with("DM_"))

DM_merge #%>% View()
VAQMOU <- DM_merge

# OUTCOME ----
# VAQMOU_DHARAN_17 appears to be a relapse?

DS %>% names()
DS %>% count(VISIT, DSTERM, DSDECOD)
DS %>% count_dup()

RS %>% names()
RS %>% count(VISIT, RSORRES, RSTESTCD)
RS_merge <- RS %>% 
  mutate(
    VISIT = ifelse(VISIT == "6 Months", "DC", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(RSORRES, RSDY),
    names_glue = "RS_{VISIT}_{.value}"
  )
RS_merge %>% print(n = Inf)
RS_merge %>% full_join(DS %>% select(USUBJID, DSTERM, DSDECOD)) #%>% View()

# MB ----
# mostly bone marrow (MBSPEC and MBLOC contains location of aspirate, not just MBLOC)
# VAQMOU_DHARAN_17 relapsed (MBDY is 175), which corresponds with data from RS

MB %>% names()
MB %>% count(VISIT, MBTESTCD, MBSPEC, MBLOC)
MB %>% count(VISIT, MBLOC, MBDY) %>% print(n = Inf)
MB_merge <- MB %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBLOC),
    values_from = MBORRES
  )
OUT <- MB_merge %>% full_join(RS_merge) %>% full_join(DS %>% select(USUBJID, DSTERM, DSSTDY)) #%>% View()
MB %>% filter(USUBJID == "VAQMOU_DHARAN_17") #%>% View()

OUT #%>% View()
OUT_merge <- OUT %>% 
  mutate(
    OUT_IC_DEATH = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = is.na(RS_DC_RSORRES),
    OUTC_IC_OTHER = "3 patients: 1 with 'INITIAL FAILURE', 1 with 'Consent withdraw=n on day 11, and 1 with 'SAE' on day 11",
    OUT_NA = FALSE,
    OUT_DC_RELAPSE = USUBJID == "VAQMOU_DHARAN_17",
    OUT_DC_DEATH = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_IC = ifelse(OUT_IC_OTHER, FALSE, TRUE),
    OUT_DC = ifelse(!OUT_IC | OUT_DC_RELAPSE, FALSE, TRUE)
    ) %>% 
  select(USUBJID, starts_with("OUT"))

OUT_merge %>% count(across(starts_with("OUT_"))) #%>% View()

VAQMOU <- VAQMOU %>% full_join(OUT_merge)

MB_merge2 <- MB_merge %>% select(USUBJID, MB_BL_LSHMANIA_BONE = Screening_NA, MB_BL_LSHMANIA_SPLEEN = Screening_SPLEEN) #%>% 

VAQMOU <- VAQMOU %>% full_join(MB_merge2)
VAQMOU %>% names()

# IN ----
# no history of relapse data here
IN %>% names()
IN %>% count(VISIT, INCAT, INTRT) %>% print(n = Inf)
IN %>% filter(INCAT == "RESCUE MEDICATION") # this is the same patient that relapsed

# PT ----

PT %>% names()
PT %>% count(VISIT, PTTRT)

# LB ----
LB %>% names()
LB %>% count(LBTESTCD)
LB %>% count(VISIT, LBTESTCD, LBSPEC) %>% print(n = Inf)
LB_merge1 <- LB %>% 
  filter(VISIT %in% c("Screening", "Day 19"), is.na(LBSPEC)) %>% 
  mutate(VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("LB_")) %>% 
  select(-c(LB_IC_PTAC, LB_BL_HCG, LB_BL_PTAC)) 

LB_merge2 <- LB %>% 
  filter(LBTESTCD %in% c("PTAC", "HCG")) %>% 
  filter(VISIT %in% c("Screening", "Day 19"), is.na(LBSPEC)) %>% 
  mutate(VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBORRES,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("LB_")) %>% 
  mutate(LB_BL_HCG = ifelse(LB_BL_HCG == "Negative", FALSE, NA))

#LB_merge2 %>% count(LB_BL_HCG)
LB_merge1 %>% names()
LB_merge2 %>% names()

VAQMOU <- VAQMOU %>% full_join(LB_merge1) %>% full_join(LB_merge2)
VAQMOU %>% names()

# MP ----

MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP_merge <- MP %>% 
  filter(!is.na(VISIT) & VISIT %in% c("Day 19", "Screening")) %>% 
  mutate(VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_SPLEEN_LENGTH"
  ) 

MP_merge #%>% View()
VAQMOU <- VAQMOU %>% full_join(MP_merge)

# SA ----
# no useful information here - all patients confirmed as not having HIV, malaria or TB
SA %>% names()
SA %>% count(VISIT, SACAT, SASCAT, SATERM, SAOCCUR)

# VS ----
# TEMP, SYSBP, DIABP, RESP, PULSE
# Screening weight!

VS %>% names()
VS %>% count(VISIT, VSTESTCD) %>% print(n = Inf)
VS %>% count(VSTESTCD)

VS_merge <- VS %>% 
  filter(VISIT %in% c("Screening", "Day 19")) %>% 
  mutate(VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  )
VS_merge #%>% View()
VAQMOU <- VAQMOU %>% full_join(VS_merge)

# RP ----
RP %>% names()
RP %>% count(VISIT, RPTEST, RPORRES)
RP_merge <- RP %>% mutate(RP_PREG = FALSE) %>% select(USUBJID, RP_PREG)

VAQMOU <- VAQMOU %>% full_join(RP_merge)

save(VAQMOU, file = "Study/Data/VAQMOU.RData")