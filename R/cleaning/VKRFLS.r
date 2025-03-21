############
## VKRFLS ##
############

rm(list = ls())
library(tidyverse)

source("R/definitions.R")
load_domains("VKRFLS")

DM %>% names()
DM %>% count()
DM %>% count(dmy(RFSTDTC)) %>% summary()
DM %>% count(ARMCD)
DM %>% count(ARM)
DM %>% count(DTHFL, dmy(DTHDTC)) %>% summary()
DM %>% count(SITEID)
DM %>% count(SEX)

VKRFLS <- DM %>% 
  mutate(
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    RFSTDTC = dmy(RFSTDTC),
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  select(USUBJID, starts_with("DM_"), RFSTDTC)

# PT
PT %>% names()
PT %>% count(PTTRT, PTDOSRGM, PTDOSFRQ)
PT #%>% View()

# IN
# relapse treatments not included here
IN %>% names
IN %>% count(DOTIND)
IN %>% count(INDOSRGM)
IN %>% count(INDUR) %>% print(n = Inf)
IN %>% count(INCAT, INTRT, EPOCH, VISIT)
IN %>% filter(INCAT == "MEDICAL HISTORY") #%>% View()

# previous treatment with SSG
IN %>% filter(INCAT == "MEDICAL HISTORY") %>% pull(USUBJID) %>% unique() -> temp
VKRFLS <- VKRFLS %>% 
  mutate(IN_MH_VL = ifelse(USUBJID %in% temp, TRUE, FALSE))

# RS
RS %>% names()
RS %>% count(RSCAT, RSSCAT, VISIT, RSTESTCD)
RS %>% count(RSEVINTX)
RS %>% count(RSCAT, RSSCAT, RSTESTCD, VISIT, RSDY) %>% print(n = Inf)
RS %>% count(USUBJID) %>% count(n)

RS_TEMP <- RS %>% 
  mutate(
    RSTESTCD = ifelse(!is.na(RSSCAT), str_c(RSTESTCD, "2"), str_c(RSTESTCD, "1"))
  )

RS_TEMP %>% count(USUBJID, VISIT, RSTESTCD) %>% filter(n > 1) %>% pull(USUBJID) %>% unique() -> temp
RS_TEMP %>% filter(USUBJID %in% temp) %>% arrange(USUBJID) %>% count(USUBJID, VISIT, RSTESTCD) #%>% View()

RS_TEMP %>% names()
RS_TEMP %>% group_by(USUBJID, VISIT, RSTESTCD) %>% arrange(RSDY) %>% 
  mutate(N = row_number()) %>% ungroup() -> RS_TEMP2

RS_TEMP2 %>% count(USUBJID, VISIT, RSTESTCD, N) %>% filter(n > 1)

RS_TEMP2 %>% 
  mutate(VISIT = str_replace(VISIT, " ", "_")) %>%
  pivot_wider( 
    id_cols = USUBJID, 
    names_from = c(VISIT, RSTESTCD, N),
    values_from = c(RSORRES, RSDY),
    names_glue = "RS_{VISIT}_{RSTESTCD}_{N}_{.value}",
    names_vary = "slowest"
  ) -> RS_CLEAN

RS_CLEAN <- RS_CLEAN %>% arrange(RS_NA_TOC1_3_RSORRES, RS_NA_TOC1_4_RSORRES, RS_NA_TOC1_5_RSORRES) %>% 
  select(-c(RS_NA_TOC1_3_RSORRES, RS_NA_TOC1_3_RSDY, RS_NA_TOC1_4_RSORRES, RS_NA_TOC1_4_RSDY, RS_NA_TOC1_5_RSORRES, RS_NA_TOC1_5_RSDY)) 

# DS
DS %>% names()
DS %>% count(VISIT, DSTERM)
DS %>% filter(VISIT == "SCREENING")  # patients with previous relapse

DS %>% count(USUBJID, VISIT, DSTERM) %>% filter(n > 1)
DS_CLEAN <- DS %>% 
  filter(VISIT != "SCREENING") %>% 
  mutate(VISIT = ifelse(VISIT == "DAY 28", "DAY 30", VISIT)) %>% 
  mutate(VISIT = str_replace(VISIT, " ", "_")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(DSTERM, DSSTDY),
    names_glue = "DS_{VISIT}_{.value}",
    names_vary = "slowest"
  )

# ignore RS_NA_OVRLRESP2_1_RSORRES - it is almost always "Final cure at 6M"

OUT <- DS_CLEAN %>% full_join(RS_CLEAN) 
OUT %>% select(DS_DAY_30_DSTERM, DS_DAY_210_DSTERM, RS_DAY_210_OVRLRESP1_1_RSORRES) #%>% View()
OUT %>% count(DS_DAY_30_DSTERM)

OUT_CLEAN <- OUT %>% 
  mutate(
    OUT_IC = ifelse(DS_DAY_30_DSTERM %in% c("28/30 days treatment, discharged with negative test of cure", "28/30 days treatment, discharged without TOC test"), TRUE, FALSE),
    OUT_IC_DEATH = ifelse(DS_DAY_30_DSTERM == "died during treatment", TRUE, FALSE),
    OUT_IC_NA = FALSE,
    OUT_IC_OTHER = DS_DAY_30_DSTERM == "extended treatment after first positive TOC" & !is.na(DS_DAY_30_DSTERM),
    OUT_IC_FAIL = ifelse(DS_DAY_30_DSTERM %in% c("defaulted treatment", "died during treatment", "extended treatment after first positive TOC"), TRUE, FALSE),
    OUT_DC = ifelse(DS_DAY_210_DSTERM %in% c("Final Cure at 6m"), TRUE, FALSE),
    OUT_DC_RELAPSE = ifelse(DS_DAY_210_DSTERM == "Relapse" & !is.na(DS_DAY_210_DSTERM), TRUE, FALSE),
    OUT_DC_OTHER = ifelse(DS_DAY_210_DSTERM %in% c("Primary Treatment Failure", "Defaulter", "Developed PKDL"), TRUE, FALSE),
    OUT_DC_NA = ifelse(is.na(DS_DAY_210_DSTERM), TRUE, FALSE), # these are either patients who defaulted/died during initial treatment, or LTFU
    OUT_NA = FALSE,
    OUT_DC_DEATH = DS_DAY_210_DSTERM == "Died" & !is.na(DS_DAY_210_DSTERM)
  )

OUT_CLEAN %>% count(pick(starts_with("OUT_"))) #%>% View()
OUT_CLEAN <- OUT_CLEAN %>% select(USUBJID, starts_with("OUT_"))

# confirm death outcomes by matching with DTHFL and DTHDTC in DM domain - they match
#VKRFLS %>% full_join(DM %>% select(USUBJID, DTHFL, DTHDTC)) %>% count(OUT_IC_DEATH, OUT_DC_DEATH, DTHFL, DTHDTC) #%>% View()

# OUT_IC is FALSE if patient required extended treatment - these patients can still relapse, or LTFU or default between IC - DC duration
# OUT_IC_OTHER = TRUE here if extended treatment
# LTFU is not explicit in outcomes, only implied
# OUT_DC_NA means literally NA in the DSTERM field

DS %>% names()
DS %>% count(VISIT)
DS %>% filter(VISIT == "SCREENING") %>% count(USUBJID) %>% pull(USUBJID) -> temp

VKRFLS <- VKRFLS %>% full_join(OUT_CLEAN)
VKRFLS <- VKRFLS %>% mutate(
  DS_MH_VL = ifelse(USUBJID %in% temp, TRUE, FALSE)
)

MB %>% names()
MB %>% count(VISIT, MBTESTCD)
MB %>% filter(MBTESTCD == "HIV12AB") %>% count(USUBJID, MBORRES) %>% count(MBORRES, n)

MB %>% filter(MBTESTCD == "HIV12AB")# %>% View()

# merge with HIV data
MB_HIV <- MB %>% filter(MBTESTCD == "HIV12AB") %>% 
  group_by(USUBJID) %>% mutate(N = row_number()) %>% ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = N,
    values_from = MBORRES,
    names_glue = "MB_BL_HIV_{N}"
  ) %>% 
  mutate(MB_BL_HIV = ifelse(MB_BL_HIV_1 == "NEGATIVE" & MB_BL_HIV_2 == "NEGATIVE", FALSE, TRUE)) %>% 
  select(USUBJID, MB_BL_HIV) # only call HIV negative if both tests are negative

VKRFLS <- VKRFLS %>% full_join(MB_HIV)
VKRFLS %>% count(MB_BL_HIV, OUT_IC_OTHER, OUT_IC, OUT_IC_FAIL) %>% print(n = Inf) # most of the HIV patients not reported as having received extended treatment despite protocol

# merge with serology 
MB %>% filter(MBTESTCD == "LSHMANIA", MBSPEC == "SERUM") %>% 
  filter(USUBJID %in% USUBJID[duplicated(USUBJID)]) # %>% View()

MB_SEROLOGY <- MB %>% filter(MBTESTCD == "LSHMANIA", MBSPEC == "SERUM") %>% 
  group_by(USUBJID) %>% arrange(MBDY) %>% mutate(N = row_number()) %>% ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = N,
    values_from = MBORRES,
    names_glue = "MB_DAT_{N}"
  ) %>% 
  mutate(
    MB_BL_DAT = ifelse(MB_DAT_1 == "POSITIVE" | (MB_DAT_2 == "POSITIVE" & !is.na(MB_DAT_2)), "POSITIVE", MB_DAT_1)
  )  %>% select(USUBJID, MB_BL_DAT)
# if there are two screening serology (DAT), then pick the result such that POSITIVE > BORDERLINE (there are no negative results)

# parasite count
MB %>% names()
MB %>% count(VISIT, MBTESTCD, MBMETHOD, MBLOC)
MB %>% filter(MBTESTCD == "LSHMANIA", MBMETHOD == "MICROSCOPY") %>% count(USUBJID, VISIT, MBLOC) %>% count(n)

# there are two patients with two screening aspirates: 
# VKRFLS_MYCADRA_483 with 2 x lymph node aspirates
# VKRFLS_MYCADRA_557 with 1 x lymph node and 1 x splenic aspirate
# for both patients, the first aspirate is negative and the second is positive
# for purposes of data harmonisation and pivoting, only include the positive (second) aspirate
MB %>% filter(MBTESTCD == "LSHMANIA", MBMETHOD == "MICROSCOPY") %>% 
  group_by(USUBJID, VISIT) %>% 
  filter(USUBJID %in% USUBJID[duplicated(USUBJID)]) #%>% View()

MB_MICRO <- MB %>% 
  filter(
    MBTESTCD == "LSHMANIA", 
    MBMETHOD == "MICROSCOPY",
    !(USUBJID == "VKRFLS_MYCADRA_483" & VISIT == "SCREENING" & MBORRES == "0"),
    !(USUBJID == "VKRFLS_MYCADRA_557" & VISIT == "SCREENING" & MBORRES == "0"),
    VISIT != "DAY 37"
  ) %>% 
  mutate(
    VISIT = ifelse(VISIT == "DAY 30", "IC", "BL") 
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}_LSHMANIA_{MBLOC}"
  ) 

VKRFLS <- VKRFLS %>% full_join(MB_MICRO) %>% full_join(MB_SEROLOGY)
VKRFLS %>% count(DM_SITE, MB_BL_DAT, pick(starts_with("MB_BL_LSHMANI")))#%>% View()

# SA
SA %>% names()
SA %>% filter(SAPRESP == "Y", VISIT == "SCREENING") %>% count(SATERM, SAOCCUR)
SA %>% filter(SAPRESP == "Y", VISIT == "SCREENING") %>% count(USUBJID, SATERM, SAOCCUR) %>% count(n)

SA_CLEAN <- SA %>% filter(SAPRESP == "Y", VISIT == "SCREENING") %>% 
  mutate(
    SATERM = str_replace(SATERM, " ", "_"),
    SA_OUT = case_when(
      SAOCCUR == "N" ~ FALSE,
      SAOCCUR == "Y" ~ TRUE,
      is.na(SAOCCUR) ~ NA,
      .default = NA
    )
  ) %>%  
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SA_OUT,
    names_glue = "SA_HX_{SATERM}"
  )

# duration info only available for diarrhoea and vomiting at TOC visit
SA %>% count(VISIT, SATERM) %>% filter(VISIT == "SCREENING") %>% arrange(desc(n)) #%>% print(n = Inf)
SA %>% count(VISIT, SATERM, !is.na(SADUR)) # %>% View()

VKRFLS <- VKRFLS %>% full_join(SA_CLEAN)

# VS
VS %>% names()
VS %>% count(VISIT, VSTESTCD)
VS %>% count(USUBJID, VISIT, VSTESTCD) %>% filter(n > 1)

VS %>% filter(VISIT == "SCREENING") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSORRES,
    names_glue = "VS_BL_{VSTESTCD}"
  ) -> VS_CLEAN

# MP
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP_CLEAN <- MP %>% filter(VISIT == "SCREENING") %>% 
  select(USUBJID, MPORRES) %>% 
  rename(MP_BL_SPLEEN_LENGTH = MPORRES)

# LB
LB %>% count(VISIT, LBTESTCD)
LB_CLEAN <- LB %>% filter(VISIT == "SCREENING") %>% 
  select(USUBJID, LBSTRESN) %>% 
  rename(LB_BL_HGB = LBSTRESN)

VKRFLS <- VKRFLS %>% 
  full_join(MP_CLEAN) %>% 
  full_join(LB_CLEAN) %>% 
  full_join(VS_CLEAN)


saveRDS(VKRFLS, "data/cleaned_rds/VKRFLS.rds")
