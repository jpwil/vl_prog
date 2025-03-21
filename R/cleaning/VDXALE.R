##########
# VDXALE #
##########

# PUBLICATION

# Rijal S, Chappuis F, Singh R, Bovier PA, Acharya P, Karki BM, Das ML, Desjeux P, Loutan L, Koirala S. 
# Treatment of visceral leishmaniasis in south-eastern Nepal: decreasing efficacy of sodium stibogluconate and need for a policy to limit further decline.
# Trans R Soc Trop Med Hyg. 2003 May-Jun;97(3):350-4. doi: 10.1016/s0035-9203(03)90167-2. PMID: 15228258.

# N = 120 recruited
# N = 114 assessed at initial cure (30 or 40 days); (4 died and 2 switched to AMBDC due to cardiotoxicity)
# N = 104 achieved initial cure (10 non-responders)
# N = 100 assessed for definitive cure (4 LTFU)
# N = 99 achieved definitive cure (1 relapse)

# DATASET

# n = 120

rm(list = ls())
source("definitions.R")
load_domains("VDXALE")

mg <- ld_missingness("VDXALE")
mg %>%
  count_na() %>%
  print(width = Inf)

TS #%>% View()

# DM ----
DM %>% names()
DM %>% count(AGE)
DM %>% count(DTHDTC, DTHFL)
DM %>% count(ARMCD)

DM_merge <- DM %>% 
  mutate(
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  select(USUBJID, RFSTDTC, starts_with("DM_"))

DM_merge #%>% View()

# OUTCOME ----

DD # 3 deaths recorded here
DM # 4 deaths recorded

DS %>% names()
DS %>% count_dup()
DS %>% count(VISIT, DSTERM, DSSTDY) %>% print(n = Inf)

RS %>% names()
RS %>% count(VISIT, RSCAT, RSTESTCD, RSORRES)
RS %>% count_dup(VISIT)

RS_pivot <- RS %>% 
  mutate(VISIT = ifelse(is.na(VISIT), "NA", "M6")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT),
    values_from = RSORRES,
    names_glue = "RS_{VISIT}"
  )

DS_pivot <- DS %>% 
  select(USUBJID, DSTERM)

OUT <- RS_pivot %>% full_join(DS_pivot) %>% full_join(DD %>% select(USUBJID, DOMAIN) %>% full_join(DM %>% select(USUBJID, DTHFL)))
OUT #%>% View()

# use MB data to help classify outcomes
MB %>% count_dup(VISIT, MBTESTCD, MBSPEC, MBTSTDTL, MBLOC)
MB_pivot <- MB %>% filter(VISIT %in% c("3 Months", "Day 29", "6 Months")) %>% 
  mutate(VISIT = str_replace_all(VISIT, " ", "")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}_{MBLOC}"
  ) 

OUT %>% full_join(MB_pivot) %>% select(-MB_3Months_SPLEEN, -MB_6Months_SPLEEN) # %>% View()
OUT_clean <- OUT %>% 
  mutate(
    OUT_NA = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DEATH = ifelse(DSTERM == "Death", TRUE, FALSE),
    OUT_IC_OTHER = ifelse(RS_NA %in% c("Non-responder", "Slow-responder") & !is.na(RS_NA), TRUE, FALSE),
    OUT_IC_DRUG = ifelse(RS_NA == "SSG toxicity, replaced by amphotericin B" & !is.na(RS_NA), TRUE, FALSE),
    OUTC_IC_DRUG = "Two patients with SSG toxicity",
    OUTC_IC_OTHER = "11 patients are non-responders, and 1 patient is a slow-responder",
    OUT_DC_RELAPSE = ifelse(USUBJID == "VDXALE_DHARAN_157", TRUE, FALSE),
    OUT_DC_DEATH = FALSE,
    OUT_DC_OTHER = ifelse(DSTERM == "VL (certain), defaulter or lost-to-follow-up", TRUE, FALSE),
    OUTC_DC_OTHER = "These 4 patients are very likely those lost to follow-up as described in the publication",
    OUT_IC = ifelse(OUT_IC_DEATH | OUT_IC_OTHER | OUT_IC_DRUG, FALSE, TRUE),
    OUT_DC = ifelse(!OUT_IC | OUT_DC_OTHER | OUT_DC_RELAPSE, FALSE, TRUE)
    )

OUT %>% full_join(MB_pivot) %>% select(-MB_3Months_SPLEEN, -MB_6Months_SPLEEN) %>% filter(RS_NA %in% c("Non-responder", "Slow-responder", "SSG toxicity, replaced by amphotericin B"))# %>% View()
OUT_clean %>% count(across(starts_with("OUT_"))) %>% mutate(across(everything(), ~as.character(.x)))# %>% View()

VDXALE <- DM_merge %>% full_join(OUT_clean %>% select(USUBJID, starts_with("OUT")))

VDXALE #%>% View()

# MB ----

## ITMA immunoassay performed in blood and serum for each patient during screening
## ? this may be the slightly modified DAT developed at ITMA

# relook June 25th 2024
# 104 patients with baseline bone marrow aspirates
# 5 more patients with baseline splenic aspirates
# 11 patients with no baseline samples
MB %>% names()
MB %>% filter(VISIT == "Screening", MBTESTCD == "LSHMANIA", is.na(MBNAM)) %>%  
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MBTSTDTL, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_BL_{MBTSTDTL}_{MBLOC}"
  ) %>% count(is.na("MB_BL_LSHMANIA_BONE MARROW"))
MB %>% names()
#MB %>% filter(MBTESTCD == "LSHMANIA", is.na(MBNAM)) %>%  count(VISIT, MBORRES, MBNAM, MBSTRF, MBEVINTX)

# include these screening MB tests
MB %>% filter(
  is.na(MBNAM), 
  VISIT == "Screening",
  MBTESTCD != "BACT",
  !(MBTESTCD == "LSHMANIA" & MBTSTDTL == "DETECTION")
  ) %>% count_dup(VISIT, MBTESTCD, MBORRES, MBTEST, MBTSTDTL, MBLOC, MBMETHOD, MBSPEC) #%>% View()
MB %>% 
  filter(
    is.na(MBNAM), 
    VISIT == "Screening",
    MBTESTCD != "BACT",
    !(MBTESTCD == "LSHMANIA" & MBTSTDTL == "DETECTION")) %>% 
  mutate(
    MBLOC = case_when(
      MBLOC == "BONE MARROW" ~ "BONE",
      is.na(MBLOC) & MBMETHOD == "SMEAR" ~ "SMEAR",
      is.na(MBLOC) & MBMETHOD == "ACID FAST STAIN" ~ "SPUTUM_AFB")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MBTESTCD, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_{MBTESTCD}_{MBLOC}"
  ) #%>% View()

MB_pivot <- MB %>% 
  filter(
    is.na(MBNAM), 
    VISIT == "Screening",
    MBTESTCD != "BACT",
    !(MBTESTCD == "LSHMANIA" & MBTSTDTL == "DETECTION")) %>% 
  mutate(
    MBLOC = case_when(
      MBLOC == "BONE MARROW" & !is.na(MBLOC) ~ "BONE",
      is.na(MBLOC) & MBMETHOD == "SMEAR" ~ "SMEAR",
      is.na(MBLOC) & MBMETHOD == "ACID FAST STAIN" ~ "SPUTUM_AFB",
      .default = MBLOC)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MBTESTCD, MBLOC),
    values_from = MBORRES,
    names_glue = "MB_BL_{MBTESTCD}_{MBLOC}"
  ) 

MB_pivot %>% count(across(-c(USUBJID, MB_BL_LSHMANIA_BONE, MB_BL_LSHMANIA_SPLEEN)))

MB_pivot <- MB_pivot %>% 
  rename(
    MB_BL_HIV = MB_BL_HIV_NA,
    MB_BL_LSHMRK39 = MB_BL_LSHMRK39_NA) %>% 
  mutate(
    across(
      -c(USUBJID, MB_BL_LSHMANIA_BONE, MB_BL_LSHMANIA_SPLEEN), 
      ~case_when(
        .x == "Negative" ~ FALSE,
        .x == "Positive" ~ TRUE,
        .default = NA
      )))

MB_pivot #%>% View()
VDXALE <- VDXALE %>% full_join(MB_pivot)
VDXALE #%>% View()

# SA ----
SA %>% names()
SA %>% count(VISIT, SATERM, SAPRESP, !is.na(SADUR))

SA_fever <- SA %>% 
  filter(SATERM == "FEVER") %>% 
  select(USUBJID, SADUR) %>% 
  rename(SA_HX_FEV_DUR = SADUR)

SA_fever #%>% View()

SA %>% filter(SATERM != "FEVER") %>% count(VISIT, SATERM, SAPRESP, SAOCCUR) %>% print(n = Inf)
SA_pivot <- SA %>% 
  filter(SATERM != "FEVER") %>% 
  mutate(
    SATERM = case_when(
      SATERM == "ABDOMINAL PAIN"      ~ "ABDO",
      SATERM == "ANOREXIA"            ~ "APP",
      SATERM == "COUGH"               ~ "CGH",
      SATERM == "DIARRHOEA"           ~ "DIARR",
      SATERM == "EPISTAXIS"           ~ "EPIS",
      SATERM == "HEPATOMEGALY"        ~ "HEP",
      SATERM == "VOMITING"            ~ "VOM",
      SATERM == "WEAKNESS"            ~ "WEAK",
      SATERM == "WEIGHT LOSS"         ~ "WL"
    )
  ) %>% 
  filter(SATERM %in% c("ABDO", "APP", "CGH", "DIARR", "EPIS", "HEP", "VOM", "WEAK", "WL")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR, 
    names_glue = "SA_BL_{SATERM}"
  ) %>% 
  mutate(
    across(
      -USUBJID,
      ~case_when(
        !is.na(.x) & .x == "Y" ~ TRUE,
        !is.na(.x) & .x == "N" ~ FALSE,
        is.na(.x) ~ NA,
        .default = NA
      )
    )
  )

SA_pivot %>% count_dup()
SA_pivot <- SA_fever %>% full_join(SA_pivot)

SA_pivot #%>% View()

VDXALE <- VDXALE %>% full_join(SA_pivot)

# LB ----
# only screening bloods are available :-(
LB %>% names()
LB %>% count_dup(VISIT, LBTESTCD)
LB %>% count(VISIT, LBTESTCD, is.na(LBSTRESN))

LB %>% count(LBSTRF, LBEVINTX)

LB_pivot <- LB %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = LBTESTCD,
    values_from = LBSTRESN,
    names_glue = "LB_BL_{LBTESTCD}"
  )
LB_pivot #%>% View()

VDXALE <- VDXALE %>% full_join(LB_pivot)

# MP ----
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC, MPORRES)

MP_pivot <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 29" ~ "IC",
      VISIT == "Screening" ~ "BL",
      .default = "OTHER"
    )
  ) %>% 
  filter(VISIT != "OTHER") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_SPLEEN_LENGTH"
  )

VDXALE <- VDXALE %>% full_join(MP_pivot)

# MP %>% names()
# MP %>% count(VISIT, MPTESTCD)
# MP

# IN -----
IN %>% names()
IN %>% count(VISIT, INCAT, INTRT, INEVINTX, INOCCUR) #%>% View()
hx_relapses <- IN %>% filter(!is.na(VISIT) & VISIT == "Screening" & INOCCUR == "Y") %>% pull(USUBJID) %>% unique()
VDXALE <- VDXALE %>% 
  mutate(IN_MH_VL = ifelse(USUBJID %in% hx_relapses, TRUE, NA))

# VS ----
VS %>% names()
VS %>% count(VISIT, VSTESTCD)
VS %>% count(VSSTRESN)
VS_merge <- VS %>% select(USUBJID, VSSTRESN) %>% rename(VS_BL_TEMP = VSSTRESN)
VDXALE <- VDXALE %>% full_join(VS_merge)
VDXALE #%>% View()

save(VDXALE, file = "Study/Data/VDXALE.RData")