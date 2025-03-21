##########
# VWPJRM #
##########

# PUBLICATION

# Sundar S, Jha TK, Thakur CP, Sinha PK, Bhattacharya SK. Injectable paromomycin for Visceral leishmaniasis in India. 
# N Engl J Med. 2007 Jun 21;356(25):2571-81. doi: 10.1056/NEJMoa066536. PMID: 17582067.

# n = 667 from across 4 sites. No disaggregation by site in the publication. 
# 1 death between recruitment and starting treatment, and 3 deaths during treatment (3 in paromomycin, 1 in amphotericin)
# 22 relapses (all in paromomycin group)
# 1 LTFU in amphotericin group

# DATASET

# n = 250 all in KAMRC
# uncertain whether we have all DC outcomes
# all EoT outcomes are CURE (or RELAPSE...?)
# 4 relapses identified 
# 0 deaths identified
# 27 patients with history of VL

rm(list = ls())
source("definitions.R")
load_domains("VWPJRM")

# every domain contains every USUBJID
# additional domains: HO, QS
mg <- ld_missingness("VWPJRM")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----

# all USUBJID unique
DM %>% count_dup2()
DM %>% names() # no death data
DM %>% count(SEX)
DM %>% count(ARM)

VWPJRM <- DM %>%
  rename(
    DM_SEX = SEX,
    DM_SITE = SITEID,
    DM_AGE = AGE,
    DM_ARM = ARMCD
  ) %>%
  select(starts_with("DM_"), USUBJID, RFSTDTC)

# RS ----

RS %>% count()
RS %>% names()

RS %>% count(VISIT, RSCAT, RSSCAT, RSTESTCD)
RS %>% count_dup(VISIT, RSCAT, RSSCAT, RSTESTCD, RSORRES)

# DS ----
# VISIT data here does not make sense
# 4 RELAPSE cases are VISIT = "Day 22", but DSDY are 15, 57, 207, 152.
# Let's assume these are the relapse cases

DS %>% count()
DS %>% names()
DS %>% count(VISIT, DSTERM)


# OUT ---
# Need to check with Gemma / Sean, if the 'Day 22' and 'Day 30' DSTERMs for CURE are actually final 6 month DC outcomes? 
# On the face of it, from -DY data, it appears DSTERM corresponds to same date as initial cure (22 and 30 day) outcomes. 
# For this reason, we keep DS_OUT = FALSE for all. 
#

OUT <- RS %>% 
 mutate(RSSCAT = ifelse(RSSCAT == "MEDICAL HISTORY", "MH", NA)) %>%
   mutate(
     VISIT = case_when(
       VISIT == "Day 22" ~ "D22",
       VISIT == "Day 30" ~ "D30",
       VISIT == "Screening" ~ "D0",
       .default = NA
     )
   ) %>%
   pivot_wider(
     id_cols = USUBJID,
     names_from = c(VISIT, RSSCAT, RSTESTCD),
     values_from = c(RSORRES, RSDY),
     names_glue = "RS_{VISIT}_{RSSCAT}_{RSTESTCD}_{.value}",
   ) %>% full_join(DS %>% select(USUBJID, DSTERM, DSDY)) %>% full_join(DM %>% select(ARMCD, USUBJID))

OUT %>% names()	
OUT %>% count(ARMCD, RS_D22_NA_TOC_RSORRES, RS_D22_NA_TOC_RSDY, RS_D30_NA_TOC_RSORRES, RS_D30_NA_TOC_RSDY, DSTERM, DSDY) #%>% View()
OUT #%>% View()
RS #%>% View()

OUT_merge <- OUT %>% 
  mutate(
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC = TRUE,  # need to check this is the case
    OUT_NA = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_DC = FALSE, # either relapse or no final outcome specified
    OUT_DC_RELAPSE = ifelse(DSTERM == "RELAPSE", TRUE, FALSE),
    OUT_DC_OTHER = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_DC_DEATH = FALSE,
    RS_MH_VL = ifelse(RS_D0_MH_OVRLRESP_RSORRES == "RELAPSE", TRUE, NA)
  ) %>% 
  select(USUBJID, RS_MH_VL, starts_with("OUT_"))

VWPJRM <- VWPJRM %>% full_join(OUT_merge)
VWPJRM %>% names()

# SA ----
SA %>% names()
SA %>% count()
SA %>% count(VISIT, SATERM, SACAT, SAPATT, !is.na(SADUR))  %>% print(n = Inf)

# fever and rigor data during first 15 days is presented with a duration (e.g. PT180.0M = 180 minutes)
# for purposes of data extraction, only extract the medical history 
SA #%>% View()

# 6 of these can be recovered from SAEVINTX 
SA %>% filter(VISIT == "Screening" & is.na(SADUR)) # %>% View()

SA_merge <- SA %>% 
  filter(VISIT == "Screening") %>% 
  mutate(SA_HX_FEV_DUR = SADUR)  %>% 
  select(USUBJID, SA_HX_FEV_DUR) %>% full_join(VWPJRM) 

VWPJRM <- VWPJRM %>% full_join(SA_merge)
VWPJRM %>% names()

# LB ----
LB %>% dim()
LB %>% names()
LB %>% count_dup2(VISIT, LBTESTCD, LBSPEC) %>% print(n = Inf)

LB_merge <- LB %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 30" ~ "IC",
      .default = "delete"
    )
  ) %>% 
  filter(VISIT %in% c("BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_merge #%>% View()

LB_TEST <- DM %>% select(USUBJID, ARMCD) %>% full_join(LB %>% select(USUBJID, VISIT, LBDY))
LB_TEST %>% count(VISIT, ARMCD, LBDY) %>% print(n = Inf)

VWPJRM <- VWPJRM %>% full_join(LB_merge)

# MB ----
MB %>% names()
MB %>% count_dup(VISIT, MBTESTCD, MBLOC, MBORRES)

MB_merge <- MB %>% 
  filter(MBTESTCD != "HIV") %>% 
  mutate(
    MB_BL_HIV = FALSE,
    MB_BL_LSHMANIA_SPLEEN = MBORRES
  ) %>% 
  select(USUBJID, starts_with("MB_"))

MB_merge# %>% View()

VWPJRM <- VWPJRM %>% full_join(MB_merge)

# MP ----
MP %>% count()
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPTEST, MPLOC)
MP #%>% View()

#MP %>% ggplot() + geom_histogram(aes(x = MPORRES))

# MPDY seem to correlate with VISITDY
MP %>% arrange(VISIT, MPDY) #%>% View()
MP %>% count(VISIT) 

MP_merge <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 30" ~ "IC",
      .default = "remove"
    )
  ) %>% 
  filter(VISIT %in% c("BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC, MPTESTCD),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_{MPTESTCD}"  
  ) 

MP_merge # %>% View()
MP_merge %>% names()

VWPJRM <- VWPJRM %>% full_join(MP_merge)

# VS ----
VS %>% names()
VS %>% count(VISIT, VSCAT, VSTESTCD) %>% print(n = Inf)

# 1 USUBJID has 2 TEMP 1 Month entries
# 18 USUBJIDs have 2 TEMP Day 15 entries
# 25 USUBJIDs have 25 TEMP Day 8 entries
# 38 USUBJIDs have 2 TEMP Day 0 entries
VS %>% 
  filter(!is.na(VSSTRESN)) %>% count_dup2(VISIT, VSCAT, VSTESTCD, VSSTRF) %>% print(n = Inf)

# what's the difference between screening and D0 temperature? Gemma
# what's the difference between Day 30 and 1 month temp & weight? Ans: I think 1 month is 1 month after treatment completed
VS_explore <- VS %>% 
  group_by(USUBJID, VISIT, VSTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  mutate(VISIT = str_c("VS_", str_replace(VISIT, " ", ""))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD, n_dup),
    values_from = VSSTRESN
  )

#VS_explore %>% names()
#VS_explore %>% ungroup() %>% mutate(diff = VS_Day30_WEIGHT_1 - VS_Day0_WEIGHT_1) %>% count(diff)

VS %>% group_by(VISIT, VSTESTCD) %>% 
  summarise(
    min = quantile(VSDY, probs = 0, na.rm = TRUE),
    lq = quantile(VSDY, probs = 0.25, na.rm = TRUE),
    median = quantile(VSDY, probs = 0.5, na.rm = TRUE),
    uq = quantile(VSDY, probs = 0.75, na.rm = TRUE),
    max = quantile(VSDY, probs = 1, na.rm = TRUE)
  ) #%>% View()

VS_explore %>% names()
VS_explore %>% select(VS_Day0_TEMP_1, VS_Day0_TEMP_2, VS_Screening_TEMP_1) %>% print(n = Inf)
VS_explore %>% select(VS_Day0_TEMP_1, VS_Day0_TEMP_2, VS_Screening_TEMP_1) %>% print(n = Inf)
VS_explore %>% select(VS_Day30_TEMP_1, VS_1Month_TEMP_1, VS_1Month_TEMP_2) %>% print(n = Inf)

VS_merge <- VS_explore %>% 
  rename(
    VS_BL_HEIGHT = VS_Day0_HEIGHT_1,
    VS_BL_TEMP = VS_Day0_TEMP_1,
    VS_BL_WEIGHT = VS_Day0_WEIGHT_1,
    VS_IC_TEMP = VS_Day30_TEMP_1,
    VS_IC_WEIGHT = VS_Day30_WEIGHT_1
  ) %>% 
  select(USUBJID, starts_with("VS_BL") | starts_with("VS_IC"))

VWPJRM <- VWPJRM %>% full_join(VS_merge)
VWPJRM %>% names()

# Other domains ----

# dates of hospitalisation
HO #%>% View()

# KPSS
QS %>% count(VISIT, QSTESTCD)

# SAVE ----
save(VWPJRM, file = "Study/Data/VWPJRM.RData")