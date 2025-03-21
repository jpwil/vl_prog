##########
# VVNGOE #
##########

# PUBLICATION

# Sundar S, Sinha PK, Verma DK, Kumar N, Alam S, Pandey K, Kumari P, Ravidas V, Chakravarty J, Verma N, Berman J, Ghalib H, Arana B. 
# Ambisome plus miltefosine for Indian patients with kala-azar. Trans R Soc Trop Med Hyg. 
# 2011 Feb;105(2):115-7. doi: 10.1016/j.trstmh.2010.10.008. Epub 2010 Dec 3. PMID: 21129762.

# Very short article - 'short communication' only. 
# of the 135 patients across both RMRIMS and KAMRC: 
# 3 relapses
# 1 death (before initial cure)
# 7 further patients 'not evaluated' (3 not evaluated at initial cure and 4 further patients not evaluated at definitive cure due to various reasons)

# this is the same study listed for VLNAZSK 

# DATASET

# all patients are KAMRC
# 5 relapses - of which one has MB correlation; possibly includes low Hb patients?
# Lab data is not associated with VISIT data - request re-look from data curation team

rm(list = ls())
source("definitions.R")
load_domains("VVNGOE")

mg <- ld_missingness("VVNGOE")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----
DM %>% names()
DM %>% count(SITEID)
DM %>% count(DTHFL) # one death
DM %>% count(ARMCD)
DM %>% count(AGE) %>% print(n = Inf)
DM %>% count(SEX)

DM_merge <- DM %>% 
  mutate(
    DM_AGE = AGE,
    DM_SEX = SEX,
    DM_ARM = ARMCD
  ) %>% 
  select(
    starts_with("DM_"), 
    DM_SITE = SITEID,
    USUBJID,
    RFSTDTC
  )

DM_merge #%>% View()
VVNGOE <- DM_merge

# OUTCOME

DS %>% names()
DS %>% count(VISIT, DSTERM, DSDECOD)
DS %>% count_dup() # present for all 100 USUBJID, all unique, curiously 5 patients listed as relapse (only 3 in study publication)

RS %>% names()
RS %>% count(VISIT, RSTEST, RSTESTCD, RSCAT, RSORRES)
RS %>% count_dup() # present for 98 USUBJID, all unique, 5 relapse again

MB %>% names()
MB %>% count_dup(MBTESTCD, MBTEST, MBLOC, MBTSTDTL, MBMODIFY, MBORRES)
MB %>% count_dup(MBTESTCD, MBTEST, MBLOC, MBORRES, MBDY)

# only one of the relapses is identified in the MB domain data (concurs with DS/RS)
MB %>% filter(is.na(MBLOC) & MBTESTCD != "HIV") %>% select(USUBJID)

DS_merge <- DS %>% select(USUBJID, DSTERM)
RS_merge <- RS %>% select(USUBJID, RSORRES)
DM_merge <- DM %>% select(USUBJID, DTHFL)

OUT <- full_join(DS_merge, RS_merge) %>% full_join(DM_merge)
OUT #%>% View()

OUT_merge <- OUT %>% 
  mutate(
    OUT_NA = is.na(DSTERM),
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_NA = is.na(DSTERM),
    OUT_IC_DEATH = USUBJID == "VVNGOE_MUZAFFARPUR_COMBO-1022",
    OUT_DC_DEATH = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_DC_RELAPSE = DSTERM == "RELAPSE" & !is.na(DSTERM),
    OUT_IC = ifelse(OUT_NA | OUT_IC_DEATH, FALSE, TRUE),
    OUT_DC = ifelse(!OUT_IC | OUT_DC_RELAPSE, FALSE, TRUE)
    ) %>% 
  select(USUBJID, starts_with("OUT"))

OUT_merge %>% count(across(-USUBJID)) %>% mutate(across(everything(), ~ as.character(.x)))# %>% View()
VVNGOE <- VVNGOE %>% full_join(OUT_merge)

VVNGOE #%>% View()

# PT ----
PT %>% names()
PT %>% count(VISIT, PTTRT)

# IN ----
# no relapse history data here
IN %>% names()
IN %>% count(VISIT, INTRT)

# max miltefosine dose is 100mg, 
# IN_merge <- IN %>% select(USUBJID, INTRT, INDOSE)
# VVNGOE <- VVNGOE %>% full_join(IN_merge)

# SA
SA %>% count()
SA %>% names()
SA %>% count(VISIT, SACAT, SAREFID, SATERM, SAOCCUR, SADY, SASTDY, !is.na(SADUR)) %>% print(n = Inf)

SA_merge_fev <- SA %>% 
  filter(VISIT == "Screening" & !is.na(VISIT) & SATERM == "Fever") %>% 
  select(USUBJID, SADUR) %>% 
  rename(SA_HX_FEV_DUR = SADUR)

SA_merge <- SA %>% 
  filter(!is.na(VISIT) & SATERM != "Fever") %>% 
  mutate(
    SAOCCUR = case_when(
      SAOCCUR == "Y" ~ TRUE,
      SAOCCUR == "U" ~ NA,
      .default = FALSE
    ),
    SATERM = case_when(
      SATERM == "Rigor" ~ "RGR",
      SATERM == "Weakness" ~ "WEAK",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR,
    names_glue = "SA_HX_{SATERM}"
  ) %>% full_join(SA_merge_fev)

SA_merge %>% print(n = Inf)
VVNGOE <- VVNGOE %>% full_join(SA_merge)

# LB ----
# no VISIT data; need to wait for VISIT data before we incorperate * GEMMA * - need to just do the best I can
LB %>% names()
LB %>% count(LBTESTCD)
LB %>% count(LBSPEC)
LB %>% count(LBTESTCD, LBDY) %>% print(n = Inf)
LB %>% count_dup(LBTESTCD) %>% arrange(LBTESTCD, count_col) %>% print(n = Inf)
LB %>% filter(LBTESTCD == "HGB") %>% count(LBDY) %>% print(n = Inf)

LB_test <- LB %>% filter(LBTESTCD == "HGB") %>% 
  arrange(USUBJID, LBTESTCD, LBDY) %>% 
  group_by(USUBJID, LBTESTCD) %>% 
  mutate(test_order = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(LBTESTCD, test_order),
    values_from = LBDY,
    names_glue = "LB_{LBTESTCD}_{test_order}"
  )
LB_test #%>% View()

# Can see there aren't many blood tests performed at 30 days
# baseline blood tests are LBDY 1-4 (inclusive)
# So let's use blood tests after treatment (days 15-19) - NO, use days 28-33 for consistency
LB_plot <- LB %>% filter(LBDY < 40) %>% 
  ggplot() + 
  geom_histogram(
    aes(x = LBDY),
    binwidth = 1
  ) + 
  facet_wrap(~LBTESTCD)
#LB_plot

# screening bloods are usually present here
# looks like baseline blood tests are LBDY from -2 to 3 days
# looks like 1 month blood tests are LBDY from 15 to 19 days X no - 28-33 days
LB %>% count(is.na(LBDY))
LB_clean <- LB %>% filter((LBDY >= -2 & LBDY <= 3) | (LBDY >= 28 & LBDY <= 33)) %>% 
  mutate(
    VISIT = ifelse(LBDY >= -2 & LBDY <= 3, "BL", "IC")
  ) %>% 
  group_by(USUBJID, LBTESTCD, VISIT) %>% arrange(LBDY) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n == 1)

LB_clean %>% count(USUBJID, VISIT, LBTESTCD, LBDY)  %>% arrange(USUBJID, VISIT, LBTESTCD, LBDY) # %>% View()
#LB_clean %>% View()  
  
LB_merge1 <- LB_clean %>% 
  filter(LBTESTCD != "HCG") %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(LBTESTCD, VISIT),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_merge2 <- LB_clean %>% 
  filter(LBTESTCD == "HCG") %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(LBTESTCD, VISIT),
    values_from = LBORRES,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% select(USUBJID, LB_BL_HCG) %>% 
  mutate(LB_BL_HCG = ifelse(is.na(LB_BL_HCG), NA, FALSE))

LB_merge <- LB_merge1 %>% full_join(LB_merge2) #%>% View()

VVNGOE <- VVNGOE %>% full_join(LB_merge)

# MB ----
# again no VISIT data 
MB %>% names()
MB %>% count(MBTESTCD, MBLOC, MBORRES, MBDY)

MB_merge <- MB %>% 
  filter(MBTESTCD == "LSHMANIA" & MBDY == 1) %>% 
  select(USUBJID, MBORRES) %>% 
  rename(MB_BL_LSHMANIA_SPLEEN = MBORRES)
MB_merge %>% names()

VVNGOE <- VVNGOE %>% full_join(MB_merge)

# MP ----
# VISIT (e.g. week 3 and week 4) often inconsistent with MPDY)
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)

MP_review <- MP %>% 
  filter(VISIT %in% c("Day 1", "Week 3", "Week 4", "1 Month")) %>%
    pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPDY,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  )  #%>% View()

MP %>% count(MPDY) %>% print(n = Inf)
MP %>% count(VISIT, MPDY) %>% print(n = Inf)

# IC values are MPDY from 28-33 (VISIT data are unreliable for IC)
MP_merge1 <- MP %>% 
  filter(VISIT == "Day 1") %>% 
  mutate(VISIT = "BL") %>% 
  select(USUBJID, VISIT, MPLOC, MPORRES) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
    ) %>% 
  select(USUBJID, starts_with("MP_"))

MP_merge2 <- MP %>% 
  filter(MPDY >= 28 & MPDY <= 33) %>% 
  mutate(VISIT = "IC") %>% 
  select(USUBJID, VISIT, MPLOC, MPORRES) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
    ) %>% 
  select(USUBJID, starts_with("MP_"))

# MP_merge <- MP %>% 
#   filter(VISIT %in% c("1 Month", "Day 1")) %>% 
#   mutate(VISIT = ifelse(VISIT == "Day 1", "BL", "IC")) %>% 
#   select(USUBJID, VISIT, MPLOC, MPORRES) %>% 
#   pivot_wider(
#     id_cols = USUBJID,
#     names_from = c(VISIT, MPLOC),
#     values_from = MPORRES,
#     names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
#   ) %>% 
#   select(USUBJID, starts_with("MP_"))
# MP_merge #%>% View()

VVNGOE <- VVNGOE %>% full_join(MP_merge1) %>% full_join(MP_merge2)

# VS
# IC VISIT not consistent! Like MP. 
VS %>% names()
VS %>% count(VISIT, VSTESTCD, VSREFID) %>% print(n = Inf)

#VS_merge %>% count(VISIT, VSTESTCD, VSREFID, VSSTRESN) %>% print(n = Inf)

VS_merge1 <- VS %>% 
  filter(VISIT == "Day 1") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSSTRESN,
    names_glue = c("VS_BL_{VSTESTCD}")
  ) %>% 
  select(USUBJID, starts_with("VS_"))

VS_merge2 <- VS %>% 
  filter(VSDY >= 28 & VSDY <= 33) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VSTESTCD,
    values_from = VSSTRESN,
    names_glue = c("VS_IC_{VSTESTCD}")
  ) %>% 
  select(USUBJID, starts_with("VS_"))

# VS_merge <- VS_merge %>%
#   mutate(VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>% 
#   pivot_wider(
#     id_cols = USUBJID,
#     names_from = c(VISIT, VSTESTCD),
#     values_from = VSSTRESN,
#     names_glue = c("VS_{VISIT}_{VSTESTCD}")
#   ) %>% 
#   select(USUBJID, starts_with("VS_"))

VVNGOE <- VVNGOE %>% full_join(VS_merge1) %>% full_join(VS_merge2)

# RP ----

RP %>% names()
RP %>% count(RPTESTCD, RPDY, RPORRES)
RP_merge <- RP %>% 
  mutate(RP_PREG = FALSE) %>% 
  select(USUBJID, RP_PREG)

VVNGOE <- VVNGOE %>% full_join(RP_merge)

# HO ---- 
HO # %>% View()

# QS ----
QS # KPSS

save(VVNGOE, file = "Study/Data/VVNGOE.RData")
