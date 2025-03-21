## EXCLUDE as unable to identify relapses

# PUBLICATION

# Das VN, Siddiqui NA, Pandey K, Singh VP, Topno RK, Singh D, Verma RB, Ranjan A, Sinha PK, Das P. 
# A controlled, randomized nonblinded clinical trial to assess the efficacy of amphotericin B deoxycholate as 
# compared to pentamidine for the treatment of antimony unresponsive visceral leishmaniasis cases in Bihar, India. 
# Ther Clin Risk Manag. 2009 Feb;5(1):117-24. Epub 2009 Mar 26. PMID: 19436614; PMCID: PMC2697519.
# https://doi.org/10.2147/TCRM.S3581

# 9 patients did not achieve initial cure (2 in AMB, 7 in PENT)
# 5 patients relapsed (1 in AMB, 4 in PENT)
# 0 deaths
# No LTFU

# DATABASE

# n = 82
# see comments for outcome

rm(list = ls())
source("definitions.R")
load_domains("VLNXMEA")

mg <- ld_missingness("VLNXMEA")
mg %>%
  count_na() %>%
  print(width = Inf)


# DM ----
# No DMDY or RFSTDTC
DM %>% names()
DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = Inf)
DM %>% count(SITEID)
DM %>% count(ARMCD)

DM_merge <- DM %>% 
  mutate(
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  select(starts_with("DM_"), USUBJID)

VLNXMEA <- DM_merge

# OUT ----

# # Clarify with Gemma and Sean: 
# Publication describes 5 relapses (4 in pentamidine group and 1 in amphotericin deoxycholate group)
# RS describes 5 patients not cured at 6 months, but labelled as 'apparent cure' at EOT (which would fit with the relapses): 
# # Control_10, Control_23, Control_32, Control_40, Study_40

# However RS *also* describes 5 patients as achieving both initial cure (apparent cure) and definitive cure (ultimate cure), but with an additional outcome of 'Relapse' occurring 'AFTER' treatment:
# # Control_2, Control_18, Control_33, Control_34, Study_34

# Can we confirm which group of 5 are the true relapses? Or more likely to be the true relapses? If uncertain will, may need to exclude this study

DS %>% names()
DS %>% count(VISIT, DSTERM)
DS %>% count_dup()

RS %>% names()
RS %>% count(VISIT, RSSCAT, RSORRES)
RS %>% count_dup(VISIT)

RS_merge <- RS %>% 
  mutate(
    VISIT = case_when(
      VISIT == "6 Months" & !is.na(VISIT) ~ "DC",
      VISIT == "EOT" & !is.na(VISIT) ~ "IC",
      is.na(VISIT) ~ "ADD"
    )
  ) %>% pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = RSORRES
  ) 

RS_merge #%>% View()

OUT_merge <- DS %>% select(USUBJID, DSTERM) %>% full_join(RS_merge) %>% full_join(VLNXMEA)
OUT_merge #%>% View()

OUT <- OUT_merge %>% 
  mutate(
    OUT_NA = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_OTHER = ifelse(IC == "Not Cured", TRUE, FALSE),
    OUTC_IC_OTHER = "These are the 9 patients that did not achieve initial cure (reasons not specified)",
    OUT_IC_DEATH = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC = !OUT_IC_OTHER,
    OUT_DC_RELAPSE = ifelse(!is.na(ADD), TRUE, FALSE), # are these really the 5 relapses?
    OUT_DC_OTHER = ifelse(DC == "Not Cured" & IC == "Apparent Cure", TRUE, FALSE),
    OUTC_DC_OTHER = "These 5 patients have 'Apparent Cure' for initial cure, and 'Not Cured' for definitive cure. But they are different to the 5 relapse patients. Is this a curation error? Are these the relapses?",
    OUT_DC_DEATH = FALSE,
    OUT_DC = ifelse(!OUT_IC | OUT_DC_RELAPSE | OUT_DC_OTHER, FALSE, TRUE)
    ) %>% 
  select(USUBJID, starts_with("OUT"))


VLNXMEA <- VLNXMEA %>% full_join(OUT)

# possible relapse group1
rg1 <- OUT_merge %>% filter(ADD == "Relapse") %>% pull(USUBJID)
rg2 <- OUT_merge %>% filter(DC == "Not Cured" & IC == "Apparent Cure") %>% pull(USUBJID)

# unable to determine which are the true relapses
mg %>% names()
VS %>% filter(USUBJID %in% rg1) #%>% View()
VS %>% filter(USUBJID %in% rg2) #%>% View()

# PT ----
PT %>% names()
PT %>% count(VISIT, PTTRT)

# LB ----
# GEMMA/SEAN, can we confirm when the blood tests labelled as 'after treatment' took place?  
LB %>% names()
LB %>% count(VISIT, VISITNUM, LBTESTCD, LBTPT, LBTPTREF)

# these values match the 'after treatment' lab values - but should confirm
LB %>% full_join(DM %>% select(USUBJID, ARMCD)) %>% group_by(VISIT, LBTESTCD, ARMCD) %>% summarise(mean = mean(LBSTRESN)) #%>% View()

LB_merge <- LB %>% 
  ungroup() %>% 
  mutate(VISIT = ifelse(is.na(VISIT), "IC", "BL")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("LB_"))

LB_merge #%>% View()
VLNXMEA <- VLNXMEA %>% full_join(LB_merge)

# MP ----
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP %>% full_join(DM %>% select(USUBJID, ARMCD)) %>% group_by(VISIT, MPLOC, ARMCD) %>% summarise(mean = mean(MPORRES))

MP_merge <- MP %>% 
  ungroup() %>% 
  mutate(VISIT = ifelse(is.na(VISIT), "IC", "BL")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  ) %>% 
  select(USUBJID, starts_with("MP_"))

VLNXMEA <- VLNXMEA %>% full_join(MP_merge)

# VS ----

# change in weight in the pentamidine group not consistent (in baseline group)
VS %>% names()
VS %>% count(VISIT, VSTESTCD)
VS %>% full_join(DM %>% select(USUBJID, ARMCD)) %>% group_by(VISIT, VSTESTCD) %>% summarise(mean = mean(VSORRES)) #%>% View()

VS_merge <- VS %>% 
  ungroup() %>% 
  mutate(VISIT = ifelse(is.na(VISIT), "IC", "BL")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("VS_"))

VS_merge #%>% View()

VLNXMEA <- VLNXMEA %>% full_join(VS_merge)

VLNXMEA #%>% View()

save(VLNXMEA, file = "Study/Data/VLNXMEA.RData")
