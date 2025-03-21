###########
# VLZUKHR #
###########

## Monday 18th March 2024

# Pandey K, Ravidas V, Siddiqui NA, Sinha SK, Verma RB, Singh TP, Dhariwal AC, Das Gupta RK, Das P. 
# Pharmacovigilance of Miltefosine in Treatment of Visceral Leishmaniasis in Endemic Areas of Bihar, India. 
# Am J Trop Med Hyg. 2016 Nov 2;95(5):1100-1105. doi: 10.4269/ajtmh.16-0242. Epub 2016 Sep 19. PMID: 27645786; PMCID: PMC5094224.

# 646 patients, with clear flow of patients through study
# Should be a total of 45 relapses within 6 months
# We know there were 11 deaths in total; 5 occurred from BL-IC, and 6 occurred from IC-DC
# Study identifies 37 defaulters (only 3 identified in dataset) - 12 defaults from BL-IC and 25 defaulters from IC-DC

# from the dataset:
# Compared to VSGPDL this is much cleaner dataset
# Age, sex and outcomes correlate with publication, but spleen/liver sizes do not
# Laboratory data is missing for the majority of patients - need to investigate further
# Spleen, liver, and weight data is available for dead patients(!?)
# Weight data - very small changes between day 0 and day 29.

# Questions for Gemma/Sean
# 1) only have blood tests for 234 patients (/646) - do we know why?
# has this been challenged?

rm(list = ls())
source("definitions.R")
load_domains("VLZUKHR")

mg <- ld_missingness("VLZUKHR")
mg # %>% View_lt()

# mg %>% 
#   mod_lt() %>% 
#   upset(
#     nsets = 15, 
#     nintersects = 50, 
#     order.by = c("freq"), 
#     set_size.show = TRUE,
#     text.scale = 2
#   )

# DM Domain Summary:
# Median age: 23.5 years (IQR 12-39.8 years), range 6-70 years
# Sex: 375 male (58.0%)

# DM ----

# all 646 patients are unique
colnames(DM)
DM %>%
  count(USUBJID) %>%
  count(n)

# 5 different sites
DM %>% count(SITEID)
DM %>% count(ARM)
DM %>% count(ARMCD)
DM %>% count(COUNTRY)
DM %>% count(DMDY)
DM %>% count(RFSTDTC)

# age and sex matches that reported in study
DM %>% var_sum(AGE)
DM %>%
  mutate(AGE_GROUP = cut(AGE, breaks = c(0, 13, 24, 34, 44, 54, 100))) %>%
  arrange(desc(AGE)) %>%
  count(AGE_GROUP)
DM %>% count(SEX)

#DM %>% var_sum(RFSTDTC)

# death - 11 deaths recorded, one with a date (died during treatment)
# 11 deaths corresponds with the publication findings
DM %>% count(DTHDTC, DTHFL)
DM %>%
  filter(DTHFL == "Y") %>%
  count(RFSTDTC) 

DM %>% colnames()

VLZUKHR <- DM %>% 
  mutate(
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID,
    DM_ARM = ARMCD
  ) %>% 
  select(USUBJID, RFSTDTC, starts_with("DM_"))

######

# TV ----
# each dose is assigned a visit

######

# TI ----

# taken from the published study
TI #  %>% View()
######

# RS domain
# one row per USUBJID (all 6 month outcomes)
# all VL outcomes
# all 6 month outcomes including relapse and cure
# deaths and defaulters not included

# RS ----
RS %>% colnames()

# each RS row corresponds to a different USUBJID
# all 6 month outcomes
RS %>%
  count(USUBJID) %>%
  count(n)
RS %>% count(RSTESTCD, RSTEST, RSCAT, VISIT)
RS %>% count(RSTESTCD, RSDTC, RSDY)

# there are 13 USUBJIDs with RSDTC/RSDTY populated (all RELAPSE)
RS %>% count(!is.na(RSDTC), !is.na(RSDY))
RS %>% count(RSORRES, RSSTRESC, RSDY)

# 45 relapses, 553 cures
# these correspond to all relapses and 6 months cures according to publication
RS %>% count(RSORRES, RSSTRESC)

# OUTCOMES ----

DM %>% select(DTHFL, DTHDTC)

RS %>% names()
DS %>% names()
RS %>% count(VISIT, RSTESTCD, RSCAT, RSORRES, RSDY)

DS %>% count(VISIT, DSTERM, DSSTDY)
DS %>% filter(DSTERM == "Defaulter") #%>% View()

# join outcomes and consider together
OUTCOME <- RS %>% select(USUBJID, RSORRES, RSDY) %>% 
  full_join(DM %>% select(USUBJID, DTHFL, DTHDTC)) %>% 
  full_join(DS %>% select(USUBJID, DSTERM, DSSTDY)) %>% 
  arrange(RSORRES, RSDY, DSTERM, DSSTDY) 
OUTCOME %>% count(RSORRES, RSDY, DSTERM, DTHDTC, DSSTDY)
OUTCOME %>% dim()

# create outcomes according to dictionary
OUTCOME_merge <- OUTCOME %>% 
  mutate(
    OUT_IC = ifelse(RSORRES %in% c("Complete", "Relapse", "Relapse within 6 months"), TRUE, FALSE),              # must have achieved IC if achieved DC
    OUT_IC_DEATH = ifelse(DSTERM == "Death" & !is.na(DTHDTC), TRUE, FALSE),
    OUT_XX_DEATH = ifelse(DSTERM == "Death" & (is.na(DTHDTC)), TRUE, FALSE),
    OUT_DC_DEATH = FALSE,
    OUT_DC_RELAPSE = ifelse(RSORRES %in% c("Relapse", "Relapse within 6 months"), TRUE, FALSE),
    OUT_IC_OTHER = FALSE, # we know there are 37 defaulters from the study (12 pre-IC and 25 post-IC); but only have timing for 2 x defaulters at 90 and 102 days
    OUT_DC_OTHER = ifelse(DSTERM == "Defaulter" & !is.na(DSSTDY), TRUE, FALSE), # these are DEFAULTERS
    OUTC_DC_OTHER = "These are the two defaulters where DSSTDY (default day) is explicity stated (90 and 102 days)",
    OUTC_XX_OTHER = "These are the remaining defaulters (n = 35) with no timing information", 
    OUT_XX_OTHER = ifelse(DSTERM == "Defaulter" & is.na(DSSTDY), TRUE, FALSE),
    OUT_IC_DRUG = FALSE,
    OUT_NA = FALSE
  )  %>% 
  mutate(
    OUT_IC = ifelse(DSTERM == "Defaulter" & !is.na(DSSTDY), TRUE, OUT_IC),  # update OUT_IC: let's make the assumption that patients who defaulted during IC-DC achieved IC. 
    OUT_DC = case_when(
      RSORRES == "Complete" ~ TRUE,
      DSTERM %in% c("Relapse", "Relapse within 6 months", "Death") ~ FALSE,
      DSTERM == "Defaulter" ~ FALSE,
      .default = TRUE),
    
    #OUT_IC_DEATH_DTC = ifelse(!is.na(DTHDTC), "30/04/2013", NA)
  )
  
OUTCOME_merge %>% count(across(contains("OUT_"))) %>% arrange(n) %>% print(width = Inf) # happy with this :-)

VLZUKHR <- VLZUKHR %>% full_join(OUTCOME_merge %>% select(USUBJID, contains("OUT_") | contains("OUTC_")))
VLZUKHR %>% count(across(starts_with("OUT_"))) %>% print(width = Inf)



# from the article we know that 

######

# PT: each dose has one entry
# Two different dosing regimens (50mg OD vs 50mg BD)

# PT ----

PT %>%
  count(USUBJID) %>%
  count(n)
PT %>% count(PTDOSRGM)

######

# IN: only two rows, documenting two anti-vomiting drugs received by one patient

# IN ----
colnames(IN)
IN # %>% View()

# SA ----
# SA domain: I don't think we can use these data
# This domain documents side effects experienced during the study (probably) - very few baseline measurements
# The publication does not present baseline clinical signs / symptoms

# there are no VISIT data (i.e. no scheduled readings)
SA %>% colnames()

# this is a pharmacovigilence study, hence SA domain is busy

SA %>%
  count(SATERM) %>% print(n = Inf)
SA %>%
  count(USUBJID, SADY, SATERM) %>%
  count(SATERM, SADY, n) %>%
  print(n = 100)

SA %>% count(SASEV, SAACN, SAOUT, SACONTRT)
SA %>%
  count(SASEV, SAACN, SAOUT, SADY, SASTDY, SAENDY, SAENRF) %>%
  print(n = Inf)

######

# LB ----

LB %>% colnames()
LB %>% 
  count(VISIT, LBTESTCD, LBTEST)  %>% print(n = Inf)

# LBDY/LBDTC all co-defined; LBDY calculated correctly, but only sometimes
# where present, EPOCH/VISIT data is also present
# there are 42 patients with no LB data at all

LB %>%
  count(LBDY_present = !is.na(LBDY), VISIT, USUBJID) %>%
  count(LBDY_present, VISIT, n) %>%
  print(n = Inf)

# 220 (220/473) patients at Motihari have blood tests
# 14 (14/54) patients at Paroo have blood tests
LB %>%
  rename(LB_DOMAIN = DOMAIN) %>%
  mutate(LB_LBDY_PRESENT = is.na(LBDY)) %>%
  full_join(DM) %>%
  select(USUBJID, SITEID, LB_DOMAIN, DOMAIN) %>%
  distinct() %>%
  count(SITEID, LB_DOMAIN, DOMAIN)

# 234 patients with blood tests
LB %>%
  distinct(USUBJID) %>%
  dim()

# day 0 available for: Hb/leuks/lymph/Plt/AST/ALT/Neuts ( ~ 200 patients), and Eosinophils (~135 patients)
# day 29 available for: Hb/leuks/lymph/Plt/AST/ALT/neuts (~ 200 patients), and Eosinophils (~180 patients)
# also have roughly the same numbers for day 14, 2 months and 6 months
# very few bilirubin and creatinine data 
LB %>% 
  group_by(VISIT, LBTESTCD, USUBJID) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, LBTESTCD, n_dup) # %>% View()

# pivot_wider
LB_merge <- LB %>%
  filter(
    VISIT %in% c("Day 0", "Day 29")
  ) %>%
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = NA) 
  ) %>% 
  filter(VISIT %in% c("BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_merge %>% colnames()
LB_merge %>% 
  summarise(
    across(
      everything(),
      ~ sum(!is.na(.))
    )
  ) %>% 
  select(sort(names(.))) #%>% View()

# LB TIMING

# only 13 patients from Paroo have LBDY data for the blood tests
# these are not helpful 
LB %>% names()
LB %>% count(VISIT, VISITDY, EPOCH, LBDY, !is.na(LBDTC))
LB_timing <- LB %>% 
  filter(!is.na(LBDY), VISIT != "Day 14") %>% 
  mutate(VISIT = if_else(VISIT == "Day 0", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = c(LBSTRESC, LBDY),
    names_glue = "LB_{VISIT}_{LBTESTCD}_{.value}"
  )
LB_timing %>% select(contains("WBC")) 
LB_timing %>% select(contains("HGB")) 
LB_timing %>% select(contains("ALT")) 
LB_timing %>% select(contains("PLAT")) 
LB_timing %>% select(contains("CREAT")) 

#mutate(DY_DIFF = LB_DA)

VLZUKHR <- VLZUKHR %>% 
  full_join(LB_merge)

# MB ---- 
# domain not populated for this study

# MP ----

# MP domain - each patient has day 0,14 and 29 spleen and liver measurements
# patients who died during treatment have day 29 measurements
# summary means across sexes do not correspond with published results

# every patient (x 646) has 3 x spleen and liver measurements (day 0, day 14, day 29)
MP %>%
  count(MPTEST, MPLOC, VISIT, USUBJID) %>%
  count(MPTEST, MPLOC, VISIT, n)
MP %>%
  pull(USUBJID) %>%
  unique() %>%
  length()

# there is no missing data - interesting given patients died before end of treatment
MP %>%
  count(VISIT, MPLOC, MPORRES, MPORRESU) %>%
  print(n = Inf)
MP %>% count(VISIT, MPLOC)

# for some reason, change in liver and spleen sizes do not correlate with those in the publication
# will likely need to discard MP values
MP_merge <- MP %>% 
  filter(VISIT %in% c("Day 0", "Day 29")) %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = "VISIT"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  ) 

MP_merge2 <- MP %>% 
    mutate(VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = VISIT
    )
     ) %>% 
   pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  ) 

test <- MP_merge2 %>% mutate(
  diff1 = `MP_Day 14_SPLEEN_LENGTH` - MP_BL_SPLEEN_LENGTH,
  diff2 = `MP_IC_SPLEEN_LENGTH` - `MP_Day 14_SPLEEN_LENGTH`,
  diff3 = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH,
  diff4 = `MP_Day 14_LIVER_LENGTH` - MP_BL_LIVER_LENGTH,
  diff5 = `MP_IC_LIVER_LENGTH` - `MP_Day 14_LIVER_LENGTH`,
  diff6 = MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH,
) 

test %>% count(diff3)
test %>% count(diff2)
test %>% count(diff6)

MP_merge %>% names()
MP_merge %>% ungroup() %>% mutate(diff = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) %>% count(diff)
MP_merge %>% ungroup() %>% mutate(diff = MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) %>% count(diff)

# no missing data!
MP_merge %>% make_log() %>% count_na()

VLZUKHR <- VLZUKHR %>% 
  full_join(MP_merge)

# MP TIMING
# no MPDY!
MP %>% names()


# VS ----
# Vital signs domain

VS %>%
  count(VISIT, VSTESTCD, USUBJID) %>%
  count(VISIT, VSTESTCD, n)
VS %>%
  group_by(VISIT, VSTESTCD) %>%
  summarise(
    n = n(),
    n_distinct = n_distinct(USUBJID),
    n_missing = sum(is.na(VSSTRESN)),
    median = median(VSSTRESN),
    mean = mean(VSSTRESN),
    min = min(VSSTRESN),
    max = max(VSSTRESN)
  )

VS_merge <- 
  VS %>% 
  mutate(
    VISIT = case_when(
      VISIT %in% c("Day 0", "Screening") ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = "error"
    )
  ) %>% 
  filter(VISIT %in% c("BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  )

VS_merge %>% names()
VS_merge %>% ungroup() %>% mutate(diff = VS_IC_WEIGHT - VS_BL_WEIGHT) %>% arrange(diff)# %>% View()
VS_merge %>% make_log() %>% count_na() # just one patient missing IC weight

VLZUKHR <- VLZUKHR %>% 
  full_join(VS_merge)

# VS TIMING
# NO VSDY
VS %>% names()

VLZUKHR %>% 
  save(VLZUKHR, file = "Study/Data/VLZUKHR.RData")

VLZUKHR %>% make_log() %>% 
  summarise(across(everything(), ~sum(.x == TRUE))) #%>% View()
