##########
# VSGPDL #
##########

# Sundar S, Singh A, Agrawal N, Chakravarty J. Effectiveness of Single-Dose Liposomal Amphotericin B in Visceral Leishmaniasis in Bihar. 
# Am J Trop Med Hyg. 2019 Oct;101(4):795-798. doi: 10.4269/ajtmh.19-0179. PMID: 31436156; PMCID: PMC6779195.

# From the article (Sundar et al. 2019) #
# This is a relatively large single arm single dose LAMB study from KAMRC
# Published in 2019 and conducted from 2013 - 2017
# Recruited 1143 patients, of whom 1088 have day 30 data (initial cure)
# From the article and protocol: no parasitological test of cure performed
# Relapse outcomes are (should be) available at 6 months and 12 months (active follow-up)
# No LTFU described beyond 30 days (denominators reported in study consistent with no LTFU) 

# From the dataset #
# Contains both PKDL and VL patients
# Contains 10 patients with two treatment records (i.e. they were re-recruited into the same study)
# For these 10 patients, the second episode is (usually) __STRTPT == "AFTER"
# Contains 1027 unique patients in DM domain
# Contains (at least) 42 patients with PKDL (when looking at first episodes only, all correspond to a multidrug LAMB regimen)

# OUTCOMES (RS and DC):
# Of VL patients with 30 day initial cure:
#   573 CURE
#   33 RELAPSE
#   1 DIED
#   360 MISSING

# this dataset only reports 6 month outcomes, not 12 month outcomes
# for analysis - EXCLUDE duplicate records and PKDL records

rm(list = ls())
source("definitions.R")
load_domains("VSGPDL")

mg <- ld_missingness("VSGPDL")
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

# RS ----
colnames(RS)

# RS Domain Summary:
# 14 patients not matching with DM patients (all the 12 DM patients with missing data, and a further 2)
# 1013 unique patients are described
# 10 patients have duplicates (2 in BASELINE, 10 in 30 DAYS, 7 in 6 MONTHS)
## all of the duplicates have some RS FINDINGS with 'AFTER' in RSSTRTPT (even medical hx)
# Questions for Gemma/Sean:
## Significance of PKDL?
## 10 x Duplicates; all including 'AFTER', and majority of which have BASELINE RELAPSE
## One of the duplicates (JDF-0971) has conflicting 6 MONTH RELAPSE status
# no death data in RS

# within visit and RSSTRTPT, all USUBJIDs are unique
RS %>%
  group_by(VISIT, RSSTRTPT, USUBJID) %>%
  count(USUBJID) %>%
  ungroup() %>%
  count(n)
RS %>%
  group_by(VISIT) %>%
  count(USUBJID) %>%
  count(n)

# label duplicates
DUP <- RS %>%
  group_by(VISIT, USUBJID) %>%
  mutate(DUPLICATE = any(RSSTRTPT == "AFTER")) %>%
  ungroup() %>%
  filter(DUPLICATE == TRUE) %>%
  pull(USUBJID) %>%
  unique()

# label PKDL
PKDL <- RS %>%
  filter(RSCAT == "PKDL") %>%
  pull(USUBJID) %>%
  unique()
intersect(PKDL, DUP)

# create list of USUBJIDs for removing
REMOVE <- c(PKDL, DUP) %>% unique()
REMOVE # corresponding to 50 USUBJIDs

# remove duplicates and PKDL patients
RS_clean <- RS %>%
  filter(!USUBJID %in% REMOVE)
RS_clean %>% dim()

# review how many patients in RS domain match with DM:
# there are 14 patients in DM that do not match with patients in RS
# 12 of these patients are the DM patients with missing sex/age/RFSTDTC
# the remaining 2 are: JDF-0672 and JDF-0781

#test <- left_join(DM_clean, RS_clean, by = join_by(USUBJID == USUBJID))
#test %>% count(STUDYID.x, STUDYID.y)
#test %>% filter(is.na(STUDYID.y))

# PIVOT to inspect RS contents
# can confirm all USUBJIDs are unique within RSTESTCD, RSSCAT, VISIT
RS_clean %>%
  group_by(RSTESTCD, RSSCAT, VISIT, USUBJID) %>%
  mutate(n_dup = row_number()) %>%
  ungroup() %>%
  count(n_dup)
RS_clean %>%
  count(RSTESTCD, RSSCAT, VISIT)

RS_clean %>%
  count(RSTESTCD, RSSCAT, RSORRES, VISIT)

RS_clean_wide <- RS %>%
  filter(!(USUBJID %in% REMOVE)) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(RSTESTCD, RSSCAT, VISIT),
    values_from = RSORRES
  )
RS_clean_wide %>% colnames()
RS_clean_wide %>%
  count(`OVRLRESP_MEDICAL HISTORY_Day 0`, `TOC_NA_Day 30`, `OVRLRESP_ADDITIONAL OUTCOMES PROVIDED_NA`, `OVRLRESP_NA_6 Months`) # %>% View()

# cross-match RS_clean_wide with DS data to confirm outcome assignments
DS %>% names()
DS %>% 
  filter(!USUBJID %in% REMOVE) %>% count(USUBJID) %>% count(n)
DS_clean <- DS %>% filter(!USUBJID %in% REMOVE)
DS_clean %>% count(VISIT, DSTERM, DSDECOD)

RS_clean_wide <- DS_clean %>% select(USUBJID, DSTERM) %>% full_join(RS_clean_wide) %>% full_join(DM %>% filter(!USUBJID %in% REMOVE) %>% select(USUBJID, RFSTDTC, DTHFL)) 
RS_clean_wide %>% count(`TOC_NA_Day 30`, `OVRLRESP_ADDITIONAL OUTCOMES PROVIDED_NA`, `OVRLRESP_NA_6 Months`, DSTERM, DTHFL)  # %>% View()

VSGPDL <- RS_clean_wide %>%
  mutate(
    RS_MH_VL = case_when(
      `OVRLRESP_MEDICAL HISTORY_Day 0` == "RELAPSED" ~ TRUE,
      .default = NA
    ),
    OUT_IC_DRUG = case_when(
      `TOC_NA_Day 30` %in% c("DRUG STOP", "HYPERSENSIVIVITY FROM AMBISOME") & !is.na(`TOC_NA_Day 30`) ~ TRUE,
      `OVRLRESP_NA_6 Months` == "HYPERSENSITIVITY FROM AMBISOME" & !is.na(`OVRLRESP_NA_6 Months`) ~ TRUE,
      .default = FALSE
    ),
    OUTC_IC_DRUG = "For 3 patients, 6 month outcomes (RS & DS) are related to drug hypersensitivity/sensitivity, or these 2 have RS 30 day outcomes related to drug hypersensitivity (Ambisome), the other 1 patient has COMPLETE RECOVERY as RS 30 day outcome. A further 3 patients have RS 30 day outcome as hypersensitivity, and missing data for 6 month outcomes. All 6 patients are therefore OUT_IC_DRUG = TRUE and OUT_IC = FALSE.",
    OUT_DC = case_when(
      `OVRLRESP_NA_6 Months` == "CURE" ~ TRUE,
      `OVRLRESP_NA_6 Months` == "RELAPSE" | `OVRLRESP_ADDITIONAL OUTCOMES PROVIDED_NA` == "RELAPSE" ~ FALSE,
      .default = FALSE
    ),
    OUT_DC_RELAPSE = case_when(
      `OVRLRESP_NA_6 Months` == "RELAPSE" | `OVRLRESP_ADDITIONAL OUTCOMES PROVIDED_NA` == "RELAPSE" ~ TRUE,
      .default = FALSE
    ),
    OUT_NA = ifelse(is.na(`TOC_NA_Day 30`), TRUE, FALSE), # TRUE or FALSE
    OUT_IC = !(OUT_IC_DRUG | OUT_NA),
    OUT_IC_OTHER = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_DC_DEATH = ifelse(
      DSTERM == "PATIENT EXPIRED (AS HOME DATED 19-08-2015)" & !is.na(DSTERM), TRUE, FALSE
    )) %>% 
    select(USUBJID, starts_with("OUT"))

VSGPDL %>% names()
VSGPDL %>% count(across(-c(USUBJID, OUT_IC_DEATH)))  %>% print(width = 100)
VSGPDL %>% count(across(-c(USUBJID)))  %>% print(width = 100)


# Duplicates summary (1st/2nd) [removed from analysis]
## VSGPDL_MUZAFFARPUR_JDF-0135: VL/PKDL, RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: cure in both episodes
## VSGPDL_MUZAFFARPUR_JDF-0715: VL/PKDL, RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: both cure
## VSGPDL_MUZAFFARPUR_JDF-0741: VL/PKDL, no RELAPSE history,          both Day 30 COMPLETE RECOVERY, 6 month: cure for PKDL only
## VSGPDL_MUZAFFARPUR_JDF-0752: VL/PKDL, RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: missing for 1st episode, cure in second (PKDL) episode
## VSGPDL_MUZAFFARPUR_JDF-0807: VL/VL,   RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: both cure
## VSGPDL_MUZAFFARPUR_JDF-0822: VL/VL,   RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: both cure
## VSGPDL_MUZAFFARPUR_JDF-0883: VL/VL,   RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: both cure
## VSGPDL_MUZAFFARPUR_JDF-0903: VL/VL,   both RELAPSE history,        both Day 30 COMPLETE RECOVERY, 6 months is only available for 1st episode (cure)
## VSGPDL_MUZAFFARPUR_JDF-0912: VL/VL,   RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: cure in both episodes
## VSGPDL_MUZAFFARPUR_JDF-0971: VL/VL,   RELAPSE only second episode, both Day 30 COMPLETE RECOVERY, 6 months: cure in 1st episode, relapse in 2nd episode

# DM ----
DM %>% colnames()

# 1027 patient records
count(DM)

# SUBJID, USUBJID are unique
DM %>%
  count(SUBJID) %>%
  filter(n > 1) # unique
DM %>%
  count(USUBJID) %>%
  filter(n > 1) # unique

# for now, remove duplicates and PKDL patients
# now there are 977 unique patients
DM_clean <- DM %>%
  filter(!USUBJID %in% REMOVE)
DM_clean %>% count()

# variables (non-timing)
DM_clean %>% count(SITEID) # always MAZAFFARPUR
DM_clean %>% var_sum(AGE) # missing in 12 patients
DM_clean %>% count(AGEU) # missing in 12 patients
DM_clean %>% count(SEX) # missing in 12 patients
DM_clean %>% count(ARMCD) # always LAMB
DM_clean %>% count(ARM) # always AMPHOTERICIN B
DM_clean %>% count(COUNTRY) # always IND

DM_clean %>% count(DTHDTC, DTHFL) # one death (also described in DS domain)

DM_clean %>%
  filter(!is.na(RFSTDTC)) %>%
  summarise(test = all(RFSTDTC == DMDTC)) # all RFSTDTC equal to DMDTC (except missing)

# only one death flag domain in DM (JDF-0815) - died on day 39
DM_clean %>%
  count(SUBJID, DTHDTC, DTHFL) %>%
  arrange(desc(DTHFL))

DM_clean %>% colnames()
DM_clean %>% count(DTHFL)

DM_clean <- DM_clean %>%
  rename(
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_AGE = AGE,
    DM_SITE = SITEID,
  ) %>%
  select(starts_with("DM_"), USUBJID)
DM_clean %>% colnames()

DM_clean %>%
  count(DM_AGE) %>%
  print(n = Inf)
DM_clean %>% count(DM_SEX)

VSGPDL <- VSGPDL %>%
  full_join(DM_clean)

# OUTCOME COMMENTS ----

# RS:
# RS_IC is complete for all entries in the RS domain
# The 6 False IC are due to drug reactions (one entry is 'DRUG STOP')
# One entry is cure at IC but hypersensitivity for medical history and DC; so changed to False IC
# All False DC (and therefore True IC) are due to relapse. No LTFU identified in dataset (or publication)
# 59 prior VL relapses identified in RS MEDICAL HISTORY (44 in the publication)

# DS:
# DS: 1 death (occurred day 40 in a patient with IC), not documented in RC
# 1 relapse less than RS domain
# 358 patients with IC have no DC outcome (for both DC and RS domains)

# DM:
# 1 death (same patient as recorded in DS domain)

# PT ----

# PT: all 1027 in DM included
# except for USUBJID, all entries are the same

######

# IN DOMAIN
# The 10 duplicate patients are patients that are retreated
# A number of patients received multidose LAMB (6 days of doses)
## 4 of the 10 duplicates received multidose LAMB (PKDL patients)
## 42 of the non-duplicates received multidose LAMB (PKDL patients)
## many of the timing variables are inconsistant - check with Gemma here

######
# IN #
######

# All variables found in the:
# Concomitant and Prior Medications (CM),
# Exposure (EX)
# Exposure as Collected (EC)


# let's delete the second treatments!!
# VSGPDL_MUZAFFARPUR_JDF-0971 - in DS and RS domains, listed as definitive cure,
# but here it describes retreatment within 6 months. Go with DS/RS for now.

IN_clean <-
  IN %>%
  filter(!(
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0135" & INSEQ == 1) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0135" & INSEQ == 2) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0715" & INSEQ == 1) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0715" & INSEQ == 2) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0741" & INSEQ == 1) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0741" & INSEQ == 2) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0752" & INSEQ == 1) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0752" & INSEQ == 2) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0807" & INSEQ == 3) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0807" & INSEQ == 4) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0822" & INSEQ == 2) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0822" & INSEQ == 3) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0883" & INSEQ == 3) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0883" & INSEQ == 2) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0903" & INSEQ == 3) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0903" & INSEQ == 4) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0912" & INSEQ == 2) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0912" & INSEQ == 3) | #second treatment
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0971" & INSEQ == 2) | #relapse
    (USUBJID == "VSGPDL_MUZAFFARPUR_JDF-0971" & INSEQ == 3))  #second treatment
  )

# filter worked
IN_clean %>%
  filter(USUBJID %in% DUP) %>%
  arrange(USUBJID, EPOCH, INSTDTC)

colnames(IN_clean)
# all USUBJIDs are now unique within INCAT. Let's now pivot, clean, and merge with MASTER
IN_clean %>% count(INCAT)
IN_clean %>%
  count(INCAT, USUBJID) %>%
  count(n)


IN_clean <- IN_clean %>%
  mutate(INCAT = if_else(INCAT == "MEDICAL HISTORY", "IN_RELAPSE", "TREATMENT", missing = "TREATMENT"))

IN_clean_wide <- IN_clean %>%
  mutate(pivot_value = paste(INTRT, INDOSRGM)) %>%
  pivot_wider(
    id_cols = c(USUBJID),
    names_from = INCAT,
    values_from = pivot_value
  )

# USUBJID now unique
IN_clean_wide %>%
  count(USUBJID) %>%
  arrange(desc(n))

colnames(IN_clean_wide)
IN_clean_wide %>%
  count(TREATMENT) %>%
  print(n = 50)
IN_clean_wide <- IN_clean_wide %>%
  mutate(
    IN_TREATMENT = case_when(
      TREATMENT == "AMBISOME Single dose of 10 mg/kg in 5% dextrose solution over 2 hours" ~ "Single Dose LAMB (10mg/kg)",
      .default = "Multiple dose LAMB (PKDL treatment)"
    ),
    IN_RELAPSE = case_when(
      is.na(IN_RELAPSE) ~ "NO",
      .default = "YES"
    )
  ) %>%
  mutate(IN_DOMAIN = "IN") %>%
  select(-TREATMENT)

# SA DOMAIN
## 3 SATERMS are 'AT ANY TIME' - "Loss of appetite", "Loss of weight", "Weakness"
## but, based on table 1 in the publication, I suspect these all refer to history (numbers are similar)

## Remainder are "BASELINE" variables
### HIV (positive in 4 entries, 3 patients (one is a duplicate entry), remainder missing
### History of cough (complete)
### History of fever (continuous as duration, missing in 41/1005)
### History of Kala-azar (Y/N, no missing)
### History of Rigors (Y/N, no missing)
### History of Vomiting (Y/N, no missing - but only present in 5 patients)
### Loss of appetite (Y/N, no missing)
### Loss of weight (Y/N, no missing)
### PKDL (42 with Y, remainder are missing)
### Weakness (Y/N, no missing)

# SA ----

SA_clean <- SA %>% 
  filter(!USUBJID %in% REMOVE)
SA_clean %>% colnames()

SA_clean %>% count(SACAT, SATERM, VISIT, VISITNUM, EPOCH)

# confirm each SACAT/SATERM/VISIT is unique
SA_clean %>% 
  group_by(USUBJID, SACAT, SATERM, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(n_dup)

SA_clean %>% count(SACAT, SATERM, VISIT, VISITNUM, EPOCH, SADY, SAEVINTX, SASTRTPT, SASTTPT)
SA_clean %>% count(SATERM, SAPRESP, SAOCCUR)

# all fever duration is stored in SADUR (there is no duration data stored in SASTDY or SADY as seen in other datasets)
SA_clean %>% count(SADY,SADUR,SASTTPT, SASTRTPT, SAEVINTX) %>% print(n = Inf) 
SA_clean %>% count(SATERM, SAOCCUR, SASTAT, SAREASND, !is.na(SADUR))

# create according to dictionary
VSGPDL <- SA_clean %>% 
  filter(!is.na(SADUR)) %>% 
  select(USUBJID, SA_HX_FEV_DUR = SADUR) %>% 
  full_join(VSGPDL) 

VSGPDL <- SA_clean %>% 
  filter(SATERM != "History of Fever") %>% 
  mutate(SAOCCUR = case_when(
    SAOCCUR == "Y" ~ TRUE,
    SAOCCUR == "N" ~ FALSE,
    .default = NA
  )) %>% 
  mutate(SATERM = str_replace_all(SATERM, " ", "_")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR
  )  %>% 
  rename(
    SA_HX_APP = Loss_of_Appetite,
    SA_HX_WL = Loss_of_Weight,
    SA_HX_WEAK = Weakness,
    SA_HX_HIV = HIV,
    SA_HX_CGH = History_of_Cough,
    SA_HX_VL = `History_of_Kala-azar`,
    SA_HX_RGR = History_of_Rigors,
    SA_HX_VOM = History_of_Vomiting
  ) %>% 
  select(-PKDL) %>% 
  full_join(VSGPDL)

# LB ----

# Routine measurements at days 0, 2 and 30
## Liver: ALT, Renal: Creatinine,  K, Na, FBC: Hb, plts, leuks, lymph, eosinophils, monocytes, neuts,
# Routine measurements at days 0, 2 (only)
## Alb, ALP, AST, Bili, glucose, urea
# Routine measurements at day 0 (although the LBDY are not always = 1)
## hCG - 9 positive (all in women, but only outcomes for a few)

# Days 0 and 2: 890 patients (all blood tests except PTT)
# AND Day 30: 860 patients (Na, K, Plt, Neuts, Mono, Lymph, Leuks, Hb, Eos, Cr, ALT) (Alb has 1 Day 30 entry)

# LBDY values seem unreliable - ignore for now

LB_clean <- LB %>% 
filter(!USUBJID %in% REMOVE)

LB_clean %>% colnames()
LB_clean %>% count(VISIT)

# relook June 25th 2024 - this time keeping the NA values
LB_clean %>% count(VISIT)
LB_clean %>% count(VISIT, LBTESTCD) %>% print(n = Inf)
LB_clean %>% count(VISIT, LBTESTCD, LBDY) 
LB_relook <- LB_clean %>% 
  filter(is.na(VISIT)) %>% 
  group_by(USUBJID, LBTESTCD) %>% 
  arrange(LBDY) %>% 
  mutate(num = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(LBTESTCD, num),
    values_from = c(LBSTRESN, LBDY),
    names_glue = "LB_{LBTESTCD}_{num}_{.value}"
  ) %>% select(matches("^LB_CREAT_[0-9]+_LBDY"))
LB_relook %>% relocate(sort(names(LB_relook))) %>% arrange(across(sort(names(LB_relook)))) #%>% View()
  #View()
  
LB_clean %>% filter(LBTESTCD == "ALT") %>% count(VISIT, LBDY) %>% print(n = Inf)

# For now, remove the VISIT == NA values and VISIT = "Day 2" values
LB_clean <- LB_clean %>% 
  filter(VISIT %in% c("Day 0", "Day 30"))

# All USUBJIDs are now unique by VISIT and LBTESTCD
LB_clean %>% 
  group_by(USUBJID, VISIT, LBTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(n_dup)

# Remove Day 30 ALB (only one value, unlikely trustworthy)
LB_clean %>% 
  count(VISIT, LBTESTCD) %>% print(n = Inf)
LB_clean %>% colnames()
LB_clean_LBSTRESN <- LB_clean %>% 
  filter(
    !(VISIT == "Day 30" & LBTESTCD == "ALB"),
    LBTESTCD != "HCG") %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 30" ~ "IC"
    )
  ) %>% 
  select(USUBJID, VISIT, LBTESTCD, LBSTRESN) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_clean_LBORRES <- LB_clean %>% 
  filter(
    LBTESTCD == "HCG") %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 30" ~ "IC"
    )
  ) %>% 
  select(USUBJID, VISIT, LBTESTCD, LBORRES) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBORRES,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% 
  mutate(
    LB_BL_HCG = case_when(
      LB_BL_HCG == "NEGATIVE" ~ FALSE,
      LB_BL_HCG == "POSITIVE" ~ TRUE,
      .default = NA
    )
  )

LB_clean_LBSTRESN %>% colnames()
LB_clean_LBORRES %>% colnames()
LB_clean_LBORRES %>% count(LB_BL_HCG)

# LB TIMING

LB %>% names()
LB %>% count(VISIT, VISITDY, LBDY, !is.na(LBDTC), LBSTRF, LBSTRTPT, LBSTTPT) %>% print(n = Inf)
# There is no LBDY data for day 30 measurements, therefore will have to assume that all IC are BL + 30 days

VSGPDL %>% 
  full_join(LB_clean_LBSTRESN) %>% 
  full_join(LB_clean_LBORRES) %>% dim()

VSGPDL <- VSGPDL %>% 
  full_join(LB_clean_LBSTRESN) %>% 
  full_join(LB_clean_LBORRES) 

# MB ----

# MBSTRTPT does not always label second episodes as "AFTER"
# Labelled with AFTER:
## VSGPDL_MUZAFFARPUR_JDF-0807
## VSGPDL_MUZAFFARPUR_JDF-0822
## VSGPDL_MUZAFFARPUR_JDF-0883
## VSGPDL_MUZAFFARPUR_JDF-0903
## VSGPDL_MUZAFFARPUR_JDF-0912
## VSGPDL_MUZAFFARPUR_JDF-0971

# Not Labelled with AFTER
## VSGPDL_MUZAFFARPUR_JDF-0752 (remove MBDY == 1)
## VSGPDL_MUZAFFARPUR_JDF-0741 (remove MBDY == -148)
## VSGPDL_MUZAFFARPUR_JDF-0715 (remove MBDY == 400)
## VSGPDL_MUZAFFARPUR_JDF-0630 - this patient appears to have had two splenic biopsies, but one episode
## VSGPDL_MUZAFFARPUR_JDF-0135 (remove MBDY == 295)

MB_clean <- MB %>% 
  filter(!USUBJID %in% REMOVE)
names(MB)
dim(MB)

# 4 different microbiological tests
MB_clean %>% count(MBTESTCD, MBTEST, VISIT)

# all USUBJID are unique within MBTESTCD and VISIT
MB_clean %>%
  #filter(MBTESTCD == "LSHMANIA") %>% 
  group_by(USUBJID, MBTESTCD, MBTEST, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(MBTESTCD, n_dup)

colnames(MB_clean)

# all USUBJID are unique within MBTESTCD *EXCEPT* for LSHMANIA (which has 78 duplicates)
MB_clean %>%
  group_by(USUBJID, MBTESTCD, MBTEST) %>% 
  mutate(
    n_dup = row_number(),
    n_tot = n()) %>% 
  ungroup() %>% 
  filter(MBTESTCD == "LSHMANIA", !is.na(MBORRES)) %>% 
  count(n_tot, MBTESTCD, !is.na(MBORRES), MBSTAT, MBREASND, MBLOC, MBMETHOD, VISIT, EPOCH,, MBDY, MBSTRF, MBCDSTDY)  #%>% View()

# re-look at the data June 25th 2024
# there are 22 additional patients with NA as VISIT, who have baseline parasitology, in addition to the 77 patients with "Day 0" in VISIT
# let's use all of these as baseline
MB_RELOOK <- MB_clean %>% 
  filter(
    !(USUBJID %in% REMOVE),
    MBTESTCD == "LSHMANIA", 
    !is.na(MBORRES)
  ) %>% 
  mutate(
    VISIT = ifelse(is.na(VISIT), "U", "D0")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT, 
    values_from = MBORRES
  )  %>% 
  rename(MB_BL_LSHMANIA_SPLEEN = U)  %>% 
  select(USUBJID, MB_BL_LSHMANIA_SPLEEN)


# all MBDY for the NA VISITs (MBTESTCD = LSHMANIA) are near to, or not long before recruitment day.
# so let's describe all of them as baseline values 
MB_clean %>% 
  ungroup() %>% 
  filter(
    MBTESTCD == "LSHMANIA"
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT, 
    values_from = MBORRES
  ) %>% count(`Day 0`, `NA`)

MB_clean_wide <- MB_clean %>% 
  filter(MBTESTCD != "LSHMANIA") %>% 
  mutate(VISIT = str_replace_all(VISIT, " ", "_")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD),
    values_from = MBORRES
  ) %>% 
  mutate(
    MB_BL_HIVAB = case_when(
      is.na(Day_0_HIVAB) ~ NA_HIVAB,
      .default = Day_0_HIVAB
    ),
    MB_BL_HIVAB = case_when(
      MB_BL_HIVAB == "NEGATIVE" ~ FALSE,
      MB_BL_HIVAB == "POSITIVE" ~ TRUE,
      .default = NA
    ),
    MB_BL_HBSAG = case_when(
      is.na(Day_0_HBSAG) ~ NA_HBSAG,
      .default = Day_0_HBSAG
    ),
    MB_BL_HBSAG = case_when(
      MB_BL_HBSAG == "NEGATIVE" ~ FALSE,
      MB_BL_HBSAG == "POSITIVE" ~ TRUE,
      .default = NA
    ),
    MB_BL_LSHMRK39 = TRUE
  ) %>% 
  select(USUBJID, starts_with("MB_"))

MB_clean_wide %>% colnames()
# MB_clean_wide %>% count(Day_0_HIVAB, NA_HIVAB) # there are 27 HIV positive patients
# MB_clean_wide %>% count(Day_0_HBSAG, NA_HBSAG) # there are 13 HBSAG postive patients
# MB_clean_wide %>% count(Day_0_LSHMRK39, NA_LSHMRK39) # all LSHMRK39 are positive
MB_clean_wide %>% count(MB_BL_HIVAB)
MB_clean_wide %>% count(MB_BL_HBSAG)
MB_clean_wide %>% count(MB_BL_LSHMRK39)

#MB_clean_wide %>% count(Day_0_LSHMANIA, NA_LSHMANIA) # as expected

VSGPDL <- VSGPDL %>% 
  full_join(MB_RELOOK) 
VSGPDL <- VSGPDL %>% 
  full_join(MB_clean_wide) 


# MP ----

# LIVER and SPLEEN length

MP_clean <- MP %>% 
  filter(!USUBJID %in% REMOVE)

MP_clean %>% colnames()
# only one test - which is LENGTH
MP_clean %>% count(MPTEST, MPTESTCD)
MP_clean %>% count(MPTESTCD, MPLOC, VISIT)

# 76 patients have multiple repeat liver and spleen measurements. Why?
MP_clean %>% 
  group_by(USUBJID, MPTESTCD, MPLOC, VISIT) %>% 
  arrange(MPDY) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(MPTESTCD, MPLOC, VISIT, MPDY) %>% print(n = Inf)

MP_explore <- MP_clean %>% 
  group_by(USUBJID, MPTESTCD, MPLOC, VISIT) %>% 
  arrange(MPDY) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  mutate(VISIT = str_replace_all(VISIT, " ", "_")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MPLOC, VISIT, n_dup),
    values_from = MPORRES
  )

# the VISIT = NA liver and spleen measurements are all (except for one) independent of the labelled VISIT measurements (Day 1,2,30). 
# I suspect we could use MBDY to attempt to scavenge some of the remaining values
# I also suspect that the VISIT = NA values may be the USUBJIDs that were excluded from the trial (e.g. they were sick, which is why some of them had multiple measurements)
# For now exclude the VISIT = NA values -> potential to further dredge the data later on if considered important. 
MP_explore %>% colnames()
MP_explore %>% 
  count(
    !is.na(SPLEEN_Day_1_1), !is.na(SPLEEN_Day_2_1), !is.na(LIVER_Day_30_1),
    !is.na(SPLEEN_NA_1), !is.na(SPLEEN_NA_2), !is.na(SPLEEN_NA_3), !is.na(SPLEEN_NA_4), !is.na(SPLEEN_NA_5), !is.na(SPLEEN_NA_6), !is.na(SPLEEN_NA_7),
    !is.na(SPLEEN_NA_8), !is.na(SPLEEN_NA_9), !is.na(SPLEEN_NA_10)
  ) #%>% View()

MP_clean_wider <- MP_clean %>% 
  filter(!is.na(VISIT) & VISIT != "Day 2") %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 1" ~ "BL",
      VISIT == "Day 30" ~ "IC",
      .default = NA
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH",
    values_from = MPORRES
  ) 

MP_clean_wider %>% colnames()
VSGPDL %>% colnames()
VSGPDL  %>% dim()
VSGPDL <- MP_clean_wider %>% 
  full_join(VSGPDL)

# MP TIMING 

MP %>% names()
MP %>% count(VISIT, VISITDY, MPDY, !is.na(MPDTC)) %>% print(n = Inf)
# no day 30 MPDY data

# VS ----
# VS is similar to MP w.r.t. VISIT missing data

VS_clean <- VS %>% 
  filter(!USUBJID %in% REMOVE)
VS_clean %>% colnames()

# DBP/SBP/Temp/Weight all at Day 1,2,30
# Temp/Weight also at Day 0
# Height at Day 0
VS_clean %>%
  count(VSTESTCD, VSTEST, VISIT) %>%
  print(n = 40)

VS_clean %>% 
  group_by(USUBJID, VSTESTCD, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VSTESTCD, VISIT, n_dup) %>% print(n = Inf)


# Day 0:  HEIGHT, TEMP, WEIGHT
# Day 1:  DIABP, PULSE, SYSBP, TEMP, WEIGHT
# Day 2:  DIABP, PULSE, SYSBP, TEMP, WEIGHT
# Day 30: DIABP, PULSE, SYSBP, TEMP, WEIGHT

# for now let's adopt the same strategy as per MP
# let's take BL HEIGHT, WEIGHT from Day 0
# let's take BL DIABP, PULSE, SYSBP, TEMP from Day 1
# let's take IC DIABP, PULSE, SYSBP, TEMP, WEIGHT from Day 30

VS_clean %>% colnames()
VS_clean_wide <- VS_clean %>% 
  filter(
    !is.na(VISIT) & VISIT != "Day 2",
    !(VISIT == "Day 0" & VSTESTCD == "TEMP"),
    !(VISIT == "Day 1" & VSTESTCD == "WEIGHT")) %>% 
  mutate(
    VISIT = case_when(
      VISIT %in% c("Day 0", "Day 1") ~ "BL",
      .default = "IC")
    ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD), 
    names_glue = "VS_{VISIT}_{VSTESTCD}",
    values_from = VSSTRESN
  ) # %>% View()

VS_clean_wide %>% dim()
VS_clean_wide %>% colnames()

# VS TIMING

VS %>% count(VISIT, VISITDY, VSDY, !is.na(VSDTC)) %>% print(n = Inf)
VSGPDL <- VSGPDL %>% full_join(VS_clean_wide) 
# No 30 day VSDY

# OTHER DOMAINS ----
# PE - physical examination #
# no useful information here

# likely same VISIT NA culprits
PE_clean <- PE %>% 
  filter(!USUBJID %in% REMOVE)
  
PE_clean %>% colnames()

PE_clean %>% 
  count(PETESTCD, PETEST, VISIT)

# all PEORRES are populated:
## CHEST: CLEAR
## CVS: NORMAL
## PA: SOFT
## PKDL: descriptions of PKDL lesions!
PE_clean %>%
  filter(PETESTCD == "PKDL") %>%
  select(PEORRES, PESTRESC) #%>% View()

# QS - questionnaires #
# these are all Karnofsky Performance Statuses
QS %>%
  count(QSTESTCD, QSTEST, VISIT, QSORRES)

# these are all pregnancy tests
RP_clean <- RP  %>% 
  filter(!USUBJID %in% REMOVE) 

RP_clean %>%
  count(RPORRES, RPSTRESC, VISIT, RPSTRF, RPCDSTDY) %>%
  print(n = Inf)

# all USUBJIDs are unique
RP_clean %>% count(USUBJID) %>% count(n)
RP_clean_wide <- RP_clean %>% 
  mutate(
    RP_PREG = case_when(
      RPORRES == "NEGATIVE" ~ FALSE,
      RPORRES == "POSITIVE" ~ TRUE,
      .default = NA
    )
  ) %>% 
  select(USUBJID, RP_PREG)

VSGPDL <- VSGPDL %>% full_join(RP_clean_wide) 

# EXPLORE DATASET ----

VSGPDL %>% colnames()
VSGPDL %>% count(RP_PREG)
VSGPDL %>% count(RP_PREG, SA_HX_HIV, MB_BL_HIVAB)
VSGPDL %>% 
  save(VSGPDL, file = "Study/Data/VSGPDL.RData")