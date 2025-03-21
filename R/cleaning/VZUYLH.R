# EXCLUDE - unable to identify outcomes

##########
# VZUYLH #
##########

rm(list = ls())
source("definitions.R")
load_domains("VZUYLH")

mg <- ld_missingness("VZUYLH")
mg  #%>% View_lt()

# mg %>% 
#   mod_lt() %>% 
#   upset(
#     nsets = 15, 
#     nintersects = 50, 
#     order.by = c("freq"), 
#     set_size.show = TRUE,
#     text.scale = 2
#   )

# INTRO ----

# March 22 2024

# Comparison of short-course multidrug treatment with standard therapy for visceral leishmaniasis in India:
# an open-label, non-inferiority, randomised controlled trial

# From the article (Sundar et al 2011):
# Overall this DNDi/KAMRC appears to be a well conducted and well designed study,
# with a protocol available.
# "There were two relapses in each group" - i.e. 8 relapses in total
# 634 patients (corresponding to 634 in the dataset)

# From the dataset:
# only have final outcomes for 44 patients
# no relapses are identified (i.e. we have no outcome data!)
# IN treatment details only available for approx one third of patients (regardless of site)

# Questions for Gemma/Sean:
# most importantly: where are the outcomes?

# DM Domain Summary:
# matches almost perfectly with publication results (except slight age discrepancy)
# n = 634 USUBJIDs

# DM ----

colnames(DM)

# 634 USUBJIDs (unique)
DM %>%
  summarise(
    n = n(),
    n_distinct = n_distinct(USUBJID)
  )

# sites consistent with publication
# MUZAFFARPUR: 478
# PATNA: 156
DM %>% count(SITEID)

# mean age consistent with publication
# min age almost consistent with publication (5 instead of 6)
DM %>%
  var_sum(AGE)
DM %>%
  summarise(
    age_18 = sum(AGE <= 18),
    age__18 = sum(AGE < 18)
  )

DM %>% count(AGEU)
DM %>% count(SEX) # same in publication

# same as publication
DM %>% count(ARM, ARMCD, SITEID)

# DMDTC always the same as RFSTDTC
DM %>% count(DMDY)
DM %>% count(ARM)
DM_merge <- DM %>%
  rename(
    DM_AGE = AGE,
    DM_SEX = SEX,
    DM_SITE = SITEID,
    DM_ARM = ARMCD
  ) 

VZUYLH <- DM_merge %>% 
  select(USUBJID, RFSTDTC, contains("DM_"))
VZUYLH %>% names()

# Inclusion/exclusion criteria extracted from both protocol and publication
# Appears only the age criteria extracted from publication (different to protocol)

# TV ----

TV # %>% View()

# some visits linked to arms, others not
TV %>%
  count(VISITNUM, VISIT, VISITDY, ARMCD, ARM, TVSTRL) %>% 
  print(n = Inf)

# TI ----
TI # %>% View()
colnames(TI)

# RS ----

RS # %>% View()
colnames(RS)
# missingness("RS")

# 626 USUBJIDs with day 45 data
# 150 USUBJIDs with day 31 data
# 476 USUBJIDs with day 15 data
# 44 USUBJIDs with 6 month data (including 3 USUBJIDs with two entries)
RS %>% count(RSTEST, RSTESTCD, RSCAT, VISIT)
RS %>%
  group_by(RSTEST, VISIT, USUBJID) %>%
  mutate(
    USUBJID_DN = row_number(),
    USUBJID_DT = max(row_number())
  ) %>%
  group_by(RSTEST, VISIT, USUBJID_DN, USUBJID_DT) %>%
  summarise(n = n())

# 6 month overall responses:
# 3 USUBJIDs with 2 x 6 month outcomes (2 of these are conflicting)
# I suspect 'FINAL CURE' is just incorrect, and these should be lack of initial cure (judging by RSDY)
# Why do we only have 44 USUBJIDs with overall response?

# MUZAFFARPUR_1052 (ADVERSE EVENT / SERIOUS ADVERSE EVENT & TREATMENT FAILURE) (AMB DEOXY) 
# PATNA_2118 (ADVERSE EVENT/SERIOUS ADVERSE EVENT - TREATMENT FAILURE & COMPLETED) (LAMBMIT)
# PATNA_2011 (ADVERSE EVENT/SERIOUS ADVERSE EVENT - TREATMENT FAILURE & COMPLETED) (AMB DEOXY))

# judging from the RSDY values, let's assign all adverse event / treatment failures as RC_IC = FALSE 
# including the two cases that have contradictory RC_DC (for now, need to ask Gemma)

# No death or relapse identified

RS %>%
  group_by(RSTEST, VISIT, USUBJID) %>%
  mutate(
    USUBJID_DN = row_number(),
    USUBJID_DT = max(row_number())
  ) %>%
  filter(VISIT == "6 Months") %>%
  arrange(USUBJID_DT) #%>% View()
  #print(n = 10)

# Let's pivot wider to better view day 15, 31, 45 and 6 month outcomes by patient
RS %>% count(VISIT)
RS_clean <- RS %>% 
  mutate(
    VISIT = case_when(
      VISIT == "6 Months" ~ "M6",
      VISIT == "Day 15" ~ "D15", 
      VISIT == "Day 31" ~ "D31",
      VISIT == "Day 45" ~ "D45",
      .default = "error"
    )
  )

# all the RS M6 AE/SAEs(-TF) occur with RSDY < 30 days
RS_clean %>% names()
RS_clean %>% count(VISIT)
RS_clean %>% count(VISIT, RSORRES)
RS_clean %>% count(VISIT, RSORRES, RSDY) %>% arrange(RSORRES, RSDY) %>% print(n = Inf)

OUT_pivot <- RS_clean %>% 
  group_by(USUBJID, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  mutate(
    VISIT = ifelse(VISIT == "M6", str_c(VISIT, n_dup, sep = "_"), VISIT)
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c("RSORRES", "RSDY")
  ) %>% 
  full_join(DS %>% select(USUBJID, DSTERM, DSDY)) 

OUT_pivot %>% names()
OUT_pivot %>% count(across(contains("RSORRES")), DSTERM) #%>% View()
OUT_pivot %>% dim()
OUT_pivot %>% count(DSTERM)

# Look at merging with MB to identify the relapses - unable to identify relapses 

# for treatment failure / adverse events / serious adverse events -> look at RSDY and DSDY for clues on when they happened
# all these events took place within BL-IC period (from DSDY and RSDY)

OUT_pivot %>% filter(DSTERM %in% c("Didn't Complete", "Treatment Failure"))#%>% View()

OUT_pivot_coded <- OUT_pivot %>% 
  mutate(
    OUT_IC = case_when(
      RSORRES_D45 == "COMPLETED" & !is.na(RSORRES_D45) ~ TRUE,
      .default = FALSE,
    ),
    OUT_IC_DRUG = FALSE,
    OUT_NA = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_DC = case_when(
      DSTERM == "Completed" & !is.na(DSTERM) ~ TRUE,
      .default = FALSE
    ),
    OUT_DC_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_XX_DEATH = FALSE, 
    OUT_DC_RELAPSE = FALSE,
    OUT_DC_OTHER = FALSE, 
    OUT_IC_OTHER = case_when(
      DSTERM %in% c("Didn't Complete", "Treatment Failure") & !is.na(DSTERM) ~ TRUE,
      .default = FALSE
    ),
    OUTC_IC_OTHER = "There are 9 patients with adverse events or treatment failure reported at 6 months in RS, and DS outcomes, all with missing initial cure outcomes in RS. Perhaps the 8 x relapses reported in the publication are here, however all timing information (-DY) suggest these outcomes occurred during treatment. From publication, we believe 12 patients did not achieve IC due to 1 death, 9 withdrawals, 2 protocol violations. The relapses may well be in the patients with no final outcome."
  )

# The 9 OUT_IC_OTHER = TRUE cases correspond to TREATMENT FAILURE / AE / SAE / Didn't complete -> likely should consider these as IC failures as in manuscript
OUT_pivot_coded %>% count(across(contains("OUT_"))) %>% print(width = Inf)

VZUYLH <- VZUYLH %>% full_join(OUT_pivot_coded %>% select(USUBJID, starts_with("OUT")))
VZUYLH %>% names()
VZUYLH %>% count(across(starts_with("OUT_"))) %>% print(width = Inf)

# PT ----

PT # %>% View()
colnames(PT)
# IN ----

IN # %>% View()
colnames(IN)

# there is a lot of information here
# INCAT: CONCOMITTANT MEDICATION, MEDICAL HISTORY, NA
# For our purpuses, INCAT == NA refers to study medication
IN %>%
  count(INCAT, INGRPID, INTRT) %>%
  arrange(INCAT, INGRPID, desc(n)) %>%
  print(n = 10)

# 52 patients had 15 doses on amphotericin B
# 52 patients had 1 dose of Ambisome and 10 doses of paromomcyin
# 55 patients had 1 dose of Ambisome and 14 doses of miltefosine
# 54 patients had 10 doses of paromomycin and 20 doses of miltefosine
# so we only have treatment information for 213 patients
IN %>%
  filter(is.na(INCAT)) %>%
  group_by(USUBJID) %>%
  summarise(
    num_milt = sum(INTRT == "Miltefosine"),
    num_amb = sum(INTRT == "Ambisome"),
    num_par = sum(INTRT == "Paromomycin"),
    num_amph = sum(INTRT == "Amphotericin B"),
    n = n(),
  ) %>%
  ungroup() %>%
  count(num_milt, num_amb, num_par, num_amph)

IN_CLEAN <- IN %>%
  filter(is.na(INCAT)) %>%
  group_by(USUBJID) %>%
  summarise(
    num_milt = sum(INTRT == "Miltefosine"),
    num_amb = sum(INTRT == "Ambisome"),
    num_par = sum(INTRT == "Paromomycin"),
    num_amph = sum(INTRT == "Amphotericin B"),
    n = n(),
  ) %>%
  ungroup() %>%
  mutate(
    IN_TREATMENT = case_when(
      num_milt == 14 & num_amb == 1 ~ "Ambisome and miltefosine",
      num_par == 10 & num_amb == 1 ~ "Ambisome and paromomycin",
      num_amph == 15 ~ "Amphotericin B",
      num_milt == 20 & num_par == 10 ~ "Miltefosine and paromomycin",
      .default = NA_character_
    )
  ) %>%
  select(USUBJID, IN_TREATMENT) %>%
  mutate(IN_DOMAIN_TRT = "IN")

# no patterns of missing IN_TREATMENT_TRT immediately obvious
# regardless of study site, approximately 1/3rd of patients have IN_TREATMENT details

# SA ----

SA # View()
colnames(SA)

SA %>% count(SATERM)

SA %>%
  count(USUBJID) %>%
  count(n) %>%
  print(n = Inf)

# we are interested only in pre-specified symptoms
SA %>%
  filter(SAPRESP=="Y") %>% 
  count(VISIT, EPOCH, SACAT, SATERM, SAPRESP, SAOCCUR) %>%
  arrange(VISIT, EPOCH, SACAT, SAPRESP, SATERM) %>% print(n = Inf)

SA %>% count(VISIT, SATERM, T = !is.na(SADUR)) %>% arrange(T) %>% print(n = Inf)
SA %>% filter(SATERM %in% c("FEVER", "FEVER WITH CHILLS AND RIGOR", "FEVER WITH RIGOR 6 MONTH")) %>% count(SADUR) %>% print(n = Inf)
SA %>% filter(SATERM == "FEVER") %>% count(VISIT, SATERM, EPOCH, SACAT, SATERM, SAPRESP, SAOCCUR)

# prespecified SATERMS are the same for all time points (including symptoms and)

# SCREENING (n = 634);
# MEDICAL HISTORY: Vomiting, Nausea, Loss of weight, Feeling of weakness, Epistaxis, Diarrhoea, Cough, Anorexia, Abdominal pain
# SYMPTOMS AT BASELINE: Hepatomegaly, Icterus, Pallor, Peripheral Lymphadenopathy, Splenomegaly

# TREATMENT + FOLLOW-UP - Day 7 (n = 632); Day 45 (n = 625); Day 15 (n = 476); Day 31 (n = 149); 6 Months (n = 35)
# Vomiting, Splenomegaly, Peripheral Lymphadenopathy, Pallor, Nausea, Loss of weight, Icterus, Hepatomegaly, Feeling of weakness, Epistaxis, Diarrhoea, Cough, Anorexia, Abdominal pain

# Fever (duration of) (n = 187) - SCREENING ONLY

# for non-specified SATERMS only, some USUBJIDs have more than one event for the 6 MONTHS VISIT
SA %>%
  group_by(USUBJID, VISIT, EPOCH, SACAT, SATERM, SAPRESP) %>%
  mutate(
    n_dup = row_number(),
    n_max = max(n_dup)
  ) %>% 
  ungroup() %>% 
  count(VISIT, SACAT, SATERM, n_dup) %>% print(n = Inf, width = 75)
  

# let's focus on pre-specified terms for now
baseline_signs <- c("Splenomegaly", "Peripheral Lymphadenopathy", "Pallor", "Icterus", "Hepatomegaly")
history_symp <- c("Vomiting", "Nausea", "Loss of weight", "Feeling of Weakness", "FEVER", "Epistaxis", "Diarrhoea", "Cough", "Anorexia", "Abdominal pain")

SA_merge <- SA %>% 
  filter(
    (VISIT == "Screening" & is.na(SACAT) & SATERM %in% baseline_signs) |
    (VISIT == "Screening" & SACAT == "MEDICAL HISTORY" & SATERM %in% history_symp) |
    (VISIT == "Day 45" & SATERM %in% c(baseline_signs, history_symp))) %>% 
  mutate(
    TIMING = case_when(
      VISIT == "Screening" & is.na(SACAT) ~ "BL",
      VISIT == "Screening" & SACAT == "MEDICAL HISTORY" ~ "HX", 
      VISIT == "Day 45" ~ "IC"
    ),
    SACODE = case_when(
      SATERM == "Abdominal pain"            ~ "ABDO",
      SATERM == "Anorexia"                  ~ "APP",
      SATERM == "Cough"                    ~ "CGH",
      SATERM == "Diarrhoea"                ~ "DIARR",
      SATERM == "Epistaxis"                ~ "EPIS",
      SATERM == "FEVER"                    ~ "FEV_DUR",
      SATERM == "Feeling of Weakness"      ~ "WEAK",
      SATERM == "Hepatomegaly"             ~ "HEP",
      SATERM == "Icterus"                  ~ "ICT",
      SATERM == "Loss of weight"            ~ "WL",
      SATERM == "Nausea"                    ~ "NAUS",
      SATERM == "Pallor"                      ~ "PAL",
      SATERM == "Peripheral Lymphadenopathy"  ~ "LYM",
      SATERM == "Splenomegaly"                ~ "SPLN",
      SATERM == "Vomiting"                    ~ "VOM"
    )
  )

# check
SA_merge %>%
  group_by(USUBJID, VISIT, EPOCH, SACAT, SATERM, SAPRESP) %>%
  mutate(
    n_dup = row_number(),
    n_max = max(n_dup)
  ) %>% 
  ungroup() %>% 
  count(VISIT, SATERM, TIMING, SACODE) %>% print(n = Inf)

# SADUR is only defined when SAOCCUR is missing
# SASTDY is actually more complete, often present when SADUR is missing, and where SADUR is present, is the same as SADUR. 
# There remain 31 FEVER entries (All PATNA patients) with FEVER entries but no timing of fever data
# VZUYLH_MUZAFFARPUR_1034 - SASTDY should be -40 (only USUBJID where SADUR is populated and SASTDY/SASTDTC not populated)

SA_merge %>% count(SATERM, !is.na(SASTDY), !is.na(SADUR))
SA_merge %>% 
  filter(
    SATERM=="FEVER", is.na(SASTDTC)
  ) # %>% View()

# fever
SA_merge <- SA_merge %>% 
  full_join(DM %>% select(USUBJID, RFSTDTC)) %>% 
  mutate(t_diff = ymd(SASTDTC) - dmy(RFSTDTC)) %>% 
  mutate(
    SAOCCUR = case_when(
      SATERM == "FEVER" & USUBJID != "ZUYLH_MUZAFFARPUR_1034" ~ as.character(t_diff),
      SATERM == "ZUYLH_MUZAFFARPUR_1034" ~ "-40",
      .default = SAOCCUR
  ))  %>% 
  mutate(
    SAOCCUR = ifelse(SAOCCUR == "1", "0", SAOCCUR)
  )

SA_wider <- SA_merge %>% pivot_wider(
    id_cols = USUBJID,
    names_from = c(TIMING, SACODE),
    names_glue = "SA_{TIMING}_{SACODE}",
    values_from = SAOCCUR,
  ) %>% 
  mutate(across(-c(USUBJID, SA_HX_FEV_DUR), ~case_when(.x == "Y" ~ TRUE, .x == "N" ~ FALSE, .default = NA)))

SA_wider #%>% View()

# merge
VZUYLH <- VZUYLH %>% 
  full_join(SA_wider)

# MB ----

MB %>% colnames()
dim(MB)

# Splenic aspirates were routinely performed at SCREENING (all positive), 
# and completion of treatment (DAY 15 or DAY 31). Only 6/444 patients had 
# positive aspirate at DAY 15, and no patients, 0/131 had positive aspirates
# at DAY 31. 

# Unable to identify relapses here

MB %>% 
  count(MBTESTCD, VISIT, MBORRES, MBLOC, MBMODIFY, MBTSTDTL, MBSTAT, MBREASND) # %>% View() 

MB %>% 
  # filter(is.na(MBSTAT)) %>% 
  group_by(MBTEST, VISIT, MBLOC, MBTSTDTL, MBSTAT, MBREASND) %>% 
  mutate(
    n = n(),
    n_distinct = n_distinct(USUBJID),
    n_test = n == n_distinct)  %>% 
  count(MBTEST, VISIT, MBLOC, MBTSTDTL, MBSTAT, MBREASND, n, n_distinct, n_test) # %>% View()

# all VISIT = NA entires have no results (MBSTAT is "NOT DONE")
MB %>% 
  filter(is.na(MBSTAT)) %>% 
  count(MBTEST, VISIT, MBLOC, MBTSTDTL)

# pivot and explore
# all bone marrows were negative (ignore for now)
MB %>% count(MBLOC, VISIT, MBMODIFY,MBORRES,MBORRESU,MBSTRESC, MBDY)
MB %>% filter(!is.na(MBDY)) # %>% View()

# pivot and merge with OUT
MB_wider <- MB %>% 
  filter(
    VISIT %in% c("Screening"),
    is.na(MBSTAT),
    MBLOC == "SPLEEN") %>% 
  mutate(
    VISIT = "BL") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}_{MBTESTCD}"
  ) %>% 
  rename(MB_BL_LSHMANIA_SPLEEN = MB_BL_LSHMANIA)

MB_wider %>% names()  

VZUYLH <- VZUYLH %>% 
  full_join(MB_wider)

VZUYLH %>% names()

# LB
# omitted LB in first cleaning attempt!
# for day 45 outcomes, there are multiple duplicates even after removing urine samples
LB %>% names()
LB %>% filter(is.na(LBSPEC), VISIT %in% c("Screening", "Day 45"), LBDY > 0) %>% count_dup(VISIT, LBTESTCD) %>% print(n = Inf) # %>% View()
LB %>% count(LBSTAT)

# remove likely incorrect values
LB_clean <- LB %>% filter(
  is.na(LBSPEC), 
  is.na(LBSTAT), 
  VISIT %in% c("Screening", "Day 45"),
  LBDY != 17,
  LBDY > -300,
  !(USUBJID == "VZUYLH_PATNA_2005" & LBSEQ == 109),
  !(USUBJID == "VZUYLH_PATNA_2019" & LBSEQ == 111),
  !(USUBJID == "VZUYLH_PATNA_2026" & LBSEQ == 110),
  !(USUBJID == "VZUYLH_MUZAFFARPUR_1137" & LBSEQ == 122),
  !(USUBJID == "VZUYLH_MUZAFFARPUR_1137" & LBSEQ == 123),
  !(USUBJID == "VZUYLH_MUZAFFARPUR_1075" & LBSEQ == 126),
  !(USUBJID == "VZUYLH_MUZAFFARPUR_1005" & LBSEQ == 16),
  !(USUBJID == "VZUYLH_MUZAFFARPUR_1106" & LBSEQ == 25),
  !(USUBJID == "VZUYLH_MUZAFFARPUR_1126" & LBSEQ == 33),
  )

LB_merge1 <- 
  LB_clean %>% 
  filter(LBTESTCD != "HCG") %>% 
  mutate(
    VISIT = ifelse(VISIT == "Screening", "BL", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )  
  
LB_merge2 <- 
  LB_clean %>% 
  filter(LBTESTCD == "HCG") %>% 
  mutate(
    VISIT = ifelse(VISIT == "Screening", "BL", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBORRES,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_merge2 <- LB_merge2 %>% 
  mutate(LB_BL_HCG = ifelse(LB_BL_HCG == "Negative", FALSE, NA))

LB_merge <- LB_merge2 %>% full_join(LB_merge1) 
LB_merge #%>% View()

VZUYLH <- VZUYLH %>% full_join(LB_merge)

# MP -----

# SPLEEN length and width
# uncertain exactly how these are measured

MP # %>% View()
colnames(MP)
dim(MP)

# results as per LB and SA
MP %>%
  count(MPTESTCD, MPTEST, MPLOC, VISIT)

# ensure can pivot (for screening, day 45) --> yes, USUBJID is unique within MPTESTCD and VISIT
MP %>%
  group_by(USUBJID, MPTESTCD, VISIT) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  count(num)

MP_wider <- MP %>%
  filter(VISIT %in% c("Screening", "Day 45")) %>%
  mutate(
    VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC, MPTESTCD),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_{MPTESTCD}"
  ) 

MP_wider %>% colnames()

MP_wider %>%
  var_sum(MP_BL_SPLEEN_LENGTH)
MP_wider %>% 
  var_sum(MP_IC_SPLEEN_LENGTH)

MP_wider %>%
  var_sum(MP_BL_SPLEEN_WIDTH)
MP_wider %>%
  var_sum(MP_IC_SPLEEN_WIDTH)

VZUYLH <- VZUYLH %>%
  full_join(MP_wider)
VZUYLH %>% names()

# VS -----

# strip empty columns
VS # %>% View()
dim(VS)
colnames(VS)

# we have: BMI, DBP, HEGHT, HR, SBP, TEMP, WEIGHT
# Height, weight (and BMI) available at screening
# HR, SBP/DBP, TEMP are available at screening (n = 634), day 45 (625), (and 35 at 6 months as per other variables)
# notably there is no repeat weight at day 45 / 6 months

VS %>% 
  count(VISIT, VSTESTCD, VSTEST) %>% print(n = Inf)

# Where VISIT is NA, there are 25 unique USUBIDs with DBP, SBP, HR and TEMP readings;
VS %>%
  group_by(VSTESTCD, VSTEST, VSLOC, VISIT) %>%
  mutate(
    n = n(),
    n_distinct = n_distinct(USUBJID),
    unique = n == n_distinct
  ) %>%
  count(VSTESTCD, VSTEST, VSLOC, VISIT, n, n_distinct, unique) # %>% View()

# none of the missing VISIT entries seem to be 45 days or 6 month readings
# all MUZAFFARPUR patients
VS %>%
  filter(is.na(VISIT)) %>%
  count(USUBJID, VSTESTCD, VSTEST, VISITNUM, EPOCH, VSDY)

# pivot the screening and 45 day results, and join
VS_wider <- VS %>%
  filter(
    VISIT %in% c("Screening", "Day 45")
  ) %>%
  mutate(
    VISIT = ifelse(VISIT == "Screening", "BL", "IC"),
    VSTESTCD = ifelse(VSTESTCD == "HR", "PULSE", VSTESTCD)) %>%  # HR previously called PULSE - change to PULSER for consistency
  pivot_wider(
    id_cols = USUBJID,
    values_from = VSSTRESC,
    names_from = c(VISIT, VSTESTCD),
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  ) %>% relocate(sort(names(.)))
VS_wider %>% names()

VZUYLH <- VZUYLH %>% 
  full_join(VS_wider)
VZUYLH %>% names()

# Other domains ----

# adverse events - unable to find any data identifying 6 month outcomes here
CC # %>% View()
dim(CC)
colnames(CC)

CC %>% 
  count(VISIT,CCTERM) %>% 
  arrange(VISIT, desc(n)) %>%
  print(n = Inf)

CC %>% filter(VISIT == "Screening") %>% count(CCTERM, CCSTDY, CCDUR) %>% print(n = Inf)

# let's just check the fever duration is the same as that in SA domain -> yes it is the same data
CC %>% filter(VISIT == "Screening" & CCTERM == "FEVER") %>% select(USUBJID, CCDUR, CCSTDY) %>% 
  full_join(VZUYLH) %>% count(CCSTDY, SA_HX_FEV_DUR) %>% print(n =Inf)

# physical examination
# almost all at screening
# only seem to be 2 abnormal entries (2 abnormal abdominal examinations...)

PE #%>% View()
PE %>% colnames()
PE %>% 
  filter(is.na(PESTAT)) %>% 
  count(PETESTCD, PETEST, PESTAT, PEREASND, VISIT, PEORRES, PESTRESC) # %>% View()
PE %>% count(PEORRES)
PE %>% count(PESTRESC)

# negative pregnancy tests

RP %>% colnames()
RP %>% dim()
RP %>% 
  count(RPTESTCD, RPTEST, RPORRES, RPSTRESC, VISIT, RPDY)

RP_merge <- RP %>% 
  select(USUBJID, RPORRES) %>% 
  mutate(RP_PREG = ifelse(RPORRES == "Negative", FALSE, NA)) %>% 
  filter(!is.na(RP_PREG)) %>% select(-RPORRES)

RP_merge %>% count(RP_PREG)

VZUYLH <- VZUYLH %>% 
  full_join(RP_merge)
VZUYLH %>% names()

save(VZUYLH, file = "Study/Data/VZUYLH.RData")
