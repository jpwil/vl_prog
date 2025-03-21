##########
# VLAULV #
##########

##############

# PMID: 20147716
# Protocol: NCT00628719

# Sundar S, Chakravarty J, Agarwal D, Rai M, Murray HW. 
# Single-dose liposomal amphotericin B for visceral leishmaniasis in India. 
# N Engl J Med. 2010 Feb 11;362(6):504-12. doi: 10.1056/NEJMoa0903627. PMID: 20147716.

# "Two patients in the conventional-therapy group were removed from the study because of severe
# diarrhea in one and presumed bacterial pneumonia in the other; both cases were considered
# treatment failures in the intention-to-treat analysis (Fig. 1)."

# Initial cure: 304/304 in LAMB arm, 106/108 in AMB arm
# Relapse: 13/304 in LAMB arm, 2/108 in AMB arm (denominator includes the 2 patients who were
# excluded during treatment)
# Deaths: 0

# 80 patients underwent previous VL treatment (none of these patients relapsed in the current study)

##############

# Dataset: 412 USUBJIDs (same as publication)
# But n = 109 in AMB, 303 in LAMB (one extra patient in AMB group)
# Only have 6 month outcome data for 284 patients / and approx 258 LB, MP, MB @ 6 months
# Unable to identify the two patients who failed initial cure

rm(list = ls())
source("definitions.R")
load_domains("VLAULV")

mg <- ld_missingness("VLAULV")
mg %>%
  count_na() %>%
  print(width = Inf)

# only study start and end date is populated in TS Domain
TS %>% colnames()
TS # %>% View()


# DM ----
DM %>% colnames()
DM %>% count(ARM)
DM %>% count(COUNTRY, SITEID)
DM %>%
  count(USUBJID) %>%
  count(n)
DM %>% count(ARM, SEX)
DM %>%
  group_by(ARM) %>%
  summarise(
    n = n(),
    age_mean = mean(AGE),
    age_min = min(AGE),
    age_max = max(AGE),
    age_per_under_12_1 = 100 * sum(AGE <= 12) / n(), # not reproducible
    age_per_under_12_2 = 100 * sum(AGE < 12) / n()
  )

VLAULV <- DM %>%
  mutate(
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID,
    DM_ARM = ARMCD
  ) %>%
  select(USUBJID, RFSTDTC, starts_with("DM_"))


# TV -----
TV %>% colnames()
TV # View()

# TI -----
TI # %>% View()

# RS ----
RS %>% colnames()

# all RS outcomes are 6 month
# 269 are CURE, 14 are RELAPSE (should be 15 in total)
# one entry for each USUBJID (but only 283 entries)
# no VL hx here
RS %>%
  count(USUBJID) %>%
  count(n)
RS %>% count(RSORRES)
RS %>%
  count(RSTESTCD, RSTEST, VISIT, EPOCH, RSDY, RSORRES) %>%
  print(n = Inf)
RS %>%
  count(RSORRES, RSSTRESC, RSDY) %>%
  print(n = Inf)

RS %>%
  pull(USUBJID) %>%
  sort()


# DS ----
DS %>% colnames()

# 284 entries (each is a different USUBJID)
DS %>%
  count(USUBJID) %>%
  count(n)
DS %>% count(DSTERM, DSDECOD, DSDY)
DS %>%
  pull(USUBJID) %>%
  sort()

# OUTCOME SUMMARY ----
OUT <-
  DS %>% select(USUBJID, DSTERM, DSDY) %>% 
  full_join(RS %>% select(USUBJID, RSORRES, RSDY)) %>% full_join(DM %>% select(USUBJID))

# DS and RS essentially contain the same information, except
# DS also includes a LAMA (lost to follow-up) patient occurring at day 15
# let's assume that this case is an IC failure (despite stating LAMA), given publication stating two patients excluded

OUT %>%
  count(RSORRES, DSTERM, RSDY, DSDY) # %>% View()

OUT_merge <- OUT %>% 
  mutate(
    OUT_XX_OTHER = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_DC_DEATH = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = ifelse(DSTERM == "LAMA" & !is.na(DSTERM), TRUE, FALSE),
    OUTC_IC_OTHER = "1 patient with DSTERM of LAMA at DSDY 15",
    OUT_DC_RELAPSE = ifelse(DSTERM == "RELAPSE" & !is.na(DSTERM), TRUE, FALSE),
    OUT_DC_OTHER = FALSE,
    OUT_NA = ifelse(is.na(DSTERM), TRUE, FALSE),
    OUT_IC = case_when(
      DSTERM %in% c("CURE", "RELAPSE") & !is.na(DSTERM) ~ TRUE,
      .default = FALSE
    ),
    OUT_DC = case_when(
      DSTERM == "CURE" ~ TRUE,
      DSTERM == "RELAPSE" ~ FALSE,
      .default = FALSE
  )) %>% 
  select(USUBJID, starts_with("OUT"))

# check outcomes - happy with these
OUT_merge %>% count(across(contains("OUT_")))  %>% print(width = Inf) # all good
OUT_merge %>% count(OUT_DC_RELAPSE)

VLAULV <- VLAULV %>%
  full_join(OUT_merge)

VLAULV %>% names()
VLAULV %>% count()

# PT ----
PT # %>% View()

# IN ----
IN %>% colnames()
IN %>%
  count(INTRT, INCAT, EPOCH, VISITNUM) # %>% View()

# each USUBJID is unique within INCAT (MEDICAL HISTORY or NA)
IN %>%
  group_by(INCAT, USUBJID) %>%
  mutate(n_dup = row_number()) %>%
  ungroup() %>%
  count(n_dup)

# these are the 80 patients with a history of previous VL treatment
# also have details on how long they were treated for and the treatment regimen, but not when
IN %>% count(INCAT)
IN %>% filter(INCAT == "MEDICAL HISTORY") # %>% View()

IN_merge <- IN %>%
  filter(INCAT == "MEDICAL HISTORY") %>%
  mutate(IN_MH_VL = TRUE) %>%
  select(USUBJID, IN_MH_VL)
VLAULV <- VLAULV %>% full_join(IN_merge)

VLAULV %>%
  select(-USUBJID, -RFSTDTC, -DM_SEX, -DM_AGE) %>%
  count(across(everything())) # %>% View()

# SA ----

# adverse events
SA %>% names()
SA %>%
  count(USUBJID) %>%
  count(n)
SA %>% count(SATERM, SACAT, SAPATT, EPOCH)

# MEDICAL HISTORY data for COUGH, FEVER, and RIGOR
# The remaining entries relate to symptoms during the study (range of SADY)

# No SAPRESP or SAOCCUR variables - i.e. there is no explicit entry stating symptoms did not 
# happen, therefore there are no 'FALSE' entries for COUGH and RIGOR

# history of cough: 35
# history of fever: 412 (with duration in SADUR - complete for all patients)
# history of rigor: 28

SA %>%
  filter(SACAT == "MEDICAL HISTORY") %>%
  count(SATERM, USUBJID) %>%
  count(n)
SA %>%
  count(EPOCH, !is.na(SADUR), SATERM, SACAT, SASTRF)
SA %>%
  count(EPOCH, !is.na(SADUR), SATERM, SACAT, SADY) %>%
  print(n = Inf)
SA %>%
  filter(SACAT == "MEDICAL HISTORY") # %>% View()

SA_fever <- SA %>% 
  filter(SACAT == "MEDICAL HISTORY") %>% 
  mutate(
    SACODE = case_when(
      SATERM == "FEVER" ~ "FEV_DUR",
      SATERM == "RIGER" ~ "RGR",
      SATERM == "COUGH" ~ "CGH",
      .default = SATERM
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SACODE,
    values_from = SADUR,
    names_glue = "SA_HX_{SACODE}"
  ) %>% 
  select(USUBJID, SA_HX_FEV_DUR)

SA_other <- SA %>% 
  filter(SACAT == "MEDICAL HISTORY") %>% 
  mutate(
    SACODE = case_when(
      SATERM == "FEVER" ~ "FEV_DUR",
      SATERM == "RIGER" ~ "RGR",
      SATERM == "COUGH" ~ "CGH",
      .default = SATERM
    ),
    SAOCCUR = TRUE
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SACODE,
    values_from = SAOCCUR,
    names_glue = "SA_HX_{SACODE}"
  ) %>% 
  select(-SA_HX_FEV_DUR)  
  
SA_other %>% names()

SA_other %>% count(SA_HX_CGH, SA_HX_RGR)
SA_fever %>% count(SA_HX_FEV_DUR) %>% print(n = 50)

SA_merge <- full_join(SA_other, SA_fever)

VLAULV <- VLAULV %>% full_join(SA_merge)

# LB ----

LB %>% colnames()

# LBORNRLO and LBORNRHI are just upper and lower limits of normal (not defined in IDDO WIKI)
LB %>% count(LBTESTCD, LBORNRLO, LBORNRHI) %>% print(n = Inf)

LB %>% count(VISIT, LBTEST, LBTESTCD, LBSPEC, LBSTRTPT, LBSTTPT) %>% print(n = Inf)
LB %>% count(VISIT)

# "SCREENING/DAY 00" -> BL
# "FOURTH WEEK/DAY 30" -> IC

LB_merge <- LB %>% 
  mutate(
    VISIT1 = ifelse(VISIT == "SCREENING/DAY 00", "BL", VISIT),
    VISIT1 = ifelse(VISIT == "FOURTH WEEK/DAY 30", "IC", VISIT1)
  ) %>% 
  filter(VISIT1 %in% c("BL", "IC")) 

# PROT and PT - where are the results? -> in LBORRES. 
LB_merge %>% filter(LBTESTCD %in% c("PROT", "PT")) #%>% View()

LB_merge %>% str()
LB_pivot1 <- LB_merge %>% 
  filter(!LBTESTCD %in% c("PROT", "PT")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT1", "LBTESTCD"),
    names_glue = "LB_{VISIT1}_{LBTESTCD}",
    values_from = LBSTRESN
  )  
  
LB_pivot2 <- LB_merge %>% 
  filter(LBTESTCD %in% c("PROT", "PT")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT1", "LBTESTCD"),
    names_glue = "LB_{VISIT1}_{LBTESTCD}",
    values_from = LBORRES
  ) 

LB_pivot2 %>% names()
LB_pivot2 <- LB_pivot2 %>% mutate(across(-USUBJID, ~as.numeric(.x)))

LB_pivot <- LB_pivot1 %>% full_join(LB_pivot2)
LB_pivot #%>% View()

VLAULV <- VLAULV %>% full_join(LB_pivot)
VLAULV %>% names()

# MB ----

# Screening PLSMDM, HIV and splenic aspirates for all (or almost all)
# Also have week 4 splenc aspirates for (almost) all

MB %>% names()

MB %>% count(MBTESTCD, MBTEST, MBLOC, MBSPEC, MBMETHOD, MBORRES, VISIT, VISITNUM, EPOCH)# %>% View()
MB %>% count(MBTESTCD, VISIT, MBORRES, MBSTRESC)

MB_merge <- MB %>% 
  filter(!is.na(VISIT)) %>% 
  mutate(VISIT = ifelse(VISIT == "FOURTH WEEK/DAY 30", "IC", "BL")) %>% 
  mutate(
    MBORRES = case_when(
      MBORRES == "NEGATIVE" ~ "FALSE",
      MBORRES == "Negative" ~ "FALSE",
      MBORRES == "POSITIVE" ~ "TRUE",
      .default = MBORRES
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT", "MBTESTCD"),
    names_glue = "MB_{VISIT}_{MBTESTCD}",
    values_from = MBORRES
  )  %>% 
  mutate(
    across(
      c(MB_BL_HIV, MB_BL_PLSMDM, MB_IC_LSHMANIA, MB_BL_LSHMRK39),
      ~ as.logical(.x)
    )
  ) %>% select(-MB_IC_LSHMANIA) %>% 
  rename(MB_BL_LSHMANIA_SPLEEN = MB_BL_LSHMANIA) # only interested in baseline MB

MB_merge %>% count(across(!USUBJID))
VLAULV <- VLAULV %>% full_join(MB_merge)
VLAULV %>% names()

# MP ----

# liver and spleen lengths are (almost) complete for baseline and initial cure time points
MP %>% names()
MP %>%
  count(VISIT, MPTESTCD, MPTEST, MPLOC)
MP %>% count(VISIT)

# the "ONE MONTH" values are duplicate USUBJIDs from "FOURTH WEEK/DAY 30". Drop them.

MP_merge <- MP %>% 
  filter(VISIT %in% c("TREATMENT DAY 1", "FOURTH WEEK/DAY 30")) %>% 
  mutate(VISIT = ifelse(VISIT == "TREATMENT DAY 1", "BL", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT", "MPTESTCD", "MPLOC"),
    names_glue = "MP_{VISIT}_{MPLOC}_{MPTESTCD}",
    values_from = MPSTRESN
  ) 

VLAULV <- VLAULV %>% 
  full_join(MP_merge)

# VS ----

# temperature and weight data (almost) complete for baseline and initial cure timepoints
# unlike previous studies, we have no duration of fever prior to admission
VS %>% colnames()
VS %>%
  count(VISIT, VSTESTCD, VSTEST)

VS_merge <- VS %>% 
  filter(VISIT %in% c("SCREENING/DAY 00", "TREATMENT DAY 1", "FOURTH WEEK/DAY 30")) %>%
  mutate(
    VISIT = ifelse(
      VISIT == "SCREENING/DAY 00", "BL", ifelse(
        VISIT == "TREATMENT DAY 1", "TD", "IC")
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT", "VSTESTCD"),
    names_glue = "VS_{VISIT}_{VSTESTCD}",
    values_from = VSSTRESN
  ) # %>% mutate(test = VS_TD_WEIGHT - VS_BL_WEIGHT) %>% count(test) # 407/412 weights bewteen day 0 and day 1 are the same

VS_merge <- VS_merge %>% 
  rename(VS_BL_TEMP = VS_TD_TEMP) %>% 
  select(-VS_TD_WEIGHT) 

VLAULV <- VLAULV %>% full_join(VS_merge)
VLAULV %>% names()

# EXPLORE ----
# QS, RP and SC domains remains

QS %>% colnames() # KPSS (possibly useful)
QS %>% count(QSTEST, QSCAT, VISITNUM, EPOCH)

RP %>% colnames() # pregnancy tests for 73 USUBJIDs (not useful - all negative)
RP %>% count(RPTEST, VISIT, EPOCH, RPORRES) # negative for 73 (but 161 female patients)
RP %>% count(USUBJID) %>% count(n)

RP_merge <- RP %>% 
  mutate(
    RP_PREG = ifelse(RPORRES == "NEGATIVE", FALSE, NA)
  ) %>% 
  select(USUBJID, RP_PREG)

VLAULV <- VLAULV %>% full_join(RP_merge)
VLAULV %>% names()
VLAULV %>% count(DM_SEX, RP_PREG)


SC %>% colnames() # Admin level 3 address (not useful)
SC %>% count(SCTESTCD, SCTEST, SCORRES, VISIT, EPOCH) # %>% View()
SC # %>% View()

save(VLAULV, file = "Study/Data/VLAULV.RData")
