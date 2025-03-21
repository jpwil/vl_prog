##########
# VYDSGR #
##########

rm(list = ls())
source("definitions.R")
load_domains("VYDSGR")
mg <- ld_missingness("VYDSGR")
mg %>% count(across(-USUBJID)) %>% print()

# Study: Sundar et al 2012, "Efficacy of miltefosine in the treatment of visceral leishmaniasis in India after a decade of use"
# Oral miltefosine, open label, KAMRC study. Reported clearly. See also Ostyn 2014 - this study specifically looks at relapse 
# predictors using data from this study and other studies. Presents a good summary of previous relapse data and miltefosine.
# Identifies young age and male gender as risk factors for relapse. 
# 39 relapse, 5 deaths
# All patients had positive smear with demonstrable parasite (apparently)

# Gemma - confirm, LAMA = leave against medical advice? - YES

# RECRUITED: 567

### 9 with SAE (including 1 x death)
### 1 death during treatment (2 deaths in total)
### 4 withdrew consent

# ACHIEVED INIITIAL CURE: 553

### 39 patients with relapse (including 1 x death)
### 2 further deaths (3 deaths in total)

# ACHIEVED FINAL CURE: 512

# DM -----

# all patients from KAMRC
DM %>% colnames()
DM %>% count(SITEID)
DM %>% var_sum(AGE) # approximately matches Sundar 2012 
DM %>% count(SEX) # roughly matches Sundar 2012

DM %>% colnames()
DM %>% count(DTHFL) # 5 deaths consistent with study (no DTC/DY)
DM %>% count(ARM, ARMCD)
DM %>% count(COUNTRY)
DM %>% count(DMDY)

VYDSGR <- 
  DM %>% 
  mutate(
    DM_AGE = AGE,
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_SITE = SITEID
  ) %>% 
  select(
    USUBJID,
    RFSTDTC,
    starts_with("DM_")
  )

VYDSGR %>% names()

# TV ----

# clearly labelled visits: days 0, 8, 15, 22, 29, and 6 months.
TV %>% colnames()
TV %>% count(VISITNUM, VISIT, VISITDY, TVSTRL)  # %>% View()

# TI ----

# from Sundar 2012
TI %>% colnames()
TI %>% count(IETESTCD, IETEST, IECAT) # %>% View()

# OUTCOMES ----

# RS
# VISIT == Day 0 always RSSCAT == MEDICAL HISTORY
# is.na(VISIT) always RSSCAT == ADDITIONAL OUTCOMES PROVIDED
# RS entries are unique within VISIT, RSTESTCD

DS %>% names()
DS %>% count(VISIT, DSTERM)

RS %>% names()
RS %>% count(VISIT, RSTESTCD, RSORRES)
RS %>% group_by(USUBJID, RSTESTCD, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, RSTESTCD, n_dup)

# create pivot variable for RS
RS_merge <- RS %>% 
  mutate(
    PIVOT = case_when(
      VISIT == "6 Months" ~ "M6",
      VISIT == "Day 0" ~ "D0",
      VISIT == "Day 29" ~ "D29",
      is.na(VISIT) & RSTESTCD == "OVRLRESP" ~ "NA_OR",
      is.na(VISIT) & RSTESTCD == "TOC" ~ "NA_TOC",
      .default = "error"
     )
  )

# all RSSTRF are 'BEFORE' for medical history 
# only a few M6 and NA RS entries have RSDTC and RSDY data available
# The one NA visit RS event with no RSDTC/RSDY data is RSEVINTX == "ANY TIME DURING RESCUE TREATMENT"
RS_merge %>% group_by(USUBJID, RSTESTCD, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(PIVOT, VISIT, RSTESTCD, !is.na(RSDY), RSSTRF, RSEVINTX) #%>% View()

RS_pivot <- RS_merge %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = PIVOT,
    values_from = c("RSORRES", "RSDY")
  ) %>% 
  select(-c(RSDY_D29, RSDY_D0)) %>% 
  full_join(DS %>% select(USUBJID, DSTERM, DSSTDY))

RS_pivot #%>% View()
RS_pivot %>% names()
RS_pivot %>% count(across(-c(USUBJID, DSSTDY))) #%>% View()


# create outcomes (checked and double checked)
OUT <- RS_pivot %>% 
  mutate(
    OUT_XX_OTHER = case_when(
      DSTERM == "LAMA" & !is.na(DSTERM) & is.na(RSORRES_D29) ~ TRUE, # one patient LAMA as DSTERM, but all other outcome variables are NA
      .default = FALSE
    ),
    OUTC_XX_OTHER = "1 patient described as 'LAMA' in DSTERM, but all other outcome variables are NA",
    OUT_XX_DEATH = FALSE,
    OUT_IC_OTHER = case_when(
      RSORRES_D29 == "ONGOING" & !is.na(RSORRES_D29) ~ TRUE, # 1 patient with 'ONGOING' in RSORRES_D29 and nothing else to guide interpretation
      RSORRES_D29 == "INCOMPLETE" & !is.na(RSORRES_D29) & DSTERM == "LAMA" & !is.na(DSTERM) ~ TRUE, # 5 patients with INCOMPLETE in RSORRES_D29 and LAMA in DSTERM
      RSORRES_D29 == "INCOMPLETE" & !is.na(RSORRES_D29) & RSORRES_M6 == "CURED" & !is.na(RSORRES_M6) ~ TRUE, # 1 patient with INCOMPLETE in RSORRES_D29 and CURED in RSORRES_M6
      .default = FALSE
    ),
    OUTC_IC_OTHER = "7 patients: 1 with 'ONGOING' as 29 day outcome, and no other information, 5 with 'INCOMPLETE' in 29 day outcome and 'LAMA' in DSTERM (left against medical advice), and 1 with 'INCOMPLETE' in 29 day outcome, but 'CURED' in 6 month outcome",
    OUT_IC_DRUG = case_when(          
      RSORRES_NA_OR == "DRUG CHANGE" & !is.na(RSORRES_NA_OR) ~ TRUE,
      RSORRES_D29 == "DRUG CHANGE" & !is.na(RSORRES_D29) ~ TRUE,
      RSORRES_D0 == "DRUG CHANGE" & !is.na(RSORRES_D0) ~ TRUE, # not sure why the Day 0, RSSCAT of MEDICAL HISTORY has 'DRUG CHANGE' in it
      .default = FALSE
    ),
    OUTC_IC_DRUG = "These are the 5 patients where 'DRUG CHANGE' described somewhere (despite some of these patients described as 'complete recovery' in other outcomes)",
    OUT_IC_DEATH = case_when(
      DSTERM == "DEATH" & !is.na(DSTERM) & (is.na(RSORRES_D29) | RSORRES_D29 == "INCOMPLETE") ~ TRUE, # can deduce from dataset and publication that these 2 deaths are during treatment
      .default = FALSE
    ),
    OUT_DC_DEATH = case_when(
      DSTERM == "DEATH" & !is.na(DSTERM) & RSORRES_D29 == "COMPLET RECOVERY" & !is.na(RSORRES_D29) ~ TRUE,
      .default = FALSE
    ),
    OUT_DC_RELAPSE = case_when(
      DSTERM == "RELAPSED" & !is.na(DSTERM) ~ TRUE,
      .default = FALSE
    ),
    OUT_NA = ifelse(is.na(RSORRES_D29) & is.na(DSTERM), TRUE, FALSE), # only one patient where all outcome variables are NA
    OUT_IC = case_when(
      RSORRES_D29 %in% c("COMPLET RECOVERY", "COMPLETE RECOVERY", "INITIAL CURE") & !is.na(RSORRES_D29) ~ TRUE,
      .default = FALSE
    ),
    OUT_IC = ifelse(OUT_IC_DEATH | OUT_IC_DRUG, FALSE, OUT_IC),
    OUT_DC = case_when(
      RSORRES_M6 == "CURED" & !is.na(RSORRES_M6) ~ TRUE,
      RSORRES_M6 == "RELAPSED" & !is.na(RSORRES_M6) ~ FALSE,
      .default = FALSE
    ),
    OUT_DC = ifelse(OUT_IC_OTHER | OUT_IC_DRUG, FALSE, OUT_DC),
    OUT_DC_OTHER = FALSE
  )

OUT %>% mutate(across(everything(), ~ as.character(.x))) %>% count(across(contains("OUT_"))) %>% print(width = 400) #%>% View()

VYDSGR <- VYDSGR %>% full_join(OUT %>% select(USUBJID, starts_with("OUT_")))

# PT ----
colnames(PT) 

PT %>% count(USUBJID) %>% count(n) # can be once daily or twice daily

PT %>% count(
  DOMAIN, PTGRPID, PTTRT, PTDOSE, PTDOSU, PTDOSFRM, PTDOSTOT, PTDOSRGM, PTROUTE, PTPSTRG, PTPSTRGU
) # %>% View()

# IN -----

colnames(IN)
IN %>% count(INCAT, INSCAT, INTRT, INPRESP, INOCCUR, INSTDY, INENDY) #%>% View()
IN %>% count(INCAT, INSCAT)
IN %>% count(INCAT, INSCAT, USUBJID) %>% count(n)

# 35 patients have RESCUE TREATMENT INSCAT entry for each INCAT
# these patients have AMPHOTERICIN B rescue treatment (i.e. initially had MILTEFOSINE then changed / rescue treatment)
IN %>% filter(INSCAT == "RESCUE TREATMENT")  %>% 
  arrange(USUBJID, INTRT) #%>% View()

# SA ----

# Baseline (VISIT == "Day 0"):
# Cough, History of Fever*, History of Kala-Azar, Loss of appetite, Loss of weight, Rigor, Vomiting, Weakness, 

SA %>% names()
SA %>% count(VISIT, SATERM, SACAT, SASCAT, !is.na(SADUR)) #%>% View()
SA %>% filter(SATERM == "History of Fever") # SARPOC contains duration of fever

# ensure unique before pivot - yes (for VISIT == "Day 0")
SA %>% group_by(USUBJID, VISIT, SATERM) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, SATERM, n_dup) %>% print(n = 100)

SA %>% count(SAOCCUR)
SA_merge <- SA %>% 
  filter(VISIT == "Day 0") %>% 
  mutate(
    SAOCCUR = case_when(
      SAOCCUR == "N" ~ FALSE,
      SAOCCUR == "Y" ~ TRUE,
      SAOCCUR == "U" ~ NA,
      is.na(SAOCCUR) ~ NA
    )
  ) %>% 
  mutate(VISIT = "HX") %>% 
  mutate(
    SACODE = case_when(
      SATERM == "Cough"                 ~ "CGH",
      SATERM == "History of Fever"      ~ "FEV_DUR",
      SATERM == "History of Kala-Azar"  ~ "VL",
      SATERM == "Loss of appetite"      ~ "APP",
      SATERM == "Loss of weight"        ~ "WL",
      SATERM == "Rigor"                 ~ "RGR",
      SATERM == "Vomiting"              ~ "VOM",
      SATERM == "Weakness"              ~ "WEAK"
    )
  )


SA_pivot <- SA_merge %>% 
  filter(!is.na(SACODE)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT", "SACODE"),
    values_from = SAOCCUR,
    names_glue = "SA_{VISIT}_{SACODE}"
  )

SA_merge_for_joining_fever <- SA_merge %>% filter(VISIT == "HX" & SACODE == "FEV_DUR") %>% select(USUBJID, SARPOC)

VYDSGR <- SA_pivot %>% 
  full_join(SA_merge_for_joining_fever) %>%
  mutate(SA_HX_FEV_DUR = SARPOC) %>% 
  select(-SARPOC) %>% 
  full_join(VYDSGR) # %>% View()

# LB ----

LB %>% names()
LB %>% count(VISIT, LBCAT, LBTESTCD, LBTEST, ) %>% 
  arrange(VISIT) #>% View()

# only interested in Day 0 and Day 29 lab results
# only 3 patients had K and Na

LB_merge <- LB %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = "error"
    )
  ) %>% 
  filter(
    VISIT %in% c("BL", "IC"),
    is.na(LBCAT)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    values_from = LBSTRESN,
    names_from = c("VISIT", "LBTESTCD"),
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_merge %>% relocate(sort(colnames(.))) %>% names()
LB_merge #%>% View()

# need to sort out HCG tests
# double check basophils

LB_merge %>% count(LB_BL_BASOLE) # all 0 
LB_merge %>% count(LB_IC_BASOLE) # all 0 except one USUBJID it is 1!
LB_merge %>% count(LB_BL_HCG)
LB_merge %>% colnames()
LB %>% colnames()

# all Day 0 HCGs are unique
# merge with actual HCG results from LBORRES and with previous results
LB_merge <- LB %>% filter(LBTESTCD == "HCG" & VISIT == "Day 0") %>% 
  mutate(HCG = ifelse(LBORRES == "NEGATIVE", FALSE, NA)) %>% 
  select(USUBJID, HCG) %>% 
  full_join(LB_merge) %>% 
  mutate(LB_BL_HCG = HCG) %>% 
  select(-HCG) %>% 
  relocate(sort(names(.))) 

LB_merge %>% str()
LB_merge %>% count(LB_BL_HCG)

VYDSGR <- VYDSGR %>% full_join(LB_merge) 

# MB ----
# MB domain is not available, 

# Females and males aged 6–70 years were eligible if they had
# symptoms and signs suggestive of VL (ie, fever with chills, rigor,
# and splenomegaly) with Leishmania parasites demonstrable in
# splenic-aspirate smears. Splenic-aspirate smears were each read
# by 2 observers in a blinded fashion. 

# MP ----
# We have liver and spleen lengths
MP %>% colnames()
MP %>% count(VISIT, MPTESTCD, MPTEST, MPCAT, MPLOC)

MP %>% group_by(USUBJID, VISIT, MPLOC) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, MPLOC, MPORRES, n_dup) %>% print(n = 100)

# pivot
MP_merge <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = "Other"
    )) %>% 
  filter(VISIT %in% c("BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT", "MPLOC"),
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH",
    values_from = MPORRES
  )

VYDSGR <- VYDSGR %>% full_join(MP_merge) 

# VS ----
VS %>% names()
VS %>% filter(is.na(VSCAT)) %>% 
  count(VISIT, VSTESTCD, VSTEST, VSCAT, VSSCAT) %>% print(n = Inf)

VS %>% filter(is.na(VSCAT)) %>%
  group_by(USUBJID, VISIT, VSTESTCD, VSSCAT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, VSTESTCD, VSSCAT, n_dup) %>% print(n = Inf)

# only have initial height for 275 patients
# I suspect the baseline DIABP, PULSE, SYSBP, TEMP are burried in the VISITs with missing data (given the number of patients)

VS_pivot<- VS %>% filter(is.na(VSCAT), is.na(VISIT)) %>% 
  arrange(VSTESTCD, VSDY) %>% 
  group_by(USUBJID, VSTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VSTESTCD, VSDY, n_dup) #%>% View()

# let's see if the first entry (by VSDY) of the NA VISIT VSTESTCD values is equal to the Day 0 VISIT value VSTESTCD values (for example, WEIGHT) 
# if true we can salvage the remaining DIABP, SYSBP, HR, TEMP
# Ask Gemma about the baseline vital signs - baseline PULSE, SYSBP and DIASBP may be salvagable depending on how confident we are in the data

VS_pivot<- VS %>% filter(is.na(VSCAT)) %>%  # ignore rescue treatments for now
  arrange(VSTESTCD, VSDY) %>% 
  group_by(USUBJID, VISIT, VSTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() 

test <- VS_pivot %>% pivot_wider(
  id_cols = USUBJID,
  names_from = c("VISIT", "VSTESTCD", "n_dup"),
  names_glue = "{VSTESTCD}_{VISIT}_{n_dup}", 
  values_from = VSSTRESN) # %>% View()

test %>% names()
test %>% 
  mutate(
    weight_diff1 = `WEIGHT_Day 0_1`- WEIGHT_NA_1,
    weight_diff2 = `WEIGHT_Day 29_1` - `WEIGHT_Day 0_1`
  ) %>% count(weight_diff1) # 593 of the weights are the same

# For now let's merge with the BL and IC data which has clear VISIT data

VS_merge <- VS %>% 
  filter(VISIT %in% c("Day 0", "Day 29"), is.na(VSCAT)) %>% 
  mutate(VISIT = ifelse(VISIT=="Day 0", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c("VISIT", "VSTESTCD"),
    names_glue = "VS_{VISIT}_{VSTESTCD}",
    values_from = VSSTRESN
  ) # %>% summarise(across(everything(), ~sum(!is.na(.x)))) #%>% View()

# what we currently have
VS_merge %>% summarise(across(everything(), ~sum(!is.na(.)))) %>% relocate(sort(names(.))) %>% print(width = Inf)

VYDSGR <- VYDSGR %>% full_join(VS_merge) 
VYDSGR %>% str()

# PE ----

# all physical examination entries are NORMAL
PE %>% names()
PE %>% count(VISIT, PETESTCD, PETEST, PECAT, PEORRES, PESTRESC) %>% print(n = Inf)

# QS ----

# KPSS-Karnofsky Performance Status - potentially useful (do not include for now)
QS %>% names()
QS %>% count(VISIT, QSTESTCD, QSTEST, QSCAT, QSORRES) %>% print(n = Inf)

# RP ----

RP %>% names()
RP %>% count(VISIT, RPTESTCD, RPTEST, RPCAT, RPORRES)
RP_merge <- RP %>% filter(is.na(RPCAT)) %>% 
  mutate(
    RP_PREG = ifelse(RPORRES == "NEGATIVE", FALSE, NA)
  ) %>% 
  select(USUBJID, RP_PREG)
RP_merge %>% count(RP_PREG)

VYDSGR %>% full_join(RP_merge) %>% names()
VYDSGR <- VYDSGR %>% full_join(RP_merge)

VYDSGR %>% 
  save(VYDSGR, file = "Study/Data/VYDSGR.RData")
