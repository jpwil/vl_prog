##########
# VFEFCS #
##########

rm(list = ls())
source("definitions.R")
load_domains("VFEFCS")

# only 4 patients have SA outcomes
mg <- ld_missingness("VFEFCS")
mg %>%
  count_na() %>%
  print(width = Inf)

# PUBLICATION ----
# Short-course paromomycin treatment of visceral leishmaniasis in India: 14-day vs 21-day treatment
# Sundar 2009, Clin Infect Dis
# PMID: 19663597
# Protocol: NCT00629031
# n = 329
# deaths = 0
# relapses = 26 (21 in 14-day arm, 5 in 21-day arm)
# BL - IC: 4 lost to follow-up, 4 stopped due to high AST and ALT. Further 15 patients failed initial cure assessment

# DATASET ----
# 329 patients
# IC TOC: if 1+ did not fail initial cure assessment; only if repeat aspirate 28 days later was 1+ (or worse)
# RS and DS domains: most outcomes at IC and DC can be correlated with publication

# DM ----
DM %>% names() # no death data
DM %>% count(USUBJID) %>% count(n)

DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = Inf)
DM %>% count(ARMCD)

DM_merge <- DM %>% 
  mutate(
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  select(
    contains("DM_"), RFSTDTC, USUBJID
  )

DM_merge %>% names()
VFEFCS <- DM_merge

# RS ----
RS %>% count()
RS %>% names()

#RS %>% count(VISIT, RSSCAT, RSTESTCD, RSEVINTX, RSCDSTDY, RSRPOC, RSORRES, RSSTRESC) %>% View()

# good to pivot
RS %>% group_by(USUBJID, VISIT, RSTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, RSTESTCD, n_dup)

RS_pivot <- RS %>% 
  mutate(
    VISIT1 = case_when(
      VISIT == "6 Months" ~ "M6",
      VISIT == "Day 15" ~ "D15",
      VISIT == "Day 22" ~ "D22",
      VISIT == "Day 43" ~ "D43",
      VISIT == "Day 50" ~ "D50",       
      is.na(VISIT) ~ "NA"
    )
   ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT1, RSTESTCD),
    values_from = RSORRES
  )

RS %>% count(VISIT, RSORRES, RSSTRESC) %>% print(n = Inf)
RS_pivot %>% count(D15_TOC, D22_TOC, D43_TOC, D50_TOC, M6_OVRLRESP, NA_OVRLRESP)  #%>% View()

# DS ----
DS %>% count()
DS %>% names()
DS %>% count(USUBJID) %>% count(n)
DS %>% count(VISIT, DSTERM, DSDECOD)
DS_merge <- DS %>% 
  select(
    USUBJID, DSTERM
  )

OUT <- RS_pivot %>% full_join(DS_merge) 
OUT %>% names()
OUT %>% count(D15_TOC, D22_TOC, D43_TOC, D50_TOC, M6_OVRLRESP, NA_OVRLRESP, DSTERM) # %>% View()
OUT %>% count(D15_TOC, D22_TOC)

# 12 patients have 1+ D15_TOC with no documented follow-up, but some with COMPLETE/RELAPSE/SPLEENIC POSITIVE DSTERMs. 
# Let's assume the COMPLETE/RELAPSE have repeat aspirates which are -ve
OUT_merge <- OUT %>% 
  mutate(
    RS_IC_LSHMANIA_SPLEEN = case_when(
      !is.na(D15_TOC) ~ D15_TOC,
      !is.na(D22_TOC) ~ D22_TOC,
      .default = NA_character_
    ),
    OUT_DC_RELAPSE = case_when(
      str_detect(NA_OVRLRESP, regex("RELAPSE")) ~ TRUE,
      .default = FALSE
    ),
    OUT_NA = FALSE,
    OUT_DC_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_IC_DRUG = case_when(
      str_detect(DSTERM, regex("DRUG")) ~ TRUE,
      .default = FALSE
    ),
    OUTC_IC_DRUG = "4 patients labelled as 'DRUG STOP ON DAY...'", 
    OUT_IC_OTHER = ifelse(DSTERM %in% c("LOST TO FU", "SPLEENIC POSITIVE"), TRUE, FALSE), 
    OUTC_IC_OTHER = "4 LTFU patients, as correspond with those described in publication, and 14 who failed repeat aspirate TOC (almost matchingf publication)",
    OUT_IC_DEATH = FALSE,
    OUT_IC = case_when(
      OUT_IC_DRUG | OUT_IC_OTHER ~ FALSE,
      .default = TRUE
    ),
    OUT_DC = case_when(
      OUT_DC_RELAPSE | !OUT_IC ~ FALSE,
      .default = TRUE
    ),
  ) %>% 
  select(USUBJID, starts_with("OUT"))

OUT_merge %>% count(OUT_IC, OUT_IC_DRUG, OUT_IC_OTHER, OUT_IC_DEATH, OUT_DC)
OUT_merge %>% names()

VFEFCS <- OUT_merge %>% select(USUBJID, contains("OUT")) %>% full_join(VFEFCS)

# there is a slight discrepancy with publication data; only 11 patients failed initial assessment in dataset, but 12 in publication (also reflected in DSTERM)
VFEFCS %>% count(across(starts_with("OUT_"))) #%>% View()
VFEFCS %>% names()

# PT ----
PT %>% names()
PT %>% count()

# allocated treatments
PT %>% count(VISIT, PTTRT) %>% print(n = Inf)
PT %>% count(PTDOSE)
PT %>% count(PTDOSFRQ)

# IN ----
# no IN domain

# SA ----
#  only patients with ALT/AST changes (presumed drug related)

SA #%>% View()

# LB ----
LB %>% count() 
LB %>% names()

LB %>% sum_na()
LB %>% count_na() #%>% View()

# unique USUBJID within VISIT and LBTESTCD
LB %>% count_dup(VISIT, LBTESTCD) %>% print(n = Inf)
LB %>% count_dup2(VISIT) %>% print(n = Inf)
LB %>% count_dup1(VISIT) %>% print(n = Inf)

LB_merge <- LB %>% 
  mutate(
    VISIT1 =  case_when(
      VISIT %in% c("Day 14", "Day 21") ~ "IC",
      VISIT == "Day 0" ~ "BL",
      .default = "other"
    ),
    VISIT = str_to_upper(str_replace_all(VISIT, " ", ""))
  )

LB_merge %>% count(VISIT, VISIT1) %>% print(n = Inf)
LB_merge %>% count(VISIT, LBTESTCD) %>% print(n = Inf)

LB %>% count_dup2(VISIT, LBTESTCD) %>% print(n = Inf)
LB_merge %>% count_dup2(VISIT, LBTESTCD) %>% print(n = Inf)

LB_pivot <- LB_merge %>% pivot_wider(
  id_cols = USUBJID,
  names_from = c(VISIT, LBTESTCD), 
  values_from = LBSTRESN
)

# LB_pivot %>% select(DM_ARM, starts_with("DAY21") | starts_with("DAY14")) %>% 
#   mutate(across(-DM_ARM, ~!is.na(.x))) %>% 
#   count(across(everything())) #%>% View()

# for all except 2 USUBJID, the IC bloods correspond to 14 days for the short course paromomycin, and 21 days for the long course paromomycin
# for many of the short course paromomycin, there are 14 day as well as 21 day bloods - if this is the case, we choose the 14 day bloods for consistency 
LB_pivot <- LB_pivot %>% select(!(starts_with("NA") | starts_with("DAY7")))
LB_pivot %>% names()
LB_pivot_clean <- LB_pivot %>% 
  mutate(
    DAY14_HGB = ifelse(is.na(DAY14_HGB), DAY21_HGB, DAY14_HGB),
    DAY14_WBC = ifelse(is.na(DAY14_WBC), DAY21_WBC, DAY14_WBC),
    DAY14_PLAT = ifelse(is.na(DAY14_PLAT), DAY21_PLAT, DAY14_PLAT),
    DAY14_CREAT = ifelse(is.na(DAY14_CREAT), DAY21_CREAT, DAY14_CREAT),
    DAY14_ALT = ifelse(is.na(DAY14_ALT), DAY21_ALT, DAY14_ALT),
    DAY14_AST = ifelse(is.na(DAY14_AST), DAY21_AST, DAY14_AST),
    DAY14_BILI = ifelse(is.na(DAY14_BILI), DAY21_BILI, DAY14_BILI),
  ) %>% 
  select(USUBJID, contains("DAY0") | contains("DAY14"))

n1 <- str_replace(names(LB_pivot_clean), "DAY0", "BL")
n2 <- str_c("LB_", str_replace(n1, "DAY14", "IC"))
names(LB_pivot_clean) <- n2

LB_pivot_clean <- LB_pivot_clean %>% 
  rename(USUBJID = LB_USUBJID) %>% 
  select(-LB_BL_HCG)

LB_pivot_clean %>% names()

LB_pivot_clean <- LB_merge %>% 
  filter(LBTESTCD == "HCG") %>% 
  mutate(LB_BL_HCG = FALSE) %>% 
  select(USUBJID, LB_BL_HCG) %>% 
  full_join(LB_pivot_clean) 

VFEFCS <- VFEFCS %>% full_join(LB_pivot_clean)
VFEFCS %>% count(DM_ARM, DM_SEX, LB_BL_HCG)

# MB ----
MB %>% count()
MB %>% names()

MB %>% count_na() #%>% View()
MB %>% count(VISIT, MBTESTCD)
MB %>% count_dup2(VISIT, MBTEST) # VISIT and MBTEST are sufficient to pivot on
MB %>% count(VISIT, MBTESTCD, MBTEST, MBMODIFY, MBORRES)
MB %>% count_dup2(VISIT) # no USUBJIDs had an aspirate on both day 15 and day 22

MB_merge <- MB %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT %in% c("Day 15", "Day 22") ~ "IC",
      .default = "other"
    )
  ) %>% 
  filter(VISIT != "other") 

MB_pivot <- MB_merge %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD),
    names_glue = "MB_{VISIT}_{MBTESTCD}",
    values_from = MBORRES
  ) %>% 
  rename(
    MB_IC_LSHMANIA_SPLEEN = MB_IC_LSHMANIA,
    MB_BL_LSHMANIA_SPLEEN = MB_BL_LSHMANIA
  )

MB_pivot %>% names()
MB_pivot <- MB_pivot %>% select(-MB_BL_HIV)
#MB_pivot %>% count(MB_IC_LSHMANIA)
MB_pivot #%>% View()

VFEFCS <- VFEFCS %>% full_join(MB_pivot)

# MP ----
MP %>% names()
MP %>% count_na() #%>% View()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP %>% count_dup2(VISIT, MPTESTCD, MPLOC)

MP_merge <- MP %>% 
  mutate(
    VISIT = str_c("V_", str_to_upper(str_replace(VISIT, " ", "")))
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC, MPTESTCD), 
    names_glue = "MP_{VISIT}_{MPLOC}_{MPTESTCD}",
    values_from = MPSTRESN
  )

# for PARO21 spleen size is available for all of week 3  (and only 29 of PARO14)
# let's choose WEEK 2 for PARO14 and WEEK 3 for PARO21
MP_merge2 <- MP_merge %>% full_join(VFEFCS %>% select(USUBJID, DM_ARM))
MP_merge2 %>% names()
MP_merge2 %>% 
  count(DM_ARM, !is.na(MP_V_WEEK2_SPLEEN_LENGTH))
MP_merge2 %>% 
  count(DM_ARM, !is.na(MP_V_WEEK3_SPLEEN_LENGTH))
MP_merge2 %>% 
  count(DM_ARM, !is.na(MP_V_6MONTHS_LIVER_LENGTH))

MP_merge2 %>% mutate(across(-DM_ARM, ~!is.na(.x))) %>% count(DM_ARM, MP_V_DAY1_SPLEEN_LENGTH, 
  MP_V_WEEK2_SPLEEN_LENGTH, MP_V_WEEK3_SPLEEN_LENGTH)  %>% print(width = Inf)
MP_merge2 %>% mutate(across(-DM_ARM, ~!is.na(.x))) %>% count(DM_ARM, MP_V_DAY1_LIVER_LENGTH, 
  MP_V_WEEK2_LIVER_LENGTH, MP_V_WEEK3_LIVER_LENGTH)  %>% print(width = Inf)

MP_merge2 %>% names()
MP_merge3 <- MP_merge2 %>% 
  mutate(
    MP_BL_SPLEEN_LENGTH = MP_V_DAY1_SPLEEN_LENGTH,
    MP_BL_LIVER_LENGTH = MP_V_DAY1_LIVER_LENGTH,
    MP_IC_SPLEEN_LENGTH = case_when(
      DM_ARM == "PARO14" ~ MP_V_WEEK2_SPLEEN_LENGTH,
      DM_ARM == "PARO21" ~ MP_V_WEEK3_SPLEEN_LENGTH,
      .default = NA
    ),
    MP_IC_LIVER_LENGTH = case_when(
      DM_ARM == "PARO14" ~ MP_V_WEEK2_LIVER_LENGTH,
      DM_ARM == "PARO21" ~ MP_V_WEEK3_LIVER_LENGTH,
      .default = NA
    )
  ) %>% 
  select(USUBJID, !contains("MP_V_"), -DM_ARM)

VFEFCS <- VFEFCS %>% full_join(MP_merge3)
VFEFCS %>% names()

# VS ----
VS %>% dim()
VS %>% names()
VS %>% count(VISIT, VSTESTCD)
VS %>% count_dup(VISIT, VSTESTCD)

VS_pivot <- VS %>% 
  mutate(
    VISIT = str_replace(VISIT, " ", "")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  ) %>% full_join(VFEFCS %>% select(USUBJID, DM_ARM)) 

# as per LB, MP, PARO14 use 2 weeks, PARO21 use 3 weeks (data are more complete)
VS_pivot %>% names()
VS_pivot %>% count(
  DM_ARM, 
  !is.na(VS_Day1_WEIGHT), 
  !is.na(VS_Week1_WEIGHT), 
  !is.na(VS_Week2_WEIGHT), 
  !is.na(VS_Week3_WEIGHT), 
  !is.na(VS_1Month_WEIGHT), 
  !is.na(VS_6Months_WEIGHT)) #%>% View()

VS_pivot2 <- VS_pivot %>% 
  mutate(
    VS_IC_TEMP = case_when(
      DM_ARM == "PARO14" ~ VS_Week2_TEMP,
      DM_ARM == "PARO21" ~ VS_Week3_TEMP,
      .default = NA
    ),
    VS_IC_WEIGHT = case_when(
      DM_ARM == "PARO14" ~ VS_Week2_WEIGHT,
      DM_ARM == "PARO21" ~ VS_Week3_WEIGHT,
      .default = NA
    ),
    VS_BL_WEIGHT = VS_Day1_WEIGHT,
    VS_BL_TEMP = VS_Day1_TEMP
  ) %>% 
  select(USUBJID, -DM_ARM, starts_with("VS_BL") | starts_with("VS_IC"))

VFEFCS <- VFEFCS %>% full_join(VS_pivot2)

# weights are often inconsistent -> will likey remove during cleaning
#VFEFCS %>% full_join(VS_pivot) %>% mutate(diff = VS_IC_WEIGHT - VS_BL_WEIGHT) %>% select(VS_BL_WEIGHT, VS_IC_WEIGHT, diff, contains("VS_"), DM_ARM, DM_AGE) %>% arrange(diff) %>% View()

# RP ----
RP %>% count_dup()
RP %>% names()

RP %>% count(RPTESTCD, RPORRES)
RP_merge <- RP %>% mutate(RP_PREG = FALSE) %>% select(USUBJID, RP_PREG)

VFEFCS <- VFEFCS %>% full_join(RP_merge)

# QS ----
QS %>% names() 
QS %>% count(VISIT, QSTESTCD) # more KPSS scores

save(VFEFCS, file = "Study/Data/VFEFCS.RData")
