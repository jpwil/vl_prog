##########
# VIZGFA #
##########

# STUDY DETAILS
# Bhattacharya 2007, J Infect Dis
# Phase 4 trial of miltefosine for the treatment of Indian visceral leishmaniasis
# n = 1135, 13 centres in Bihar
# flow chart included
# 3 deaths (all during BL-IC), and 44 relapses
# reasons for drop-outs not stated

# DATASET DETAILS
# n = 367
# 22 relapses
# 1 death (during treatment, RTA)
# 44 patients with relapse 
# only including Muzaffarpur patients (based on SITEID in DM domain)
# No useful variables in SA domain (only presence r, which is almost always present)

rm(list = ls())
source("definitions.R")
load_domains("VIZGFA")

mg <- ld_missingness("VIZGFA")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----

DM %>% names()
DM %>% count()

# one death flagged with no date (we know this was during treatment)
DM %>% count(DTHFL) 
DM %>% count(SITEID, COUNTRY)

DM %>% count(SEX)
DM %>% var_sum(AGE)

DM %>% count(ARM, ARMCD)

DM_merge <- DM %>% 
    mutate(
        DM_ARM = ARMCD,
        DM_SEX = SEX,
        DM_SITE = SITEID,
        DM_AGE = AGE
        ) %>% 
    select(USUBJID, RFSTDTC, starts_with("DM_")) 
DM_merge %>% names()
DM_merge %>% count(across(-c("DM_AGE", "USUBJID", "RFSTDTC")))

VIZGFA <- DM_merge

# DD ----

# one death
DD %>% names()
DD %>% count()
DD_merge <- DD %>% 
  mutate(DD_DEATH = TRUE, NA) %>% 
  select(USUBJID, DD_DEATH)
DD %>% print(width = Inf)

# RS ----

# all outcomes are RSTEST = "Overall Response"

RS %>% names()
RS %>% count(USUBJID) %>% count(n)
RS %>% count(VISIT, RSSCAT, RSORRES, RSSTRESC)  %>% print(n = Inf)

# these must be the patients with history of VL (but RSSCAT not 'MEDICAL HISTORY' as with other datasets)
RS %>% 
  filter(VISIT == "Day 0") %>% 
  count(VISIT, RSTESTCD, RSSCAT, RSORRES, RSDY, RSCDSTDY)

RS %>% 
  group_by(USUBJID, VISIT) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, RSSCAT, n_dup) # 7 patients have duplicate 6 month outcomes (all are RSSCAT == "ADDITIONAL OUTCOMES PROVIDED")

RS %>% names()

RS %>% count(RSORRES, VISIT, RSSCAT, RSDY) %>% print(n = Inf)
RS_pivot <- RS %>% 
  mutate(VISIT = case_when(
    VISIT == "2 Months" ~ "M2",
    VISIT == "6 Months" ~ "M6",
    VISIT == "Day 0" ~ "D0",
    is.na(VISIT) ~ "NA",
    .default = "error"
  )
  ) %>% 
  mutate(RSSCAT = ifelse(RSSCAT == "ADDITIONAL OUTCOMES PROVIDED", "AOP", "NA")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, RSSCAT),
    names_glue = "RS_{.value}_{VISIT}_{RSSCAT}",
    values_from = c(RSORRES, RSDY),
    names_vary = "slowest"
  )

RS_pivot # %>% View()

RS_pivot <- RS_pivot %>% 
  full_join(
    DM %>% select(USUBJID, DTHFL) 
  ) %>% 
  full_join(DD %>% select(USUBJID, DDORRES))

RS_pivot %>% names()
RS_pivot_merge <- RS_pivot %>% 
  mutate(RS_MH_VL = ifelse(RS_RSORRES_D0_NA == "RELAPSE", TRUE, NA))

# DS ----

DS %>% names()
DS %>% count(USUBJID) %>% count(n) # complete and unique
DS %>% count(VISIT, DSTERM, DSDY)

# OUTCOMES ----
OUT <- RS_pivot_merge %>% full_join(DS %>% select(USUBJID, DSTERM, DSDY)) 
OUT %>% names()

OUT %>% count(RS_RSORRES_M6_NA, DSTERM, RS_RSDY_M6_NA, DSDY)  #%>% View() #%>% print(n = Inf)

# DS outcomes contain all the information in RS 6 month (visit == NA) outcome
# no more information in RS_RSORRES_M2_AOP
OUT %>% count(across(-c(USUBJID, RS_RSORRES_M6_NA, RS_RSDY_M6_NA, RS_RSDY_D0_NA, RS_RSORRES_D0_NA, RS_MH_VL, RS_RSDY_M2_AOP, RS_RSORRES_M2_AOP))) %>% 
  relocate(DSTERM, DSDY) # %>% View()

OUT %>% filter(DSTERM == "NOT TURNUP") %>% pull(USUBJID)

OUT_merge <- OUT %>% 
  mutate(
    OUT_XX_DEATH = FALSE,
    OUT_IC_OTHER = case_when( # LTFU cases which happened during BL-IC
      USUBJID %in% c("VIZGFA_MUZAFFARPUR_MILT293", "VIZGFA_MUZAFFARPUR_MILT198", "VIZGFA_MUZAFFARPUR_MILT358") ~ TRUE,
      .default = FALSE
    ),
    OUTC_IC_OTHER = "These 3 patients were LTFU before IC",
    OUT_DC_OTHER = case_when( # LTFU cases which happened during IC-DC
      USUBJID %in% c("VIZGFA_MUZAFFARPUR_MILT241", "VIZGFA_MUZAFFARPUR_MILT333") ~ TRUE, 
      .default = FALSE
    ),
    OUTC_DC_OTHER = "These 2 patients were LTFU after IC and before DC assessment",
    OUT_XX_OTHER = case_when( # LTFU cases which happened at an unknown time
      USUBJID %in% c("VIZGFA_MUZAFFARPUR_MILT278", "VIZGFA_MUZAFFARPUR_MILT288", "VIZGFA_MUZAFFARPUR_MILT292", "VIZGFA_MUZAFFARPUR_MILT265", "VIZGFA_MUZAFFARPUR_MILT275", "VIZGFA_MUZAFFARPUR_MILT302") ~ TRUE,
      .default = FALSE
    ),
    OUTC_XX_OTHER = "These 6 patients were LTFU at an unknown time",
    OUT_IC_DRUG = case_when(
      RS_RSORRES_NA_AOP %in% c(
        "Drug stopped day 21 due to raised creatinine 9.53 gr 3 ",
        "Drug stopped  due to loose motions 12-15 times CTC 4",
        "Drug stopped day 4 Due to raised bilirubin after 4 days (4.70 CTC 2)",
        "Drug stopped day 21 Raised creatinine 3.83 CTC 2",
        "Drug stopped day 15 \"Due to loose motions 12-15 times, vomiting 9 times CTC 4 on day 15\"",
        "Drug stopped due to loose motions 12-15 times CTC 4"
        ) ~ TRUE,
        .default = FALSE
    ),
    OUTC_IC_DRUG = "These 6 patients had drug stopped during treatment. 4 of them have DSTERM = CURE, and 2 have DSTERM = NOT TURNUP. We consider IC_DC to be FALSE for all these patients",
    OUT_NA = FALSE,
    OUT_IC_DEATH = ifelse(DSTERM == "DIED - ROAD ACCIDENT", TRUE, FALSE),
    OUT_DC_RELAPSE = ifelse(DSTERM == "RELAPSE", TRUE, FALSE)
  ) %>% 
  mutate(
    OUT_DC_RELAPSE = ifelse(RS_RSORRES_NA_AOP == "RELAPSE at 9 months Splenic 4+" & !is.na(RS_RSORRES_NA_AOP), FALSE, OUT_DC_RELAPSE), # this shouldn't be a relapse at 9 months
    OUT_DC_DEATH = FALSE,
    OUT_IC = case_when(
      OUT_IC_DRUG ~ FALSE,
      OUT_IC_OTHER ~ FALSE,
      OUT_XX_OTHER ~ FALSE,
      OUT_IC_DEATH ~ FALSE,
      .default = TRUE
    ),
    OUT_DC = case_when(
      OUT_DC_RELAPSE ~ FALSE,
      OUT_DC_OTHER ~ FALSE,
      OUT_XX_OTHER ~ FALSE,
      OUT_IC_OTHER ~ FALSE,
      OUT_IC_DRUG ~ FALSE,
      OUT_IC_DEATH ~ FALSE,
      .default = TRUE
    )
  ) 

OUT_merge %>% count(RS_RSORRES_NA_AOP, OUT_IC_DRUG)
OUT_merge %>% count(across(contains("OUT"))) %>% print(width = Inf)
OUT_merge %>% count(OUT_IC, OUT_DC, DSTERM)

VIZGFA <- VIZGFA %>% full_join(OUT_merge %>% select(USUBJID, RS_MH_VL, contains("OUT")))

# this is good
VIZGFA %>% count(across(contains("OUT_"))) %>% mutate(across(everything(), ~as.character(.x))) #%>% View() (Avoid VS Code R Viewer tempermental checkboxes)
OUT_merge %>% count(DSTERM, RS_RSORRES_M6_NA, RS_RSORRES_M6_AOP, RS_RSORRES_NA_AOP, across(contains("OUT_"))) %>% mutate(across(everything(), ~as.character(.x))) #%>% View() 

# IN ----

IN %>% names()
IN %>% count(VISIT, INTRT, INCAT)
IN %>% count(USUBJID) %>% count(n) # 44 patients with history of VL treatment 

IN_merge <- IN %>% 
  mutate(IN_MH_VL = TRUE) %>% 
  select(USUBJID, IN_MH_VL)

VIZGFA <- VIZGFA %>% full_join(IN_merge)

# 5 extra relapse cases identified in IN domain
VIZGFA %>% count(across(contains("MH_VL")))

# PT ----
PT %>% names()
PT %>% count()

# SA ----
SA %>% names()
SA %>% count()
SA %>% count(USUBJID) %>% count(n)
SA %>% count(VISIT, SATERM, SACAT)
SA %>% filter(is.na(VISIT)) #%>% View()

# the non-fever duration entries are the patients with drug problems (OUT_IC_DRUG == TRUE)
test1 <- VIZGFA %>% filter(OUT_IC_DRUG == TRUE) %>% pull(USUBJID)
test2 <- SA %>% filter(is.na(VISIT)) %>% pull(USUBJID)
intersect(test1, test2)

# no information on duration of fever 
# (only one patient with SADUR)
SA %>% filter(!is.na(VISIT)) %>% count(USUBJID) %>% count(n) # unique USUBJIDs
SA %>% filter(!is.na(VISIT)) %>% count(SAPRESP, SAOCCUR) # all had hx of fever except 6
SA %>% filter(!is.na(VISIT)) %>% count(SAOCCUR)

SA_merge <- SA %>% filter(!is.na(VISIT)) %>% mutate(SA_HX_FEV = case_when(
  SAOCCUR == "N" ~ FALSE,
  SAOCCUR == "Y" ~ TRUE,
  .default = NA
)) %>% select(USUBJID, SA_HX_FEV)

SA_merge %>% count(SA_HX_FEV)

VIZGFA <- VIZGFA %>% full_join(SA_merge)

# LB ----

LB %>% names()
LB %>% count(VISIT, LBTESTCD, LBTEST)

# For BL, use Day 0 
# For IC, use Day 30

LB %>% count(LBDY)
LB %>% group_by(VISIT, LBTESTCD) %>% summarise(
  n_missing = sum(is.na(LBDY)),
  n = n(),
  LBDY_median = median(LBDY, na.rm = TRUE),
  LBDY_LQ = quantile(LBDY, 0.25, na.rm = TRUE),
  LBDY_UP = quantile(LBDY, 0.75, na.rm = TRUE)
  ) %>% print(n = Inf)


LB %>% count(VISIT)
LB_merge <- LB %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 30" ~ "IC",
      .default = "error"
    )
  ) %>% 
  filter(VISIT %in% c("BL", "IC"))

# confirm ready to pivot
LB_merge %>% 
  group_by(USUBJID, VISIT, LBTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, LBTESTCD, n_dup)

LB_pivot <- LB_merge %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_pivot #%>% View()
LB_pivot %>% names()

VIZGFA <- VIZGFA %>% full_join(LB_pivot)

# MB ----

# Day 0 HIV
# Baseline rK39 and Leishmania splenic aspirate
# "Parasitological cure was assessed by splenic aspiration at the end of treatment."
# But unable to find evidence of parasitological cure

MB %>% count()
MB %>% names()
MB %>% count(VISIT, MBTESTCD, MBMODIFY, MBTSTDTL, MBLOC, MBDY, EPOCH, MBEVINTX, MBRPOC)  %>% print(n = Inf) # %>% View()

# there are 180 bone marrow aspirates, performed 'at any time', and are all negative. (??)
MB %>% filter(is.na(VISIT) & is.na((MBMODIFY))) # %>% View()

MB_merge <- MB %>% 
  mutate(VISIT = ifelse(VISIT == "Day 0", "BL", "Other")) %>% 
  filter(VISIT == "BL")

# ready to merge
MB_merge %>% 
  group_by(USUBJID, VISIT, MBTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, MBTESTCD, n_dup, MBORRES, MBSTRESC, MBLOC)

MB_LSHMANIA <- MB_merge %>% 
  filter(MBTESTCD == "LSHMANIA") %>% 
  select(USUBJID, MBORRES) %>% 
  rename(MB_BL_LSHMANIA_SPLEEN = MBORRES) %>% 
  mutate(MB_BL_LSHMANIA_SPLEEN = ifelse(MB_BL_LSHMANIA_SPLEEN == "+", "1+", MB_BL_LSHMANIA_SPLEEN))

MB_OTHER <- MB_merge %>% 
  filter(MBTESTCD %in% c("HIV", "LSHMRK39")) %>% 
  mutate(
    OUT = case_when(
      MBTESTCD == "HIV" ~ FALSE,
      MBTESTCD == "LSHMRK39" ~ TRUE,
      .default = NA
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD),
    names_glue = "MB_{VISIT}_{MBTESTCD}",
    values_from = OUT
  )

MB_OTHER %>% count(across(-USUBJID))
MB_LSHMANIA %>% count(across(-USUBJID))

VIZGFA <- VIZGFA %>% full_join(MB_OTHER) %>% full_join(MB_LSHMANIA)
VIZGFA %>% names()

# MP ----

MP %>% count()
MP %>% names()
MP %>% count(MPLOC, VISIT, MPTESTCD, MPTEST)

# ready for pivoting
MP %>% group_by(USUBJID, VISIT, MPLOC, MPTESTCD) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, MPTESTCD, n_dup) %>% print(n = Inf)

MP_pivot <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 0" ~ "BL",
      VISIT == "Day 30" ~ "IC",
      .default = "error"
    )
  ) %>% 
  filter(VISIT %in% c("BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC, MPTESTCD),
    names_glue = "MP_{VISIT}_{MPLOC}_{MPTESTCD}",
    values_from = MPORRES
  )
MP_pivot  %>% names()
MP_pivot %>% count(MP_BL_SPLEEN_LENGTH, MP_IC_SPLEEN_LENGTH) %>% print(n = Inf)

VIZGFA <- VIZGFA %>% full_join(MP_pivot)
VIZGFA %>% names()

# VS ----
VS %>% names()
VS %>% count()

VS %>% count(VISIT, VSCAT, VSTESTCD, VSTEST)
VS %>% group_by(USUBJID, VISIT, VSCAT, VSTESTCD, VSTEST) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  count(VISIT, VSCAT, VSTESTCD, n_dup)

# ask Gemma - are these meant to be hx of fever, i.e. fever duration? What's the difference between MH TEMP and NA TEMP?
# which one do I use as baseline? (same for 30 day vs 1 month temperature and weight values)
VS %>% filter(VSCAT == "MEDICAL HISTORY") #%>% View()
VS %>% filter(VISIT == "Day 0" & is.na(VSCAT) & VSTESTCD == "TEMP") #%>% View()
VS %>% filter(VISIT == "Day 0" & VSTESTCD == "TEMP")# %>% View()

VS_pivot <- VS %>% 
  mutate(
    VSCAT = ifelse(!is.na(VSCAT), "MH", VSCAT),
    VISIT = case_when(
      VISIT == "1 Month" ~ "M1",
      VISIT == "2 Months" ~ "M2",
      VISIT == "6 Months" ~ "M6",
      VISIT == "Day 0" ~ "D0",
      VISIT == "Day 15" ~ "D15",
      VISIT == "Day 30" ~ "D30",
      VISIT == "Day 8" ~ "D8",
      .default = "error"
    )
  ) %>%
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSCAT, VSTESTCD),
    names_glue = "VS_{VISIT}_{VSCAT}_{VSTESTCD}",
    values_from = VSSTRESN
  ) 

# they are different temperatures
VS_pivot %>% names()
VS_pivot %>% 
  mutate(TEMP_COMP = VS_D0_MH_TEMP == VS_D0_NA_TEMP) %>% count(TEMP_COMP)
VS_pivot %>% 
  count(VS_D0_MH_TEMP, VS_D0_NA_TEMP)  %>% print(n = Inf)

VS_pivot %>% names()
VS_pivot %>% relocate(sort(names(VS_pivot))) %>% names()

# compare one month and Day 30 temp and weight
VS_pivot %>% count(VS_D0_NA_TEMP, VS_D0_MH_TEMP)
VS_pivot %>% count(!is.na(VS_D0_NA_TEMP), !is.na(VS_D0_MH_TEMP))

VS_pivot %>% count(VS_D30_NA_TEMP, VS_M1_NA_TEMP)
VS_pivot %>% count(!is.na(VS_D30_NA_TEMP), !is.na(VS_M1_NA_TEMP)) # M1 and D30 temps are different, M1 only populated if D30 populated

VS_pivot %>% count(VS_D30_NA_WEIGHT, VS_M1_NA_WEIGHT)
VS_pivot %>% count(!is.na(VS_D30_NA_WEIGHT), !is.na(VS_M1_NA_WEIGHT)) # same is true for weight

# for now, let's include baseline temp for D0 where VISIT is NA, and the D30 values - can change later after d/w Gemma
VS_merge <- VS_pivot %>% 
  select(
    USUBJID,
    VS_BL_TEMP = VS_D0_NA_TEMP,
    VS_BL_HEIGHT = VS_D0_NA_HEIGHT,
    VS_BL_WEIGHT = VS_D0_NA_WEIGHT,
    VS_IC_TEMP = VS_D30_NA_TEMP,
    VS_IC_WEIGHT = VS_D30_NA_WEIGHT
  ) 
VS_merge %>% names()
VS_merge  %>% count(VS_BL_HEIGHT)

VIZGFA <- VIZGFA %>% full_join(VS_merge)

# OTHER ----

# hospitalisation days (not useful)

HO #  %>% View()
QS # %>% View()

# KPSS at day 0 and day 30 (could be useful)
QS %>% count(VISIT)

save(VIZGFA, file = "Study/Data/VIZGFA.RData")

VIZGFA %>% names()
