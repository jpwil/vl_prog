library(tidyverse)

##########
# VEZMZD #
##########

# PUBLICATION

# Sundar S, Rai M, Chakravarty J, Agarwal D, Agrawal N, Vaillant M, Olliaro P, Murray HW. New treatment approach in 
# Indian visceral leishmaniasis: single-dose liposomal amphotericin B followed by short-course oral miltefosine. 
# Clin Infect Dis. 2008 Oct 15;47(8):1000-6. doi: 10.1086/591972. PMID: 18781879.

# TREATMENT ARM                                             COMMENTS        N   GROUP
# 5 mg/kg of L-AmB alone                                    Randomised	    45  Group A
# 5 mg/kg of L-AmB followed by miltefosine for 10 days      Randomised	    46  Group B
# 5 mg/kg of L-AmB followed by miltefosine for 14 days      Randomised	    45  Group C
# 3.75 mg/kg of L-AmB followed by miltefosine for 14 days   Randomised	    45  Group D
# 5 mg/kg of L-AmB followed by miltefosine for 7 days       Non-randomised	45  Group E (later added on)

# IC: Day 16
# DC: 9 months or more

# All achieved initial apparent cure
# 22 previously treated
# One death (IC-DC)
# 9 relapses (7 during months 1–6 and 2 during months 7–9)
# All patients achieved IC

# DATASET

# Whilst publication states DC at 9 months, raw data suggests DC captured at 6 months post-treatment
# 226 patients
# 1 death accounted for
# no VISIT data for SA, LB

rm(list = ls())
source("definitions.R")
load_domains("VEZMZD")

# All domains are complete
# All standard domains are available, + QS and RP
mg <- ld_missingness("VEZMZD")
mg %>%
  count_na() %>%
  print(width = Inf)

DM %>% count()
DM %>% names()

DM %>% count(DTHFL) # Death flag available (not death date)
DM %>% count(SEX)
DM %>% summary(AGE)
DM %>% count(ARMCD, ARM)

DM_merge <- DM %>%
  select(
    USUBJID,
    DM_AGE = AGE,
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_SITE = SITEID,
    RFSTDTC
  )

DM_merge %>% names()
DM_merge %>% count(DM_ARM)

VEZMZD <- DM_merge

# OUTCOMES ----

RS %>% names()
RS %>% count()

# only 44 RS responses for definitive cure
RS %>% count(VISIT, RSTESTCD, RSTEST, RSCAT, RSSCAT, RSORRES) %>% print(n = Inf)
RS %>% count_dup()
RS %>% count_dup(VISIT)

TV #%>% View()

RS_pivot <- RS %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Final Response" ~ "DC",
      VISIT == "Screening" ~ "BL",
      is.na(VISIT) ~ "RL",
      .default = "error"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    names_glue = "RS_{VISIT}",
    values_from = RSORRES
  )

RS_pivot %>% count(RS_RL)
RS_pivot %>% count(RS_BL)
RS_pivot %>% count(RS_DC)

DS %>% count() # now we have 226!
DS %>% names()
DS %>% count(VISIT, DSTERM)
DS %>% count_dup(VISIT)

DS_pivot <- DS %>% 
  select(USUBJID, DSTERM)

# one patient (IDR-085) has conflicting RS/DS relapse outcomes (likely this is relapse, so treat as relapse)
OUT_merge <- 
  DM %>% filter(DTHFL == "Y") %>% select(USUBJID, DTHFL) %>% 
  full_join(RS_pivot) %>%
  full_join(DS_pivot)

DS %>% filter(USUBJID == "VEZMZD_MUZAFFARPUR_IDR-085")
RS %>% filter(USUBJID == "VEZMZD_MUZAFFARPUR_IDR-085") #%>% View()

OUT_merge %>% count(RS_BL)
OUT_merge %>% count(DSTERM)

OUT_final <- OUT_merge %>% 
  mutate(
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_NA = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_IC = TRUE,
    OUT_IC_DEATH = FALSE,
    OUT_DC = 
      case_when(
        DSTERM == "CURE" ~ TRUE,
        .default = FALSE
      ),
    OUT_DC_DEATH = ifelse(DSTERM == "DIED AT HOME", TRUE, FALSE),
    OUT_DC_RELAPSE = ifelse(DSTERM == "RELAPSE", TRUE, FALSE),
    OUT_DC_OTHER = FALSE,
    RS_MH_VL = ifelse(is.na(RS_BL), NA, TRUE)) %>% 
    select(USUBJID, RS_MH_VL, starts_with("OUT"))

OUT_final #%>% View()

VEZMZD <- VEZMZD %>% full_join(OUT_final)
VEZMZD %>% names()

# SA ----

# Are these values the presence of FEVER and RIGORS on the day of starting treatment?
# Yes - it appears so. 

# no VISIT data
# no duration of fever history
# no PMHx of fever/rigors

# fever and rigor data is one per patient, but the day of onset 
SA %>% names()
SA %>% count(SADUR) %>% print(n = 50) # duration data is in minutes
SA #%>%  View()
SA %>% count(SATERM, SAOCCUR) %>% print(n = Inf)

SA %>% count(SATERM, SAOCCUR, SASTAT, SAREASND, !is.na(SADUR), !is.na(SADY))
SA %>% count(SADUR) %>% print(n = Inf)

SA %>% group_by(USUBJID, SATERM) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  filter(n_dup > 1) #%>% View()

SA_explore <- SA %>% 
  filter(!is.na(SAOCCUR)) %>%
  filter(USUBJID != "VEZMZD_MUZAFFARPUR_IDR-101") %>%  
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = c(SAOCCUR, SADY)
) %>% 
  relocate(USUBJID, SAOCCUR_FEVER, SADY_FEVER, SAOCCUR_RIGOR, SADY_RIGOR)

# yes - the SADY are the same for FEVER and RIGOR outcomes
# need to clarify when this time is
# but for now, seems likely this is BL data (not history!)
SA_explore %>% 
  mutate(check = SADY_RIGOR == SADY_FEVER) #%>% View()

SA_merge <- SA %>% 
  filter(
    !(USUBJID == "VEZMZD_MUZAFFARPUR_IDR-101" & SASEQ == 3),
    !is.na(SAOCCUR)
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR
  ) %>% 
  mutate(
    SA_BL_FEV = case_when(
      FEVER == "Y" ~ TRUE,
      FEVER == "N" ~ FALSE,
      .default = NA
    ),
    SA_BL_RGR = case_when(
      RIGOR == "Y" ~ TRUE,
      RIGOR == "N" ~ FALSE,
      .default = NA
    )
  ) %>% 
  select(USUBJID, starts_with("SA_"))

VEZMZD <- VEZMZD %>% full_join(SA_merge)

# IN ----

IN %>% names()
IN %>% count()
IN %>% count(VISIT)
IN %>% count(INDY)
IN %>% count(INSTDY, INDY)

# LB ----

# no VISIT data again :-/
LB %>% names()
LB %>% filter(LBTESTCD == "HGB") %>% count(LBDY) %>% print(n = Inf)

LB %>% filter(LBDY < 50) #%>% ggplot() + geom_histogram(aes(x = LBDY), width = 1)
LB %>% count_dup(LBTEST) %>% print(n = Inf)

LB %>% count(LBDY) %>% print(n = 50)
# let's look at blood tests performed between days 1-5 (this must be the baseline bloods)

LB %>% 
  filter(LBDY > 0 & LBDY <= 5) %>% 
  count_dup(LBTESTCD) # only one urea has duplicate USUBJID in this initial period (VEZMZD_MUZAFFARPUR_IDR-193, LBSEQ 4 and 5 (both LBDY 1))


LB %>% 
  filter(LBDY > 0 & LBDY <= 5) %>% 
  filter(LBTESTCD == "UREAN") # %>% View()

LB %>% filter(LBTESTCD == "UREAN", USUBJID == "VEZMZD_MUZAFFARPUR_IDR-193") #%>% View()
LB %>% filter(LBTESTCD == "PLAT", USUBJID == "VEZMZD_MUZAFFARPUR_IDR-061") # VEZMZD_MUZAFFARPUR_IDR-061 has two platelet counts on day 18 (LBSEQ 34,35)

# BASELINE
LB %>% 
  filter(LBDY > 0 & LBDY <= 5 & !(USUBJID == "VEZMZD_MUZAFFARPUR_IDR-193" & LBSEQ == 5)) %>% 
  count_dup(LBTESTCD)
LB_BL <- LB %>% 
  filter(LBDY > 0 & LBDY <= 5 & !(USUBJID == "VEZMZD_MUZAFFARPUR_IDR-193" & LBSEQ == 5)) %>% 
  mutate(VISIT = "BL")

# INITIAL CURE
LB_IC <- LB %>% 
  filter(LBDY >= 16 & LBDY <= 22) %>% 
  filter(!(USUBJID == "VEZMZD_MUZAFFARPUR_IDR-061" & LBSEQ == 34))  %>% 
  mutate(VISIT = "IC")

LB_merge <- LB_BL %>% bind_rows(LB_IC)
LB_pivot <- LB_merge %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("LB_")) %>% 
  select(-LB_BL_HCG)

LB_pivot %>% names()
LB_pivot %>% count()

LB_HCG <- LB %>% 
  filter(LBTESTCD=="HCG") %>% 
  mutate(LB_BL_HCG = FALSE) %>% 
  select(USUBJID, LB_BL_HCG)

LB_HCG %>% names()

LB_pivot <- LB_pivot %>% full_join(LB_HCG)
LB_pivot %>% count(LB_BL_HCG)

VEZMZD <- VEZMZD %>% full_join(LB_pivot)

VEZMZD %>% count(DM_SEX, LB_BL_HCG) # !!

# MB ----
MB %>% names()
MB %>% 
  filter(!is.na(VISIT)) %>% 
  count(VISIT, MBTEST, MBTESTCD, MBMODIFY, MBTSTDTL, MBORRES, MBSTAT, MBSPEC)
MB %>% count_dup(MBTEST, VISIT)

MB_merge <- MB %>% 
  filter(!is.na(VISIT)) %>% 
  mutate(
    VISIT = "BL"
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}_{MBTESTCD}"
  ) %>% 
  rename(
    MB_BL_LSHMANIA_SPLEEN = MB_BL_LSHMANIA
  ) %>% 
  mutate(
    MB_BL_HIV = case_when(
      is.na(MB_BL_HIV) | MB_BL_HIV == "NOT DONE" ~ NA,
      MB_BL_HIV == "NEGATIVE" ~ FALSE,
      .default = NA
    )
  )
MB_merge %>% count(MB_BL_HIV, MB_BL_LSHMANIA_SPLEEN)

VEZMZD <- VEZMZD %>% full_join(MB_merge)

# MP ----
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPSTAT, MPLOC, MPREASND, MPMETHOD)

# just look at day 1 and week 2
MP %>% 
  filter(VISIT %in% c("Day 1", "Week 2")) %>% 
  count_dup(VISIT, MPLOC)

MP_merge <- MP %>% 
  filter(VISIT %in% c("Day 1", "Week 2")) %>% 
  mutate(
    VISIT = ifelse(VISIT == "Day 1", "BL", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  )

MP_merge # %>% View()
MP_merge %>% names()
VEZMZD <- VEZMZD %>% full_join(MP_merge)

# VS ----
VS %>% names()
VS %>% count(VISIT, VSTESTCD, VSCAT)
VS %>% count_dup(VISIT, VSTESTCD)

# this looks like baseline temperature
VS %>% filter(is.na(VISIT)) #%>% View()
VS %>% filter(VISIT == "Day 1" & VSTESTCD == "TEMP") # %>% View() - let's use these values

# "Screening" is time of recruitment
VS %>% filter(VISIT == "Screening" & VSTESTCD == "TEMP") #%>% View()

VS_pivot <- VS %>% 
    mutate(VISIT = str_replace_all(VISIT, " ", "")) %>% 
    pivot_wider(
      id_cols = USUBJID,
      names_from = c(VISIT, VSTESTCD),
      values_from = c(VSSTRESN, VSDY),
      names_glue = "VS_{VISIT}_{VSTESTCD}_{.value}"
    )

# NA TEMP and DAY 1 TEMP - what's the difference? Ask Gemma....
VS_pivot %>% names()
VS_pivot %>% count(VS_NA_TEMP_VSDY, VS_Day1_TEMP_VSDY)
VS_pivot %>% count(VS_NA_TEMP_VSSTRESN, VS_Day1_TEMP_VSSTRESN) %>% print(n = Inf)

# for now use Day 1 and 2 Weeks Visit data
VS_pivot <- VS %>% 
    mutate(VISIT = str_replace_all(VISIT, " ", "")) %>% 
    filter(VISIT %in% c("Day1", "Week2")) %>% 
    mutate(VISIT = ifelse(VISIT == "Day1", "BL", "IC")) %>% 
    pivot_wider(
      id_cols = USUBJID,
      names_from = c(VISIT, VSTESTCD),
      values_from = c(VSSTRESN),
      names_glue = "VS_{VISIT}_{VSTESTCD}"
    )

VEZMZD <- VEZMZD %>% full_join(VS_pivot)

VEZMZD %>% names()
VEZMZD %>% select(contains("VS_")) #%>% View()

# RP ----
# same as LB domain pregnancy data
RP %>% names()
RP %>% count(VISIT, RPTESTCD, RPORRES)
RP_merge <- RP %>% 
  mutate(RP_PREG = ifelse(RPORRES == "N", FALSE, NA)) %>% 
  select(USUBJID, RP_PREG)

VEZMZD <- VEZMZD %>% full_join(RP_merge)
VEZMZD %>% count(DM_SEX, RP_PREG, LB_BL_HCG)

# QS ----
# all KPS Scale
QS %>% names()
QS %>% count(VISIT, QSTESTCD, QSCAT)

save(VEZMZD, file = "Study/Data/VEZMZD.RData")
