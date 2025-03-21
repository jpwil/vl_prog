##########
# VIVXJN #
##########

# PUBLICATION

# Sundar S, Pandey K, Thakur CP, Jha TK, Das VN, Verma N, Lal CS, Verma D, Alam S, Das P. Efficacy and safety of amphotericin B emulsion versus liposomal formulation 
# in Indian patients with visceral leishmaniasis: a randomized, open-label study. PLoS Negl Trop Dis. 2014 Sep 18;8(9):e3169. doi: 10.1371/journal.pntd.0003169. 
# PMID: 25233346; PMCID: PMC4169371.

# n = 500 across 4 sites (3:1 ABLE:LAMB, both 15mg/kg single dose)
# 2 deaths (one BL-IC, one IC-DC), both ABLE group
# a number of relapses occured, but unable to extract from manuscript (not confirmed by aspirate)

# DATASET

# n = 150
# patients from KAMRC only
# 9 relapses
# 6 treatment failures (presumably occuring during the initial treatment)
# DS and RS contain the same information
# Unable to identify LTFU 

rm(list = ls())
source("definitions.R")
load_domains("VIVXJN")

mg <- ld_missingness("VIVXJN")
mg %>%
  count_na() %>%
  print(width = Inf)

TS #%>% View()

# DM ----
DM %>% names() # no death data
DM %>% count(ARMCD)
DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = Inf)
DM %>% count(SITEID) # all patients are from KAMRC (Muzaffarpur)

DM_clean <- DM %>%  
    rename(
        DM_SEX = SEX,
        DM_SITE = SITEID,
        DM_AGE = AGE,
        DM_ARM = ARMCD
    ) %>% 
    select(USUBJID, RFSTDTC, starts_with("DM_"))

DM_clean #%>% View()
VIVXJN <- DM_clean

# OUTCOMES ----

# no DD, no death data in DM, so just RS and DS
RS %>% names()
RS %>% count()
RS %>% count_dup()
RS #%>% View() 
RS %>% count(RSDY)
RS %>% count(VISIT, RSCAT, RSTESTCD)

RS_merge <- RS %>% select(USUBJID, RSORRES, RSDY)

DS %>% names()
DS %>% count(VISIT)
DS_merge <- DS %>% select(USUBJID, DSTERM, DSDY)

# DS and RS domains contain the same information
# all we have is RELAPSE, CURE or TREATMENT FAILURE, with a corresponding DY entry
# it is likely treatment failures occur during treatment; although this isn't entirely certain

OUT <- RS_merge %>% full_join(DS_merge)
OUT #%>% View()

 OUT_merge <- OUT %>%  
    mutate(
        OUT_IC_DRUG = FALSE,
        OUT_IC_OTHER = ifelse(RSORRES %in% c("TREATMENT FAILURE", "Treatment Failure"), TRUE, FALSE), 
        OUTC_IC_OTHER = "These 6 x'TREATMENT FAILURES' presumably occurred during treatment (otherwise would be listed as relapses)",
        OUT_XX_OTHER = FALSE,
        OUT_XX_DEATH = FALSE,
        OUT_NA = FALSE,
        OUT_IC_DEATH = FALSE,
        OUT_DC_DEATH = FALSE,
        OUT_DC_RELAPSE = ifelse(RSORRES == "RELAPSE", TRUE, FALSE),
        OUT_DC_OTHER = FALSE,
        OUT_IC = ifelse(RSORRES %in% c("CURE", "RELAPSE"), TRUE, FALSE),
        OUT_DC = case_when(
            RSORRES == "CURE" ~ TRUE,
            RSORRES == "RELAPSE" ~ FALSE,
            .default = FALSE
        )
    ) %>% 
    select(USUBJID, starts_with("OUT"))

OUT_merge %>% select(-USUBJID) %>% mutate(across(everything(), ~as.character(.x))) %>% count(across(everything())) #%>% View()

VIVXJN <- VIVXJN %>% full_join(OUT_merge)

# PT ----
PT #%>% View()

# IN ----
IN #%>% View()

# SA ----
SA %>% names()
SA %>% count(VISIT, SACAT, SATERM, SAPRESP, SAOCCUR, SADUR, SADY) %>% print(n = Inf)

# VISIT with NA
# "VL Symptoms" - not sure what this is (duration in days from 2 days to 12 days)
# "Vomating" (only present in 3) "at any time"
# "Riger" is a duration from 15 minutes to 50 minutes, from SADY 2 to 7
# "Fever" - ?presence of fever at screening? present for 100 

# let's extract WEAKNESS, COUGH (BL) and FEVER (duration, MHx)
# weakness is Y for 145 and U for 5 (not useful)
# cough is Y for 1 and U for 149 (not useful)
# looks like the only useful SA data is duration of fever (surrogate for illness duration)

SA %>% filter(VISIT == "Day 0") %>% count_dup(SATERM)
SA %>% filter(VISIT == "Day 0") %>% count(SATERM, SAPRESP, SAOCCUR, !is.na(SADUR))

SA_merge <- SA %>% filter(VISIT == "Day 0" & SATERM == "Fever") %>% 
    mutate(SA_HX_FEV_DUR = SADUR) %>% select(USUBJID, SA_HX_FEV_DUR)

VIVXJN <- VIVXJN %>% full_join(SA_merge)

# LB ----
LB %>% names()
LB %>% count(VISIT, LBTESTCD, !is.na(LBSTRESN)) %>% print(n = Inf)

LB_clean <- LB %>% filter(VISIT %in% c("Day 0", "Day 30"))
LB_clean %>% count(VISIT, LBTESTCD, LBSPEC, LBSPEC) %>% print(n = Inf)
LB_clean %>% filter(VISIT == "Day 0" & LBTESTCD == "WBC" & LBSPEC == "URINE") #%>% View()

LB_clean %>% count(LBSPEC)
LB_clean <- LB %>% filter(VISIT %in% c("Day 0", "Day 30") & is.na(LBSPEC) & !LBTESTCD %in% c("HCG", "PROT"))
LB_clean %>% count(VISIT, LBTESTCD, !is.na(LBSTRESN)) %>% print(n = 50)
LB_clean %>% count_dup(VISIT, LBTESTCD) %>% print(n = 50)


LB_pivot <- LB_clean %>% 
    mutate(VISIT = ifelse(VISIT == "Day 30", "IC", "BL")) %>% 
    pivot_wider(
        id_cols = USUBJID,
        names_from = c(VISIT, LBTESTCD),
        values_from = LBSTRESN,
        names_glue = "LB_{VISIT}_{LBTESTCD}"
    ) %>% 
    select(USUBJID, starts_with("LB_"))

LB_HCG <- LB %>% filter(LBTESTCD == "HCG") %>% 
  mutate(LB_BL_HCG = FALSE) %>% 
  select(USUBJID, LB_BL_HCG)

VIVXJN <- VIVXJN %>% full_join(LB_pivot)
VIVXJN <- VIVXJN %>% full_join(LB_HCG)

VIVXJN %>% names()

# MP ----

MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPTEST, MPSTAT, MPREASND, MPLOC)
MP %>% count_dup(VISIT, MPTESTCD, MPTEST, MPSTAT, MPREASND, MPLOC)

MP_pivot <- MP %>% 
    filter(VISIT %in% c("Day 1", "Day 30")) %>% 
    mutate(VISIT = ifelse(VISIT == "Day 1", "BL", "IC")) %>% 
    pivot_wider(
        id_cols = USUBJID,
        names_from = c(VISIT, MPLOC),
        values_from = MPORRES,
        names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
    ) %>% 
    select(
        USUBJID, starts_with("MP_")
    )

MP_pivot2 <- MP %>% 
    filter(VISIT %in% c("Day 1", "Day 30")) %>% 
    mutate(VISIT = ifelse(VISIT == "Day 1", "BL", "IC")) %>% 
    pivot_wider(
        id_cols = USUBJID,
        names_from = c(VISIT, MPLOC),
        values_from = MPDY,
        names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
    ) %>% 
    select(
        USUBJID, starts_with("MP_")
    ) 
MP_pivot2 %>% mutate(diff = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) %>% count(diff)

MP_pivot %>% names()
MP_pivot #%>% View()

VIVXJN <- VIVXJN %>% full_join(MP_pivot)

# MB ----

MB %>% names()
MB %>% count(VISIT, MBTESTCD, MBTEST, MBMODIFY, MBTSTDTL, MBCAT, MBSTAT, MBREASND)
MB %>% filter(VISIT %in% c("Day 0", "Day 30")) %>% 
    count(VISIT, MBTESTCD, MBORRES, MBCAT, MBLOC)

MB_pivot <- MB %>% 
  filter(VISIT %in% c("Day 0")) %>% 
  mutate(VISIT = "BL") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD),
    values_from = MBORRES,
    names_glue = "MB_{VISIT}_{MBTESTCD}"
  )

# all patients are HIV, HBSAG, HCAG, PLSMDM negative  
MB_pivot %>% names()
MB_pivot %>% count(MB_BL_PLSMDM)

MB_pivot <- MB_pivot %>% 
  mutate(MB_BL_LSHMANIA_SPLEEN = MB_BL_LSHMANIA) %>% 
  select(USUBJID, MB_BL_LSHMANIA_SPLEEN)

MB_pivot %>% names()
VIVXJN <- VIVXJN %>% full_join(MB_pivot)

# VS ----

VS %>% names()
VS %>% count(VISIT, VSCAT, VSTESTCD, VSDY) %>% print(n = Inf)

# these temperatures are probably at time of starting treatment
VS %>% filter(is.na(VISIT)) #%>% View()

# Day 1 temp readings?
VS %>% filter(VISIT == "Day 0" & VSCAT == "MEDICAL HISTORY") #%>% View()

VS_pivot <- VS %>% 
  mutate(VISIT = str_replace_all(VISIT, " ", "")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN
  ) 

VS_pivot %>% names()
VS_pivot %>% mutate(test = Day0_WEIGHT == Day1_WEIGHT) %>% count(test)
VS_pivot  %>% mutate(test = NA_TEMP == Day0_TEMP) %>% count(test)

VS_clean <- VS_pivot %>% 
  mutate(
    VS_BL_TEMP = Day0_TEMP,
    VS_BL_WEIGHT = Day0_WEIGHT,
    VS_IC_TEMP = Day30_TEMP,
    VS_IC_WEIGHT = Day30_WEIGHT) %>% 
  select(USUBJID, starts_with("VS_"))

VIVXJN <- VIVXJN %>% full_join(VS_clean)

# RP ----

RP %>% names()
RP %>% count(VISIT, RPTEST, RPORRES)
RP_merge <- RP %>% mutate(RP_PREG = ifelse(RPORRES == "NEGATIVE", FALSE, NA)) %>% 
  select(USUBJID, RP_PREG)

VIVXJN <- VIVXJN %>% full_join(RP_merge)

# QS ----
QS %>% count(QSTESTCD)
# KPSS data

save(VIVXJN, file = "Study/Data/VIVXJN.RData")
