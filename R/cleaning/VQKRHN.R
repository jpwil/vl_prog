# PUBLICATION

# Sundar S, Chakravarty J, Agarwal D, Shah A, Agrawal N, Rai M. Safety of a pre-formulated amphotericin 
# B lipid emulsion for the treatment of Indian Kala-azar. Trop Med Int Health. 
# 2008 Sep;13(9):1208-12. doi: 10.1111/j.1365-3156.2008.02128.x. Epub 2008 Jul 28. PMID: 18664241.

# DATABASE

# n = 45 (all domains complete)
# All patients achieved initial cure
# 4 relapses (4 in 15mg/kg group, 1 in 12 mg/kg group)
# Questions: when were repeat bloods taken?
# No DS domain

rm(list = ls())
source("definitions.R")
load_domains("VQKRHN")

mg <- ld_missingness("VQKRHN")
mg %>%
  count_na() %>%
  print(width = Inf)

TS #%>% View()

# DM ----

DM %>% names()
DM %>% count(SEX)
DM %>% count(AGE)
DM %>% count(SITEID)
DM %>% count(ARMCD)

DM_merge <- DM %>%  
  rename(
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_AGE = AGE
  ) %>% 
  select(
    USUBJID, DM_SITE = SITEID, RFSTDTC, starts_with("DM_")
  )

VQKRHN <- DM_merge

# OUTCOME ----

# No DS domain, data from RS domain (can also cross-reference with study ARM and MB data)
# 

# it does appear that there are 4 relapses that occur within 6 month (based on RSDY data)
# ABE_5, ABE_8, ABE_14, ABE_15. There are 4 further relapses that occur as RSDY of 272, 310, 475, 499

RS %>% names()
RS %>% count(VISIT, RSCAT, RSSCAT)

RS %>% dim()
RS %>% count_dup()

RS_merge <- RS %>% 
  mutate(
    VISIT = case_when(
      is.na(VISIT) ~ "D_NA",
      VISIT == "Baseline" & !is.na(VISIT) ~ "BL",
      VISIT == "Day 14" & !is.na(VISIT) ~ "IC",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(RSORRES, RSDY),
    names_glue = "{VISIT}_{.value}"
  ) %>% full_join(DM %>% select(USUBJID, ARMCD))

RS_merge #%>% View()
RS_merge %>% count(D_NA_RSORRES)
RS_merge %>% count(D_NA_RSDY)

OUT <- RS_merge %>% 
  mutate(
    OUT_IC = TRUE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_DC_RELAPSE = !is.na(D_NA_RSORRES) & !is.na(D_NA_RSDY) & D_NA_RSDY <= 180,
    OUT_DC_DEATH = FALSE,
    OUT_NA = FALSE,
    OUT_DC = !OUT_DC_RELAPSE
    ) %>% 
  select(USUBJID, starts_with("OUT"))

VQKRHN <- VQKRHN %>% full_join(OUT)

# RS RELAPSE

RS_merge %>% count(D_NA_RSORRES, D_NA_RSDY, BL_RSORRES, BL_RSDY)
RS_merge2 <- RS_merge %>% 
  rename(RS_MH_VL = BL_RSORRES) %>% 
  select(USUBJID, RS_MH_VL) %>% 
  mutate(RS_MH_VL = ifelse(is.na(RS_MH_VL), NA, TRUE))

VQKRHN <- VQKRHN %>% full_join(RS_merge2)

# MB ----
# all HIV are negative
# SPLEEN COUNT AT BASELINE AND NA

MB %>% names()
MB %>% count(VISIT, MBLOC, MBTESTCD, MBORRES)

MB_merge <- MB %>% 
  filter(!is.na(MBLOC)) %>% 
  mutate(VISIT = ifelse(is.na(VISIT), "IC", "BL")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = c(MBORRES, MBDY),
    names_glue = "{VISIT}_{.value}"
)

# 3/4 of the relapses match with MB data (OK!)
MB_merge  %>% full_join(RS_merge) %>% full_join(DM %>% select(USUBJID, ARMCD)) #%>% View()
MB_merge2 <- MB_merge %>% select(USUBJID, BL_MBORRES) %>% rename(MB_BL_LSHMANIA_SPLEEN = BL_MBORRES)

VQKRHN <- VQKRHN %>% full_join(MB_merge2)

# PT ----
PT %>% names()
PT %>% count(VISIT, PTTRT)

# IN ----
IN %>% names()
IN %>% count(VISIT, INCAT, INDOSRGM) 
IN %>% filter(VISIT == "Baseline") #%>% View()
IN_merge <- IN %>% filter(VISIT == "Baseline") %>% mutate(IN_MH_VL = TRUE) %>% select(USUBJID, IN_MH_VL)

VQKRHN <- VQKRHN %>% full_join(IN_merge)

# LB ----

LB %>% names()
LB %>% count(VISIT, LBTESTCD) %>% print(n = Inf)
LB %>% count(VISIT, LBDY) %>% print(n = Inf)

LB_merge <- LB %>% filter(VISIT %in% c("Day 1", "Day 14")) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 1", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID, 
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN, 
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )
LB_merge #%>% View()

VQKRHN <- VQKRHN %>% full_join(LB_merge)

# SA ----
# 10 patients have day 1 fever and rigor durations (minutes...)
# History of fever is available for all patients, but duration (i.e. days/weeks) 

SA %>% names()
SA %>% count(VISIT, SACAT, SATERM)
SA %>% filter(!is.na(VISIT) & VISIT %in% c("Baseline", "Day 1")) #%>% View()

# MP ---
# liver and spleen length at day 14 is available

MP %>% names()
MP %>% count(VISIT, MPLOC, MPTEST) %>% print(n = Inf)

MP_merge <- MP %>% filter(VISIT %in% c("Day 1", "Day 14")) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 1", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  ) 
MP_merge #%>% View()
VQKRHN <- VQKRHN %>% full_join(MP_merge)

# VS ----
# Baseline height and temp
# Day 1 and Day 14 temp and weight
# Use Day 1 temp (not baseline temp)

VS %>% names()
VS %>% count(VISIT, VSTESTCD) %>% print(n = Inf)

VS %>% filter(VISIT == "Baseline" & VSTESTCD == "HEIGHT") #%>% View()
VS %>% filter(VISIT == "Day 1" & VSTESTCD == "WEIGHT") #%>% View()

VS_merge <- VS %>% 
  filter(VISIT %in% c("Baseline", "Day 1", "Day 14")) %>% 
  filter(is.na(VSREFID))

VS_merge %>% count_dup(VISIT, VSREFID, VSCAT, VSTESTCD)
VS_merge %>% filter(VISIT=="Baseline" & VSTESTCD == "TEMP") #%>% View()

VS_merge %>% count(VISIT)
VS_merge2 <- VS_merge %>% 
  filter(is.na(VSCAT)) %>% 
  mutate(
    VISIT = case_when(
      VISIT %in% c("Baseline", "Day 1") ~ "BL",
      VISIT == "Day 14" ~ "IC",
      .default = "ERROR"
    )) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  )

VS_merge2 #%>% View()
VQKRHN <- VQKRHN %>% full_join(VS_merge2)

save(VQKRHN, file = "Study/Data/VQKRHN.RData")


