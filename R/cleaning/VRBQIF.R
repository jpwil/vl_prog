# PUBLICATION

# Sundar S, Singh A, Rai M, Chakravarty J. 
# Single-dose indigenous liposomal amphotericin B in the treatment of Indian visceral leishmaniasis: a phase 2 study. 
# Am J Trop Med Hyg. 2015 Mar;92(3):513-7. doi: 10.4269/ajtmh.14-0259. Epub 2014 Dec 15. 
# PMID: 25510715; PMCID: PMC4350540.

# no withdrawals, no LTFU
# all 30 patients achieved initial cure
# two treatment failures (relapses)

# DATABASE

# 3 relapses are identified

rm(list = ls())
source("definitions.R")
load_domains("VRBQIF")

mg <- ld_missingness("VRBQIF")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----

DM %>% names()
DM %>% count()
DM %>% count(SITEID)
DM %>% count(AGE)
DM %>% count(SEX)
DM %>% count(ARMCD)
DM_merge <- DM %>% 
  select(
    USUBJID, 
    RFSTDTC, 
    DM_SEX = SEX, 
    DM_SITE = SITEID, 
    DM_AGE = AGE, 
    DM_ARM = ARMCD) 

VRBQIF <- DM_merge

# OUTCOME ----
# RS and DS final outcomes listed as 3 month VISIT (?)
# 3 relapses identified in RS domain!!
# VRBQIF_MUZAFFARPUR_FST-007
# VRBQIF_MUZAFFARPUR_FST-029
# VRBQIF_MUZAFFARPUR_FST-023
relapse <- c("VRBQIF_MUZAFFARPUR_FST-007", "VRBQIF_MUZAFFARPUR_FST-029", "VRBQIF_MUZAFFARPUR_FST-023")

DS %>% names()
DS %>% count(VISIT, DSTERM, DSDECOD)
DS #%>% View()

RS %>% names()
RS #%>% View()
RS %>% count(VISIT, RSORRES)
RS %>% filter(RSORRES == "RELAPSED")  %>% pull(USUBJID)

OUT <- DS %>% select(USUBJID) %>% 
  mutate(
    OUT_NA = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC = TRUE, 
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_DC = ifelse(USUBJID %in% relapse, FALSE, TRUE),
    OUT_DC_OTHER = FALSE,
    OUT_DC_RELAPSE = ifelse(USUBJID %in% relapse, TRUE, FALSE),
    OUT_DC_DEATH = FALSE
  )

VRBQIF <- VRBQIF %>% full_join(OUT)

# MB ----
# MB: all HIV, malaria are negative
# MB data are not useful for determining relapses
MB #%>% View()
MB_merge1 <- MB %>% 
  filter(MBTESTCD %in% c("LSHMRK39", "LSHMANIA") & VISIT != "Day 30")

MB_merge1 %>% names()
MB_merge1 %>% count(VISIT, MBTESTCD, MBMODIFY, MBCAT, MBLOC, MBMETHOD, MBORRES)

MB_merge1 %>% count(MBORRES)
MB_merge2 <- MB_merge1 %>% 
  mutate(
    VISIT = "BL"
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = MBTESTCD,
    values_from = MBORRES,
    names_glue = "MB_BL_{MBTESTCD}"
  ) %>% 
  rename(MB_BL_LSHMANIA_SPLEEN = MB_BL_LSHMANIA) %>% 
  mutate(MB_BL_LSHMRK39 = ifelse(is.na(MB_BL_LSHMRK39), NA, TRUE))

MB_merge2

VRBQIF <- VRBQIF %>% full_join(MB_merge2)

# SA ----
SA %>% names()
SA %>% count(VISIT, SATERM, SACAT, SAOCCUR, SASTDY) %>% print(n = Inf) 
SA_merge <- SA %>% 
  filter(!is.na(VISIT)) %>% 
  mutate(
    SATERM = case_when(
      SATERM == "History of Cough"          ~ "CGH",
      SATERM == "History of Rigor"          ~ "RGR",
      SATERM == "History of Vomiting"       ~ "VOM",
      SATERM == "Loss of Weight"            ~ "WL",
      SATERM == "Loss of appetite"          ~ "APP",
      SATERM == "Past History of Kala-azar" ~ "VL",
      SATERM == "Weakness"                  ~ "WEAK",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SATERM,
    values_from = SAOCCUR,
    names_glue = "SA_HX_{SATERM}",
  ) %>% 
  mutate(across(-USUBJID, ~ifelse(.x == "Y", TRUE, FALSE)))

SA_merge #%>% View()
VRBQIF <- VRBQIF %>% full_join(SA_merge)

# LB ----
LB %>% names()
LB %>% count(VISIT, LBTESTCD, LBSPEC, LBSPCCND) %>% print(n = Inf)
LB_merge1 <- LB %>% 
  filter(VISIT %in% c("Day 0", "Day 30")) %>% 
  filter(LBSPEC != "URINE" | is.na(LBSPEC))


LB_merge1 %>% count(VISIT)
LB_merge1 %>% count(VISIT, LBTESTCD, LBSPEC)

LB_merge2 <- LB_merge1 %>% 
  mutate(VISIT = ifelse(VISIT == "Day 0", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

#LB_merge2 %>% View()
VRBQIF <- VRBQIF %>% full_join(LB_merge2)

# IN ----
IN %>% names()
IN %>% count(INDECOD)

# PT ----
PT %>% names()
PT %>% count(PTTRT)

# MP ----
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP_merge <- MP %>% 
  filter(VISIT %in% c("Day 0", "Day 30")) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 0", "BL", "IC"))  %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  ) 

MP_merge #%>% View()

VRBQIF <- VRBQIF %>% full_join(MP_merge)
VRBQIF %>% names()

# VS ----
VS %>% names()
VS %>% count(VISIT, VSCAT, VSTESTCD) %>% print(n = Inf)
VS %>% filter(!is.na(VSCAT)) #%>% View()

VS_merge1 <- VS %>% filter(!is.na(VSCAT)) %>% select(USUBJID, SA_HX_FEV_DUR = VSEVLINT)
VS_merge1 #%>% View()

VS_merge2 <- VS %>% filter(VISIT %in% c("Screening", "Day 0", "Day 30")) %>% 
  filter(!(VISIT == "Screening" & VSTESTCD %in% c("TEMP", "WEIGHT"))) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 30", "IC", "BL")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  ) %>% 
  full_join(VS_merge1)

VS_merge2 #%>% View()

VRBQIF <- VRBQIF %>% full_join(VS_merge2)

# RP ----
RP_merge <- RP %>% 
  select(USUBJID, RPORRES) %>% 
  mutate(RP_PREG = ifelse(RPORRES == "NEGATIVE", FALSE, NA)) %>% 
  select(-RPORRES)

VRBQIF <- VRBQIF %>% full_join(RP_merge)

# OTHER ----
PE #%>% View() all physical examinations are recorded as normal(!)
QS %>% count(QSTESTCD) # KPSS

TV #%>% View()

# SAVE ----
save(VRBQIF, file = "Study/Data/VRBQIF.RData")