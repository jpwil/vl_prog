# PUBLICATION

# Chakraborty D, Basu JM, Sen P, Sundar S, Roy S. 
# Human placental extract offers protection against experimental visceral leishmaniasis: a pilot study for a phase-I clinical trial. Ann Trop Med Parasitol. 
# 2008 Jan;102(1):21-38. doi: 10.1179/136485908X252133. PMID: 18186975.

# n = 5 (all HPE)
# 4 patients failed initial cure assessment
# 1 patient achieved initial cure and final cure at 6 months
# 0 relapses
# 0 deaths

# DATABASE

# n = 10 (5 x HPE and 5 x amphotericin B deoxycholate)
# it appears all 5 x amphotericin B deoxycholate patients completed 6 months with cure
# the 5 x HPE patients match with results from publication
# need to confirm with Gemma - when were the repeat splenic aspirates? immediately post-treatment, or 1-month post treatment?

rm(list = ls())
source("definitions.R")
load_domains("VFFFOP")

mg <- ld_missingness("VFFFOP")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----

DM %>% names()
DM %>% count(ARMCD)
DM %>% count(SEX)

DM_merge <- DM %>% 
  mutate(
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_ARM = ARMCD,
    DM_SITE = SITEID
  ) %>% 
  select(USUBJID, RFSTDTC, starts_with("DM_"))

VFFFOP <- DM_merge

# OUTCOME ----

# DS only gives 6 month outcomes
# GEMMA need to clarify the timing of the post-treatment splenic aspirates
# If we include the 5 x amphotericin B deoxycholate patients, we would need to extrapolate from the protocol 
DS %>% count_dup()
DS %>% names()
DS %>% count(VISIT, DSTERM)
DS_merge <- DS %>% select(USUBJID, DSTERM)

RS %>% count_dup()
RS %>% names()
RS %>% count(VISIT, RSTESTCD, RSCAT, RSORRES)
RS_merge <- RS %>% select(USUBJID, RSORRES)

MB %>% filter(MBTESTCD == "LSHMANIA") %>% count(MBEVINTX)

MB_merge <- MB %>% filter(MBTESTCD == "LSHMANIA") %>%
  mutate(TIME = ifelse(is.na(MBEVINTX), "BL", "OTHER")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = MBORRES,
    names_glue = "MB_{VISIT}"
  )

OUT <- MB_merge %>% full_join(DM_merge %>% select(USUBJID, DM_SEX, DM_AGE, DM_ARM)) %>% full_join(DS_merge) %>% full_join(RS_merge) 
OUT_merge <- OUT %>% 
  select(
    MB_BL_LSHMANIA_SPLEEN = `MB_Day 0`,
    DSTERM,
    USUBJID
  ) %>% 
  mutate(
    OUT_NA = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC = ifelse(DSTERM == "Cured", TRUE, FALSE),
    OUT_IC_DEATH = FALSE,
    OUT_IC_OTHER = ifelse(DSTERM == "Cured", FALSE, TRUE),
    OUTC_IC_OTHER = "These 4 failed patients has positive parasites at ToC",
    OUT_IC_DRUG = FALSE,
    OUT_DC = ifelse(DSTERM == "Cured", TRUE, FALSE),
    OUT_DC_RELAPSE = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_DC_DEATH = FALSE
  ) %>% select(-DSTERM)
  
OUT_merge %>% count(across(starts_with("OUT_"))) #%>% View()

VFFFOP <- VFFFOP %>% full_join(OUT_merge)
VFFFOP #%>% View()

# IN and PT ----

IN %>% names()
IN #%>% View()
PT %>% names()
#PT %>% count(VISIT, PTTRT) %>% View()

# SA ----
# all SADUR data is duration of fever/rigors according to study day (in minutes)
SA %>% count()
SA %>% names()
SA %>% count(VISIT, SACAT, SATERM, SAOCCUR, !is.na(SADUR), SAEVINTX) #%>% View()
SA_merge <- SA %>% filter(SACAT == "MEDICAL HISTORY") %>% 
  mutate(
    SACODE = case_when(
      SATERM == "History of Anorexia" ~ "APP",
      SATERM == "History of Bleeding" ~ "BLD",
      SATERM == "History of Pain in the Abdomen" ~ "ABDO",
      SATERM == "History of Rigor" ~ "RGR",
      SATERM == "History of Vomiting" ~ "VOM", 
      SATERM == "History of Weakness" ~ "WEAK",
      .default = "ERROR"
    ),
    SAOCCUR = case_when(
      SAOCCUR == "Y" ~ TRUE, 
      SAOCCUR == "N" ~ FALSE,
      .default = NA
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = SACODE,
    values_from = SAOCCUR,
    names_glue = "SA_HX_{SACODE}"
  ) 
SA_merge #%>% View()
VFFFOP <- VFFFOP %>% full_join(SA_merge)

# LB ----
# there are 30 days and 1 month after EoT bloods (which one to use?)
# this will depend on when the splenic aspirates were positive

LB %>% names()
LB %>% count(VISIT, LBTESTCD) %>% print(n = Inf)
LB_merge <- LB %>% filter(VISIT %in% c("Day 0", "Day 30")) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 0", "BL", "IC")) %>%   # 30 day initial cure
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_merge #%>% View()

VFFFOP <- VFFFOP %>% full_join(LB_merge)

# MP ----

MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)

MP_merge <- MP %>% filter(VISIT %in% c("Day 0", "Day 30")) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 0", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_LENGTH"
  )
MP_merge #%>% View()
VFFFOP <- VFFFOP %>% full_join(MP_merge)

# VS ----
VS %>% names()
VS %>% count(VISIT, VSCAT, VSTESTCD, VSSTRF)
VS %>% filter(VISIT == "Day 0") %>% arrange(VSCAT, VSTESTCD) #%>% View()

VS_merge <- VS %>% 
  filter(
    is.na(VSCAT),
    VISIT %in% c("Day 0", "Day 30")
  ) %>% 
  mutate(
    VISIT = ifelse(VISIT == "Day 0", "BL", "IC")
    ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  )

VS_merge #%>% View()
VFFFOP <- VFFFOP %>% full_join(VS_merge)

QS %>% count(QSTESTCD) # KPSS

# SAVE ----
save(VFFFOP, file = "Study/Data/VFFFOP.RData")

