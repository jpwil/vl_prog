########### 
# VFETIZ #
##########

# Contributed by DNDi (according to IDDO repository)
# No funding information in protocol

# PUBLICATION

# there is no publication; protocol only (located in https://cvbe.iddo.org/)
# No outcomes are available for cross-referencing

# DATASET

# Submitted as 'Miltefosine Nepal 2004-5' by Dr Suman Rijal
# we only have DM, DS, LB, PT, RS (!!)
# n = 125
# Despite the lack of publication, the curated dataset presents the outcomes clearly -> I thnk we can include these data, if we can impute appropriately

# Ask Gemma: confirm no available vital signs or spleen size measurements? -> they are available, only they have not yet been curated

rm(list = ls())
source("definitions.R")
load_domains("VFETIZ")

mg <- ld_missingness("VFETIZ")
mg %>%
  count_na() %>%
  print(width = Inf)

TS # %>% View()

# DM ----
DM %>% dim()
DM %>% names()
DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = Inf) # range 2 - 65

DM %>% count(SITEID)
DM %>% count(DTHDTC, DTHFL) # 2 deaths reported

DM %>% count(ARMCD)
DM_clean <- DM %>%
  mutate(
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  select(starts_with("DM_"), USUBJID, RFSTDTC)

# OUTCOMES ----

RS %>% names()
RS %>% count_dup(VISIT, RSTESTCD)
RS %>% count(RSORRES)
RS %>% count(RSEVINTX)

RS_merge <- RS %>% mutate(VISIT = str_replace_all(VISIT, " ", "")) %>% 
    pivot_wider(
        id_cols = USUBJID,
        names_from = VISIT,
        values_from = c(RSORRES, RSDY)
    )

DS_merge <- DS %>% 
    select(USUBJID, DSTERM, DSDY)

DM_merge <- DM %>% select(USUBJID, DTHFL, DTHDTC, RFSTDTC, )

DS %>% names()
DS %>% count(DSEVINTX, DSTERM, DSDY) %>% print(n = Inf)
DS %>% count(DSTERM)
DS %>% count(DSEVINTX)

DS %>% filter(USUBJID == "VFETIZ_BPKIHS_MPT117") # died during the second week of treatment

OUT <- RS_merge %>% full_join(DS_merge) %>% full_join(DM_merge)
OUT %>% select(-RSDY_NA) #%>% View()

OUT %>% count(across(-c(RSDY_NA, RSDY_Day28, USUBJID, RFSTDTC, DTHDTC, DSDY, RSDY_Month7))) #%>% View()
# VFETIZ_BPKIHS_MPT091 and VFETIZ_BPKIHS_MPT057 have conflicting 7 month outcomes in RS domain (cure and relapse); DS domain is cure. 
# They relapsed after final cure (see RSEVINTX)
OUT_merge <- OUT %>% 
  mutate(
    OUT_NA = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_DEATH = is.na(RSORRES_Day28),
    OUT_DC_DEATH = FALSE,
    OUT_IC_OTHER = RSORRES_Day28 %in% c("Failue - Rescue Medicine", "Treatment Withdrawn") & !is.na(RSORRES_Day28), 
    OUTC_IC_OTHER = "One patient had treatment withdrawn (not sure why) and other patient has 28-day outcome: 'Failure - Rescue Medication', but DSDY is 144; let's assume this is pre-IC, otherwise would be coded as relapse?",
    OUT_DC_RELAPSE = RSORRES_Month7 == "Relapse" & !is.na(RSORRES_Month7),
    OUT_DC_OTHER = DSTERM == "LTFU",
    OUTC_DC_OTHER = "These 4 patients were LTFU",
    OUT_IC = ifelse(OUT_IC_DEATH | OUT_IC_OTHER, FALSE, TRUE),
    OUT_DC = ifelse(!OUT_IC | OUT_DC_RELAPSE | OUT_DC_OTHER, FALSE, TRUE)
    )
  
OUT_merge %>% mutate(across(everything(), ~as.character(.x)))# %>% View()
RS %>% filter(USUBJID %in% c("VFETIZ_BPKIHS_MPT091", "VFETIZ_BPKIHS_MPT057")) #%>% View()

# check results are consistent (yes they are)
OUT_merge %>% count(across(starts_with("OUT_"))) %>% mutate(across(everything(), ~as.character(.x))) # %>% View()
OUT_clean <- OUT_merge %>% select(USUBJID, starts_with("OUT_")) 

VFETIZ <- DM_clean %>% full_join(OUT_clean)
VFETIZ

# LB ----

# ALT/AST/WBC/HGB available at Day 0, 7, 14, 21, 28, week 12, month 7
LB %>% names()
LB %>% count(VISIT, LBTESTCD) %>% print(n = Inf)

LB_pivot <- LB %>% filter(VISIT %in% c("Day 0", "Day 28")) %>% 
  mutate(VISIT = ifelse(VISIT == "Day 0", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_pivot #%>% View()
VFETIZ <- VFETIZ %>% full_join(LB_pivot)
VFETIZ %>% names()


save(VFETIZ, file = "Study/Data/VFETIZ.RData")