# review relationship between duration of fever and relapse risk
rm(list = ls())
library(tidyverse)
library(lme4)

VBXLIU <- readRDS("data/cleaned_rds/VBXLIU.rds")
VSEVQC <- readRDS("data/cleaned_rds/VSEVQC.rds")

VBXLIU %>% names()
VBXLIU <- VBXLIU %>% select(
  USUBJID, DM_AGE, DM_SEX, MB_BL_LSHMANIA_BONE, MB_BL_LSHMANIA_SPLEEN, MB_BL_LSHMANIA_LYMPH, SA_HX_FEV_DUR, OUT_IC, OUT_DC_RELAPSE)

VBXLIU %>% names()


VSEVQC %>% names()
VSEVQC <- VSEVQC %>% select(
  USUBJID, DM_AGE, DM_SEX, MB_BL_ALL = MB_DAY0_MBORRES, SA_HX_FEV_DUR = SADUR, OUT_IC, OUT_DC_RELAPSE
)

# continue cleaning
VSEVQC %>% names()
VBXLIU %>% names()

### VSEVQC

VSEVQC %>% names()
VSEVQC %>% count(DM_AGE)
VSEVQC %>% count(DM_SEX)
VSEVQC %>% count(MB_BL_ALL)
VSEVQC %>% count(SA_HX_FEV_DUR) %>% print(n = Inf)

VSEVQC_CLEAN <- VSEVQC %>% 
  mutate(
    STUDYID = "VSEVQC",
    MB_BL_LSH = case_when(
    MB_BL_ALL == "1+" ~ 1,
    MB_BL_ALL == "2+" ~ 2,
    MB_BL_ALL == "3+" ~ 3,
    MB_BL_ALL == "4+" ~ 4,
    MB_BL_ALL == "5+" ~ 5,
    MB_BL_ALL == "6+" ~ 6,
    .default = NA),
    SA_DUR = (duration(SA_HX_FEV_DUR) / (60 * 60 * 24)) %>% unclass()
  ) %>% select(USUBJID, STUDYID, DM_AGE, DM_SEX, MB_BL_LSH, SA_DUR, OUT_IC, OUT_DC_RELAPSE)


VSEVQC_CLEAN %>% names()
#VSEVQC_CLEAN %>% View()

### VBXLIU

VBXLIU %>% names()
VBXLIU %>% count(pick(starts_with("MB_")))
VBXLIU_CLEAN <- VBXLIU %>% 
  mutate(
    STUDYID = "VBXLIU",
    MB_BL_LSH = if_else(is.na(MB_BL_LSHMANIA_BONE), MB_BL_LSHMANIA_SPLEEN, MB_BL_LSHMANIA_BONE),
    SA_DUR = (duration(SA_HX_FEV_DUR) / (60 * 60 * 24)) %>% unclass()
  )

VBXLIU_CLEAN %>% names()
VBXLIU_CLEAN <- VBXLIU_CLEAN %>% 
  mutate(
    MB_BL_LSH = case_when(
      MB_BL_LSH == "0" ~ NA,
      MB_BL_LSH == "1+" ~ 1,
      MB_BL_LSH == "2+" ~ 2,
      MB_BL_LSH == "3+" ~ 3,
      MB_BL_LSH == "4+" ~ 4,
      MB_BL_LSH == "5+" ~ 5,
      MB_BL_LSH == "6+" ~ 6,
      .default = NA)
  ) %>% select(USUBJID, STUDYID, DM_AGE, DM_SEX, MB_BL_LSH, SA_DUR, OUT_IC, OUT_DC_RELAPSE)

VBXLIU_CLEAN %>% names()
VSEVQC_CLEAN %>% names()

# MERGE DATASETS

EA_VL <- rbind(VBXLIU_CLEAN, VSEVQC_CLEAN)
EA_VL %>% count(OUT_IC)
EA_VL %>% count(OUT_DC_RELAPSE)

mod1 <- glm(
  formula = OUT_DC_RELAPSE ~ STUDYID + DM_AGE + I(DM_AGE^2) + DM_SEX + MB_BL_LSH + log(SA_DUR),
  data = EA_VL %>% filter(OUT_IC),
  family = binomial()
)

mod1 %>% summary()
EA_VL %>% count(OUT_DC_RELAPSE)
