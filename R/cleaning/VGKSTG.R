############
## VGKSTG ##
############

# PUBLICATION

# Rijal S, Bhandari S, Koirala S, Singh R, Khanal B, Loutan L, Dujardin JC, Boelaert M, Chappuis F. Clinical risk 
# factors for therapeutic failure in kala-azar patients treated with pentavalent antimonials in Nepal. 
# Trans R Soc Trop Med Hyg. 2010 Mar;104(3):225-9. doi: 10.1016/j.trstmh.2009.08.002. Epub 2009 Sep 1. PMID: 19726065.

# DATASET

# 224 patients were considered 
# 198 eligible and started treatment (other patients either had taken SSG before or had contraindication)
# 198 started treatment with SSG, 24 started treatment with AMPDC, 2 unknown
# BL-IC: 5 died, 14 non-responders
# IC-DC: 2 relapses and 12 LTFU

### I have coded for 6 month outcomes (NOT the 1 year outcomes) - some deaths / relapses are therefore not coded
### The dataset describes some patients as initial responders even if SSG changed to AMBDC during initial treatment - I have described these as non-responders (OUT_IC is NA)
### It is surprising how many of the patients on AMBDC (n = 24) are marked as late relapse in the DS/RS domains -> but no relapses before 180 day assessment!

rm(list = ls())
source("definitions.R")
load_domains("VGKSTG")

# All USUBJIDs across domains
mg <- ld_missingness("VGKSTG")
mg %>%
  count_na() %>%
  print(width = Inf)

# TREATMENT ----

# PT ----
PT %>% count()
PT %>% names()
PT %>% count(VISIT, VISITDY)  %>% arrange(VISITDY) %>% print(n = Inf)
PT %>% count_dup(VISIT) %>% count(count_col)
PT %>% count(PTTRT)

PT %>% count(VISIT, VISITDY, PTTRT, PTDOSRGM)  %>% arrange(VISITDY) %>% print(n = Inf)

# who receives amphotericin B days 30-43? - these are all patients with 30 days SSG immediately before
PT %>% filter(PTTRT == "Amphotericin B" & VISITDY >= 31) %>% pull(USUBJID) %>% unique() #same patients
ap <- PT %>% filter(PTTRT == "Amphotericin B" & VISITDY >= 31) %>% pull(USUBJID) %>% unique() 
PT %>% filter(USUBJID %in% ap) %>% count(USUBJID, PTTRT) %>% print(n = Inf)

# who receives amphotericin B days 1-15? - there patients only receive Amphotericin B
ab <- PT %>% filter(PTTRT == "Amphotericin B" & VISITDY <= 13) %>% pull(USUBJID) %>% unique() 
PT %>% filter(USUBJID %in% ab) %>% count(USUBJID, PTTRT) %>% print(n = Inf) # 

# IN ----
IN %>% count()
IN %>% names()
IN %>% count(VISIT, INTRT, INDOSRGM, INSTDY, INENDY, INDUR) %>% print(n = Inf)
IN %>% count_dup(VISITDY, INTRT, INDOSRGM)

# it appears that patients with VISIT == "SCREENING", the IN information relates to previous treatments
# otherwise, the treatments seem to correlate with this episode: VISIT == "Day 0"
# pivot IN to get a better understanding of previous treatments

IN %>% count_dup(VISIT) # 4 USUBJIDs with two SCREENING entries
IN_pivot <- IN %>% arrange(USUBJID, INSTDY, INDUR) %>% group_by(USUBJID) %>% mutate(pivot = row_number())
IN_pivot <- IN_pivot %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = pivot,
    values_from = c(INTRT, INSTDY, INDUR),
    names_glue = "IN_{pivot}_{.value}"
  )

# for the 'SCREENING' VISIT entries, INSTDY is always negative or NA (8 entries are NA)
IN_pivot %>% 
  relocate(USUBJID, IN_1_INTRT, IN_1_INSTDY, IN_1_INDUR, IN_2_INTRT, IN_2_INSTDY, IN_2_INDUR, IN_3_INTRT, IN_3_INSTDY, IN_3_INDUR, IN_4_INTRT, IN_4_INSTDY, IN_4_INDUR) %>% 
  arrange(IN_1_INTRT, IN_1_INSTDY, IN_1_INDUR, IN_2_INTRT, IN_2_INSTDY, IN_2_INDUR, IN_3_INTRT, IN_3_INSTDY, IN_3_INDUR, IN_4_INTRT, IN_4_INSTDY, IN_4_INDUR) #%>% View()

# from IN we can tag patients as either having previous treatment, and the name of their first and second treatments
IN_relapse <- IN %>% filter(VISIT == "SCREENING") %>% 
  pull(USUBJID) %>% unique()

IN_relapse <- tibble(
  USUBJID = IN_relapse,
  IN_MH_VL = TRUE
)

IN_clean <- IN %>% filter(VISIT != "SCREENING") %>% arrange(INSTDY)  %>% 
  group_by(USUBJID) %>% mutate(n_dup = row_number()) %>% ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = n_dup,
    values_from = c(INTRT, INSTDY),
    names_glue = "IN_{n_dup}_{.value}"
  ) %>% 
  select(-IN_1_INSTDY) %>% 
  full_join(IN_relapse) %>% 
  arrange(IN_MH_VL, IN_1_INTRT, IN_2_INTRT, IN_2_INSTDY) 

IN_clean <- IN_clean %>% 
  mutate(
    across(
      c(IN_1_INTRT, IN_2_INTRT),
      ~ case_when(
        .x == "Amphotericin B" ~ "AMBDC",
        .x == "SAG" ~ "SSG",
        .x == "Miltefosine" ~ "MILT",
        .default = NA)
    )
  )

IN_clean <- IN_clean %>% 
  mutate(
    IN_TRT = ifelse(is.na(IN_2_INTRT), IN_1_INTRT, str_c(IN_1_INTRT, "_", IN_2_INTRT, "_D", IN_2_INSTDY)
    )
  ) %>% 
  select(USUBJID, IN_MH_VL, IN_TRT)

# there are 198 patients started on SSG 
# there are 24 patients started on AMB
# there are 2 patients where starting drug is not documented in IN
# there are 24 patients with previous VL treatment documented
IN_clean %>% count(IN_TRT) %>% print(n = 50)
IN_clean %>% count(IN_MH_VL)

# DM ----
DM %>% count()
DM %>% names()

DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = Inf) # 1.5 - 75 years

DM %>% count(ARMCD)

DM_clean <- DM %>% 
  mutate(
    DM_ARM = ARMCD,
    DM_SEX = SEX,
    DM_AGE = AGE,
    DM_SITE = SITEID
  ) %>% 
  full_join(IN_clean) %>% 
  select(USUBJID, RFSTDTC, starts_with("IN_"), starts_with("DM_"))

# DM and IN agree w.r.t. initial treatment choice
DM_clean %>% count(IN_TRT, DM_ARM) %>% print(n = Inf)
VGKSTG <- DM_clean # leave IN_TRT in for now because retreatment details can help understand outcomes

# OUTCOMES ----

DD %>% names()
DD %>% count(VISIT, DDTEST, DDDY)
DD %>% count_dup()

DM %>% 
  mutate(
    DTHDY = dmy(DTHDTC) - dmy(RFSTDTC)
  ) %>% 
  count(DTHFL, DTHDY) %>% print(n = 40)
DM_pivot <- DM %>% 
  mutate(
    DTHDY = dmy(DTHDTC) - dmy(RFSTDTC)
  ) %>% 
  select(USUBJID, DTHFL, DTHDY)

RS %>% names()
RS %>% count_dup(VISIT, RSSCAT) 
RS %>% count(VISIT, RSSCAT, RSORRES) %>% print(n = Inf)

RS %>% count_dup(VISIT, RSSCAT) %>% print(n = Inf)
RS_pivot <- RS %>% 
  mutate(
    VISIT = str_c(VISIT, " ", ""),
    RSSCAT = ifelse(is.na(RSSCAT), "NA", "AOP")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, RSSCAT),
    values_from = RSORRES,
    names_glue = "RS_{VISIT}_{RSSCAT}")

# almost all USUBJIDs have two entries for VISIT == "DAY 360"
DS %>% names()
DS %>% count()
DS %>% count_dup(VISIT)

DS_pivot <- DS %>% group_by(USUBJID, VISIT) %>% 
  arrange(USUBJID, DSSEQ) %>% 
  mutate(n_dup = row_number()) %>% 
  ungroup() %>% 
  mutate(VISIT = str_replace_all(VISIT, " ", "")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, n_dup),
    values_from = DSTERM,
    names_glue = "DS_{VISIT}_{n_dup}"
  ) %>% relocate(USUBJID, DS_SCREENING_1)

RS_pivot %>% names()
IN_clean %>% names()

OUT <- IN_clean %>% full_join(DM_pivot) %>% full_join(RS_pivot) %>% full_join(DS_pivot)

OUT %>% count(IN_TRT) %>% print(n = 90)
OUT %>% count()
OUT %>% count(IN_TRT)

# We have 6 month outcomes (n = 198), so code according to these outcomes
# restrict to those treated with SSG as these can correlate with publication
OUT  %>% filter(IN_TRT != "AMBDC") %>% select(-DS_SCREENING_1, -IN_MH_VL) #%>% View()

# of those patients that died, it seems that the DTHDY is not reliable
OUT_clean <- OUT %>% 
  mutate(
    OUT_IC_DRUG = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DEATH = case_when(
      USUBJID %in% c("VGKSTG_BPKIHS_113", "VGKSTG_BPKIHS_201", "VGKSTG_BPKIHS_67", "VGKSTG_BPKIHS_266", "VGKSTG_BPKIHS_172") ~ TRUE, # all these patients died during treatment (SSG)
      USUBJID %in% c("VGKSTG_BPKIHS_74", "VGKSTG_BPKIHS_11") ~ TRUE, # both AMBDC patients
      USUBJID == "VGKSTG_BPKIHS_165" ~ TRUE, # this patient died before 30 day assessment, but no treatment arm details
      USUBJID == "VGKSTG_BPKIHS_164" ~ TRUE, # confirmed deceased after initial cure assessment, but did not achieve IC, so consider to be still IC-DC period (DTHDY is 32 days)
      .default = FALSE
    ),
    OUT_DC_DEATH = case_when(
      USUBJID == "VGKSTG_BPKIHS_252" ~ TRUE, # AMBDC patient, initial cure and then died (?D21 death)
      .default = FALSE
    ),
    OUT_NA = FALSE,
    OUT_IC_OTHER = case_when(     
      USUBJID %in% c(
        "VGKSTG_BPKIHS_213", # defaulted
        "VGKSTG_BPKIHS_7", # non-responder at 30 day assessment
        "VGKSTG_BPKIHS_86", # all RS outcomes are NA, no initial cure documented
        "VGKSTG_BPKIHS_9", # non-responder at 30 day assessment
        #"VGKSTG_BPKIHS_164", # non-responder (already included as OUT_IC_DEATH)
        "VGKSTG_BPKIHS_57", # switched to AMBDC at D11 (and relapse between 180 and 360 day assessment it seems)
        "VGKSTG_BPKIHS_95", # switched to AMBDC at D11
        "VGKSTG_BPKIHS_51", # switched to AMBDC at D12 (and looks like patient died after 180 day assessment and before 360 day assessment)
        "VGKSTG_BPKIHS_180", # switched to AMBDC at D12
        "VGKSTG_BPKIHS_141", # switched to AMBDC at D14
        "VGKSTG_BPKIHS_4", # switched to AMBDC at D16
        "VGKSTG_BPKIHS_60", # switched to AMBDC at D17
        "VGKSTG_BPKIHS_56", # switched to AMBDC at D17
        "VGKSTG_BPKIHS_275", # switched to AMBDC at D30, non-responder
        "VGKSTG_BPKIHS_221", # switched to AMBDC at D30, non-responder
        "VGKSTG_BPKIHS_153", # switched to AMBDC at D33, non-responder
        "VGKSTG_BPKIHS_173", # switched to AMBDC at D33, non-responder
        "VGKSTG_BPKIHS_257", # switched to AMBDC at D34, let's consider this a non-responder (although states initial cure) 
        "VGKSTG_BPKIHS_161", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_104", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_115", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_226", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_154", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_218", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_227", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_5", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_6", # switched to AMBDC at D8 
        "VGKSTG_BPKIHS_292", # switch to AMBDC and 'non-responder'
        "VGKSTG_BPKIHS_190" # NOT SSG (IN_TRT is NA) - defauled before 30 day assessment
        ) ~ TRUE,
      .default = FALSE
    ),
    OUTC_IC_OTHER = "multiple reasons: including medication switching, defaulted treatment, non-responder",
    OUT_DC_OTHER = case_when(
      USUBJID %in% c(
        "VGKSTG_BPKIHS_73",  # LTFU
        "VGKSTG_BPKIHS_238", # LTFU
        "VGKSTG_BPKIHS_175", # LTFU
        "VGKSTG_BPKIHS_199", # LTFU
        "VGKSTG_BPKIHS_211", # LTFU 
        "VGKSTG_BPKIHS_295", # LTFU
        "VGKSTG_BPKIHS_78",  # LTFU
        "VGKSTG_BPKIHS_102", # LTFU,
        "VGKSTG_BPKIHS_205", # LTFU (AMBDC)
        "VGKSTG_BPKIHS_280" # looks like LTFU after 3 months according to DSTERM text entry
      ) ~ TRUE,
    .default = FALSE
    ),
  OUTC_DC_OTHER = "all 10 are LTFU",
  OUT_DC_RELAPSE = case_when(
    USUBJID %in% c(
      "VGKSTG_BPKIHS_181" # relapse on D209 (both RS and DS domains, restarted AMBDC D211) - therefore let's include this (see below for other relapses)
  ) ~ TRUE,
  .default = FALSE
  ),
  OUT_IC = ifelse(OUT_IC_DEATH | OUT_IC_OTHER, FALSE, TRUE),
  OUT_DC = ifelse(OUT_DC_RELAPSE | OUT_DC_OTHER | OUT_DC_DEATH | !OUT_IC, FALSE, TRUE)
  ) %>% 
  select(USUBJID, starts_with("OUT"))

OUT_clean %>% names()
OUT_clean %>% count(across(starts_with("OUT_")))%>% mutate(across(everything(), ~as.character(.x))) %>% print(n = Inf, width = Inf) #%>% View()

RS %>% filter(USUBJID == "VGKSTG_BPKIHS_280") #%>% View()
DS %>% filter(USUBJID == "VGKSTG_BPKIHS_280") #%>% View()

# can confirm that information in DD agrees with death data we have  already (one patient died after 6 months so not included as outside the 6 month point)
OUT_clean %>% full_join(DD %>% select(USUBJID, DDORRES, DDDY))  %>% relocate(starts_with("DD")) %>% arrange(DDORRES) #%>% View()

VGKSTG <- OUT_clean %>% select(starts_with("OUT"), USUBJID) %>% full_join(VGKSTG) 

# RELAPSES NOT INCLUDED

# VGKSTG_BPKIHS_57 relapsed D363 (not included here)
# VGKSTG_BPKIHS_80 relapsed D304 (started AMBDC D336) (not included here)
# VGKSTG_BPKIHS_177 relapsed D295 (AMBDC)
# VGKSTG_BPKIHS_158 relapsed D350 (AMBDC)
# VGKSTG_BPKIHS_243 relapsed D366 (AMBDC)
# VGKSTG_BPKIHS_35 relapsed D369 (AMBDC)
# VGKSTG_BPKIHS_118 relapsed D375 (AMBDC)
# VGKSTG_BPKIHS_135 relapsed D359 (AMBDC)
# VGKSTG_BPKIHS_149 probable relapse D312 (AMBDC) - timing uncertain because 90 day assessment is NA, 180 assessment is 'not come'
# VGKSTG_BPKIHS_132 probable relapse D360 (AMBDC)
# VGKSTG_BPKIHS_163 relapse D451 (AMBDC)
# VGKSTG_BPKIHS_88 relapse D362 (AMBDC)

VGKSTG # %>% View()
VGKSTG %>% count(IN_TRT, across(starts_with("OUT_"))) %>% mutate(across(everything(), ~as.character(.x))) # %>% View()

# SA ----

SA %>% names()
SA %>% count(VISIT, SATERM, SAPRESP, SAOCCUR, !is.na(SADUR)) %>% print(n = Inf)
SA %>% count_dup(VISIT, SATERM) %>% print(n = Inf)

SA %>% filter(VISIT == "SCREENING" & SATERM == "Fever") %>% count(SADUR) %>% print(n = Inf)

SA_merge <- SA %>% 
  mutate(
    VISIT = case_when(
      VISIT == "SCREENING" ~ "BL",
      VISIT == "DAY 30" ~ "IC",
      .default = "other"
    ),
    SATERM = case_when(
      SATERM == "weight loss" ~ "WL",
      SATERM == "vomiting" ~ "VOM",
      SATERM == "loss of appetite" ~ "APP",
      SATERM == "diarrhoea" ~ "DIARR",
      SATERM == "cough" ~ "CGH",
      SATERM == "Skin Darkening" ~ "SKINDK",
      SATERM == "Fever" & VISIT == "BL" ~ "FEV_DUR",
      SATERM == "Fever" & VISIT == "IC" ~ "FEV"
    ),
    SARESULT = case_when(
      !is.na(SAOCCUR) & SAOCCUR == "Y" ~ TRUE,
      !is.na(SAOCCUR) & SAOCCUR == "N" ~ FALSE,
      .default = NA
    ),
    SARESULT = case_when(
      SATERM %in% c("FEV", "CGH", "DIARR", "APP", "VOM", "WL") ~ TRUE,
      is.na(SATERM) ~ NA,
      .default = SARESULT
    )
  )

SA_merge %>% count(SATERM)
SA_merge_fev_dur <- SA_merge %>% 
  filter(SATERM == "FEV_DUR" & VISIT == "BL") %>% 
  select(
    USUBJID,
    SA_HX_FEV_DUR = SADUR
  )

SA_merge_rest <- SA_merge %>% 
  filter(!is.na(SATERM)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, SATERM),
    values_from = SARESULT,
    names_glue = "SA_{VISIT}_{SATERM}"
  )

SA_merge %>% count_dup(VISIT, SATERM)

SA_merge_fev_dur %>% names()
SA_merge_rest %>% names()

SA_final <- SA_merge_fev_dur %>% full_join(SA_merge_rest) %>% select(-SA_BL_FEV_DUR)
SA_final #%>% View()

SA #%>% View()

VGKSTG <- VGKSTG %>% full_join(SA_final)

# LB ----

# have a range of blood tests at baseline, but only HGB at Day 30
LB %>% names()
LB %>% count(VISIT, LBTESTCD, !is.na(LBSTRESN), LBDY) %>% print(n = Inf)
LB %>% count_dup(VISIT, LBTESTCD) # ready to pivot

LB_pivot <- LB %>% 
  mutate(
    VISIT = ifelse(VISIT == "DAY 0", "BL", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_pivot2 <- LB %>% 
  mutate(
    VISIT = ifelse(VISIT == "DAY 0", "BL", "IC")
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBDY,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

LB_pivot %>% dim()
LB_pivot %>% names()
LB_pivot %>% count(LB_IC_HGB) %>% print(n = Inf)
LB_pivot2 %>% select(contains("HGB")) %>% mutate(DIFF = LB_IC_HGB - LB_BL_HGB) %>% count(DIFF) %>% print(n = Inf)

VGKSTG <- VGKSTG %>% full_join(LB_pivot)
VGKSTG %>% names()

# MB ----
MB %>% names()
MB %>% count_dup2(VISIT, MBTEST, MBMETHOD, MBTSTDTL, MBLOC)

MB %>% filter(
  VISIT == "DAY 0",
  MBTEST == "Leishmania",
  MBMETHOD == "MICROSCOPY",
  MBLOC == "BONE MARROW"
) #%>% View() # need to also tabulate by MBTSTDTL

MB %>% count_dup2(VISIT, MBTEST, MBMETHOD, MBTSTDTL, MBLOC)

MB %>% filter(VISIT == "DAY 0") %>% count(MBTESTCD, MBMETHOD, MBTSTDTL, MBLOC, MBORRES) #%>% View()
MB_pivot <- MB %>%  
  mutate(across(c(VISIT, MBTESTCD, MBMETHOD, MBLOC), ~str_replace_all(.x, " ", "_"))) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MBTESTCD, MBMETHOD, MBTSTDTL, MBLOC),
    values_from = MBORRES
  )



# it appears all patients had bone marrow aspirate (detection and quantification)
# for the 10 patients with negative bone marrow aspirate, they were positive either in splenic aspirate (n = 5) or on leishmania culture (n = 6)
# only have 5 patients with splenic aspirate (i.e. if bone marrow was negative and presumably clinical suspicion persisted)
# all patients also had two leishmania cultures: culture with MBLOC NA for all patients, and culture with MBLOC BONE MARROW for all bar 29 patients
# the leishmania cultures are very often negative, and where positive, are much more likely to be positive in the BONE MARROW culture (perhaps they cultured twice from the BONE MARROW, or also from the blood)
MB_pivot %>% names()
MB_pivot %>% count(DAY_0_LSHMANIA_MICROSCOPY_QUANTIFICATION_BONE_MARROW, DAY_0_LSHMANIA_MICROBIAL_CULTURE_DETECTION_NA,
  DAY_0_LSHMANIA_MICROBIAL_CULTURE_DETECTION_BONE_MARROW,DAY_0_LSHMANIA_MICROSCOPY_DETECTION_SPLEEN) #%>% View()
MB_pivot %>% count(DAY_0_LSHMANIA_MICROSCOPY_DETECTION_BONE_MARROW,DAY_0_LSHMANIA_MICROBIAL_CULTURE_DETECTION_BONE_MARROW,DAY_0_LSHMANIA_MICROBIAL_CULTURE_DETECTION_NA)

# pragmatically; for VISIT == "DAY 0" only extract the BONE MARROW aspirate results (both quantification and detection, because DAY 30 has a NA for quantification and POSITIVE detection))

MB_pivot %>% count(across(starts_with("DAY_30"))) #%>% View()
MB_pivot %>% names()
MB %>% filter(VISIT == "DAY 360") # %>% View() # confirms the relapse on day 209

MB_pivot %>% count(DAY_0_LSHMANIA_MICROSCOPY_DETECTION_BONE_MARROW, DAY_0_LSHMANIA_MICROSCOPY_QUANTIFICATION_BONE_MARROW)


MB_merge <- MB_pivot %>% select(USUBJID, DAY_0_LSHMANIA_MICROSCOPY_DETECTION_BONE_MARROW, DAY_0_LSHMANIA_MICROSCOPY_QUANTIFICATION_BONE_MARROW)
MB_merge <- MB_merge %>%
  rename(
    MB_BL_LSHMANIA_SPLEEN = DAY_0_LSHMANIA_MICROSCOPY_QUANTIFICATION_BONE_MARROW
  ) %>% 
  select(USUBJID, MB_BL_LSHMANIA_SPLEEN)

MB_merge %>% count(MB_BL_LSHMANIA_SPLEEN)

VGKSTG <- VGKSTG %>% full_join(MB_merge)

# MP ----
# spleen lengths
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPSTAT, MPLOC)

MP_merge <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "DAY 0" ~ "BL",
      VISIT == "DAY 30" ~ "IC",
      .default = NA
    )
  ) %>% 
  filter(!is.na(VISIT)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_SPLEEN_LENGTH"
  )

MP_merge2 <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "DAY 0" ~ "BL",
      VISIT == "DAY 30" ~ "IC",
      .default = NA
    )
  ) %>% 
  filter(!is.na(VISIT)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = MPDY,
    names_glue = "MP_{VISIT}_SPLEEN_LENGTH"
  )
MP_merge2 %>% mutate(diff = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) %>% count(diff) %>% print(n = Inf)


VGKSTG <- VGKSTG %>% full_join(MP_merge)
VGKSTG %>% names()

# VS ----
VS %>% names()
VS %>% count(VISIT, VISITDY, EPOCH, VSTEST, VSTESTCD)

# every row has been duplicated
VS %>% dim()
VS %>% unique() %>% dim()
VS_clean <- VS %>% unique() 

VS_clean %>% count_dup(VISIT, VISITDY, EPOCH, VSTEST, VSTESTCD)

VS_pivot <- VS_clean %>% 
  mutate(VISIT = "BL") %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  )



VS_pivot #%>% View()
VGKSTG <- VGKSTG %>% full_join(VS_pivot)

# HO ----
# these are hospital admission dates
HO %>% names()

VGKSTG <- VGKSTG %>% select(-IN_TRT)
save(VGKSTG, file = "Study/Data/VGKSTG.RData")

VGKSTG %>% names()