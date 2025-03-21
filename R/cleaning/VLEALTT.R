###########
# VLEALTT #
###########

# PUBLICATION

# Pandey K, Pal B, Siddiqui NA, Rabi Das VN, Murti K, Lal CS, Verma N, Babu R, Ali V, Kumar R, Das P. 
# Efficacy and Safety of Liposomal Amphotericin B for Visceral Leishmaniasis in Children and Adolescents at a Tertiary Care Center in Bihar, India. 
# Am J Trop Med Hyg. 2017 Nov;97(5):1498-1502. doi: 10.4269/ajtmh.17-0094. Epub 2017 Oct 10. PMID: 29016288; PMCID: PMC5817748.

# n = 100, 100% initial cure rate, 2 subsequent relapses, 3 LTFU

# DATASET

# N = 100, each domain is fully populated
# OUTCOMES are clear
# there are no symptoms/signs
# there are no baseline parasite counts
# there is no RFSTDTC!

rm(list = ls())
source("definitions.R")
load_domains("VLEALTT")

mg <- ld_missingness("VLEALTT")
mg %>%
  count_na() %>%
  print(width = Inf)

TS #%>% View()

# DM ----
# no timing variables! (Should probably let Gemma know)

DM %>% names()
DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = 50)
DM %>% count(ARMCD)

DM_merge <- DM %>%  
  rename(
    DM_SEX = SEX,
    DM_ARM = ARMCD,
    DM_SITE = SITEID,
    DM_AGE = AGE) %>% 
  select(
    USUBJID,
    starts_with("DM_")
  )

# OUTCOME ----
# this is very clear!
DS %>% names()
DS %>% count(VISIT, DSTERM, DSDECOD, DSTPT, DSTPTREF)

DS %>% count_dup(VISIT)
DS_merge <- DS %>% 
    mutate(
      VISIT = case_when(
        is.na(VISIT) ~ "NA",
        !is.na(VISIT) & VISIT == "6 Months" ~ "DC",
        !is.na(VISIT) & VISIT == "Day 30" ~ "IC",
        .default = NA
      )
    ) %>% pivot_wider(
      id_cols = USUBJID,
      names_from = VISIT,
      values_from = DSTERM, 
      names_glue = "DS_{VISIT}"
    )

RS %>% names()
RS %>% count(VISIT, RSTESTCD, RSTEST, RSORRES, RSTPT, RSTPTREF)
RS %>% count_dup(VISIT)
RS_merge <- RS %>% 
    mutate(
      VISIT = case_when(
        is.na(VISIT) ~ "NA",
        !is.na(VISIT) & VISIT == "6 Months" ~ "DC",
        !is.na(VISIT) & VISIT == "Day 30" ~ "IC",
        .default = NA
      )
    ) %>% 
    select(USUBJID, VISIT, RSORRES) %>% 
    pivot_wider(
      id_cols = USUBJID,
      names_from = VISIT,
      values_from = RSORRES, 
      names_glue = "RS_{VISIT}"
    )

OUT_preview <- RS_merge %>% full_join(DS_merge)
OUT_preview #%>% View()

OUT <- OUT_preview %>% 
  mutate(
    OUT_NA = FALSE,
    OUT_XX_DEATH = FALSE,
    OUT_XX_OTHER = FALSE,
    OUT_IC_DRUG = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_IC = TRUE,
    OUT_IC_DEATH = FALSE,
    OUT_DC_RELAPSE = ifelse(DS_NA == "Relapse" & !is.na(DS_NA), TRUE, FALSE),
    OUT_DC_OTHER = ifelse(DS_NA == "Lost to Follow Up" & !is.na(DS_NA), TRUE, FALSE),
    OUTC_DC_OTHER = "3 patients were LTFU, also correlates with publication",
    OUT_DC_DEATH = FALSE,
    OUT_DC = ifelse(OUT_DC_RELAPSE | OUT_DC_OTHER, FALSE, TRUE)
    ) %>%  
  select(USUBJID, starts_with("OUT"))

OUT %>% count(across(starts_with("OUT_"))) # %>% View()

VLEALTT <- DM_merge %>% full_join(OUT)
VLEALTT # %>% View()

# IN/PT ----
# IN documents the patients with a history of previous VL treatment (n = 13 here, but n = 14 stated in the publication)
# PT is just the planned treatment
IN #%>% View() 
PT # %>% View() 

IN_merge <- IN %>% select(USUBJID) %>% mutate(IN_MH_VL = TRUE)
VLEALTT <- VLEALTT %>% full_join(IN_merge)
VLEALTT %>% count(IN_MH_VL, OUT_DC_RELAPSE)

# SA ----
# only previous VL history here (we identify the extra patient with hx of VL!)
SA %>% count_dup()
SA %>% count(SAOCCUR)
SA %>% count(SAPRESP, SAOCCUR)
SA_merge <- SA %>% 
  mutate(SA_HX_VL = ifelse(SAOCCUR == "Y", TRUE, FALSE)) %>% 
  select(USUBJID, SA_HX_VL)
VLEALTT <- VLEALTT %>% full_join(SA_merge)
VLEALTT %>% count(SA_HX_VL, IN_MH_VL)

# LB ----

LB %>% names() 
LB %>% count(VISIT, LBTESTCD) %>% print(n = Inf)
LB_merge <- LB %>% 
  mutate(VISIT = case_when(
    VISIT == "Day 1" ~ "BL",
    VISIT == "Day 30" ~ "IC",
    .default = NA
  )) %>% 
  filter(!is.na(VISIT)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, LBTESTCD),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("LB_"))

VLEALTT <- VLEALTT %>% full_join(LB_merge)

# MB ----
# we don't have baseline parasite counts! Gemma (ask)
MB %>% names()
MB %>% count(VISIT, MBTESTCD, MBTEST, MBORRES, MBMETHOD)
MB #%>% View()
MB_merge <- MB %>% mutate(MB_BL_LSHMRK39 = TRUE) %>% select(USUBJID, MB_BL_LSHMRK39)
VLEALTT <- VLEALTT %>% full_join(MB_merge)

VLEALTT #%>% View()

# MP ----
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)
MP_merge <- MP %>% 
  mutate(
    VISIT = case_when(
      VISIT == "Day 1" ~ "BL",
      VISIT == "Day 30" ~ "IC",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, MPTESTCD, MPLOC),
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_{MPLOC}_{MPTESTCD}"
  ) %>% 
  select(USUBJID, starts_with("MP_"))

VLEALTT <- VLEALTT %>% full_join(MP_merge)

# VS ----
VS %>% names()
VS %>% count(VISIT, VSTESTCD, VSTEST)
VS %>% count_dup(VISIT, VSTESTCD)
VS_merge <- VS %>% 
  mutate(VISIT = case_when(
    VISIT == "Day 1" ~ "BL",
    VISIT == "Day 30" ~ "IC",
    .default = NA
  )) %>% 
filter(!is.na(VISIT)) %>% 
pivot_wider(
  id_cols = USUBJID,
  names_from = c(VISIT, VSTESTCD),
  values_from = VSSTRESN,
  names_glue = "VS_{VISIT}_{VSTESTCD}"
) %>% 
select(USUBJID, starts_with("VS_"))
VS_merge #%>% View()

VLEALTT <- VLEALTT %>% full_join(VS_merge)

save(VLEALTT, file = "Study/Data/VLEALTT.RData")
