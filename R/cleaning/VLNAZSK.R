# PUBLICATION

# Sundar S, Sinha PK, Verma DK, Kumar N, Alam S, Pandey K, Kumari P, Ravidas V, Chakravarty J, Verma N, Berman J, Ghalib H, Arana B. 
# Ambisome plus miltefosine for Indian patients with kala-azar. Trans R Soc Trop Med Hyg. 
# 2011 Feb;105(2):115-7. doi: 10.1016/j.trstmh.2010.10.008. Epub 2010 Dec 3. PMID: 21129762.

# Very short article - 'short communication' only. 
# of the 135 patients across both RMRIMS and KAMRC: 
# 3 relapses
# 1 death (before initial cure)
# 7 further patients 'not evaluated' (3 not evaluated at initial cure and 4 further patients not evaluated at definitive cure due to various reasons)

# this is the same study listed for VVNGOE 

# DATABASE

# all patients are RMRIMS
# n = 32
# no relapses / deaths are identified
# Gemma - request LB VISIT data if possible

rm(list = ls())
source("definitions.R")
load_domains("VLNAZSK")

mg <- ld_missingness("VLNAZSK")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----

DM %>% names()
DM %>% count(SEX)
DM %>% count(AGE) %>% print(n = Inf)
DM %>% count(SITEID)
DM %>% count(ETHNIC)

DM_merge <- DM %>% 
  select(
    DM_SEX = SEX, 
    DM_SITE = SITEID, 
    DM_AGE = AGE, 
    USUBJID, 
    RFSTDTC, 
    DM_ARM = ARMCD
  )

VLNAZSK <- DM_merge

# OUT ----

# VLNAZSK_PATNA_2013 apparently not assessed for 2 week parasitological cure (RS), but RS and DS specify 
# achieved final cure, and in MB had 1+ parasites on Splenic aspirate on MBDY 36. 

# RS has day 29 clinical response and parasitological response specified seperately.

RS %>% names()
RS %>% count(VISIT, RSCAT, RSSCAT, RSTESTCD)
RS_merge <- RS %>% 
  mutate(
    VISIT = ifelse(VISIT == "6 Months", "DC", "IC"),
    RSSCAT = case_when(
      RSSCAT == "CLINICAL RESPONSE" & !is.na(RSSCAT) ~ "CR",
      RSSCAT == "PARASITOLOGICAL RESPONSE" & !is.na(RSSCAT) ~ "PR",
      is.na(RSSCAT) ~ "OTHER",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, RSSCAT),
    values_from = RSORRES,
    names_glue = "RS_{VISIT}_{RSSCAT}"
  )

RS_merge #%>% View()
RS %>% count_dup()

# DS ----
DS %>% count_dup()
DS %>% count(VISIT, DSTERM)

OUT <- RS_merge %>% full_join(DS %>% select(USUBJID, DSTERM))

# MB
# No VISIT data here
# 7 patients have negative AFB
# 32 patients have negative HIV
# 30 patients have negative malaria
# 1 patient has a negative Salmonella test
MB %>% count()
MB %>% names()
MB %>% count(MBTEST, MBLOC, MBDY) %>% print(n = Inf)
MB %>% count(MBTESTCD, MBLOC, MBDY, MBORRES) %>% print(n = Inf)
MB %>% count(MBTESTCD, MBLOC)

MB %>% filter(MBTESTCD == "LSHMANIA") %>% count(MBLOC, MBORRES, MBDY) %>% print(n = Inf)

MB_merge <- MB %>% filter(MBTESTCD == "LSHMANIA") %>% 
  mutate(MBLOC = ifelse(is.na(MBLOC), "NS", MBLOC))
MB_merge %>% count_dup(MBLOC) # 9 patients have extra spleen aspirates


MB_merge1 <- MB_merge %>% group_by(USUBJID, MBLOC) %>% arrange(USUBJID, MBLOC, MBDY) %>% mutate(n = row_number()) %>% 
  ungroup() 

MB_merge1 %>% count(MBLOC)

MB_merge2 <- MB_merge1 %>% 
  mutate(MBLOC = ifelse(MBLOC == "BONE MARROW", "BONE", MBLOC)) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(MBLOC, n),
    values_from = c(MBORRES, MBDY),
    names_glue = "MB_{MBLOC}_{n}_{.value}"
  )

OUT_merge <- OUT %>% full_join(MB_merge2) 
OUT_merge #%>% View()

# NB: I have included patients with NA, or not assessed for parasitological cure at initial cure assessment, if the final cure is populated and normal.
OUT_merge2 <- OUT_merge %>% 
  mutate(
    OUT_XX_DEATH = FALSE,
    OUT_IC_OTHER = FALSE,
    OUT_IC_DEATH = FALSE,
    OUT_XX_OTHER = is.na(RS_DC_OTHER),
    OUTC_XX_OTHER = "1 patient has DSTERM: 'Droped out due to withdrawl of consent ,due to his own reason ,not related to drug', and 1 other patient has DSTERM: 'Lost 6 months follow up'. Both patients have 'NA' as parasitological response.",
    OUT_IC_DRUG = FALSE,
    OUT_NA = FALSE,
    OUT_DC_DEATH = FALSE,
    OUT_DC_RELAPSE = FALSE,
    OUT_DC_OTHER = FALSE,
    OUT_IC = ifelse(OUT_XX_OTHER, FALSE, TRUE),
    OUT_DC = ifelse(!OUT_IC, FALSE, TRUE)
  ) %>%
  select(USUBJID, starts_with("OUT"), MB_BL_LSHMANIA_SPLEEN = MB_SPLEEN_1_MBORRES)

VLNAZSK <- VLNAZSK %>% full_join(OUT_merge2)

# IN ----
# no relapse data
IN %>% names()
IN %>% count(INCAT, INTRT)

# PT ----
PT %>% names()
PT %>% count(VISIT, PTTRT)

# LB ----
# no VISIT data; need to wait for VISIT data before we incorperate * GEMMA * - need to just do the best I can
LB %>% names()
LB %>% count(LBTESTCD)
LB %>% count(LBTESTCD, LBDY) %>% print(n = Inf)
LB %>% count_dup(LBTESTCD) %>% arrange(LBTESTCD, count_col) %>% print(n = Inf)
LB %>% filter(LBTESTCD == "HGB") %>% count(LBDY) %>% print(n = Inf)

LB_test <- LB %>% filter(LBTESTCD == "WBC") %>% 
  arrange(USUBJID, LBTESTCD, LBDY) %>% 
  group_by(USUBJID, LBTESTCD) %>% 
  mutate(test_order = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(LBTESTCD, test_order),
    values_from = LBDY,
    names_glue = "LB_{LBTESTCD}_{test_order}"
  )
LB_test #%>% View()

# Can see there aren't many blood tests performed at 30 days
# baseline blood tests are LBDY 3-8 (inclusive)
# So let's use blood tests after treatment (days 15-22)
LB_plot <- LB %>% filter(LBDY < 40) %>% 
  ggplot() + 
  geom_histogram(
    aes(x = LBDY),
    binwidth = 1
  ) + 
  facet_wrap(~LBTESTCD)
LB_plot

# screening bloods are usually present here
# looks like baseline blood tests are LBDY from 3 to 8 days
# looks like 1 month blood tests are LBDY from 28 to 33 days
LB %>% count(is.na(LBDY))
LB_clean <- LB %>% filter((LBDY >= 3 & LBDY <= 8) | (LBDY >= 30 & LBDY <= 38)) %>% 
  mutate(
    VISIT = ifelse(LBDY >= 3 & LBDY <= 8, "BL", "IC")
  ) %>% 
  group_by(USUBJID, LBTESTCD, VISIT) %>% arrange(LBDY) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n == 1)

LB_clean %>% count(USUBJID, VISIT, LBTESTCD, LBDY)  %>% arrange(USUBJID, VISIT, LBTESTCD, LBDY) #%>% View()
#LB_clean %>% View()  
  
LB_merge <- LB_clean %>% 
  filter(LBTESTCD != "HCG") %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(LBTESTCD, VISIT),
    values_from = LBSTRESN,
    names_glue = "LB_{VISIT}_{LBTESTCD}"
  )

preg_neg <- c("VLNAZSK_PATNA_2004", "VLNAZSK_PATNA_2005", "VLNAZSK_PATNA_2009", "VLNAZSK_PATNA_2019", "VLNAZSK_PATNA_2024")

LB_merge <- LB_merge %>% 
  mutate(LB_BL_HCG = ifelse(USUBJID %in% preg_neg, FALSE, NA))
LB_merge #%>% View()
VLNAZSK <- VLNAZSK %>% full_join(LB_merge)

# SA ----
# a number of symptoms (cough, weakness, rigor (to some extent)), are just NA, uncertain what this means. 
SA %>% names()
SA %>% count(VISIT, SACAT, SATERM, SAOCCUR, !is.na(SARPOC))
SA %>% filter(is.na(SACAT) & SATERM == "Fever") #%>% View()
SA %>% filter(SATERM == "Cough") #%>% View()

SA_merge1 <- SA %>% filter(SACAT == "MEDICAL HISTORY" & SATERM == "Fever") %>% select(USUBJID, SA_HX_FEV_DUR = SARPOC)
SA_merge2 <- SA %>% filter(SATERM == "Riger") %>% 
  select(USUBJID, SA_HX_RGR = SAOCCUR) %>% 
  mutate(SA_HX_RGR = ifelse(is.na(SA_HX_RGR), NA, TRUE))
SA_merge3 <- SA %>% filter(SATERM == "Vomiting") %>% select(USUBJID, SA_HX_VOM = SAOCCUR) %>% 
  mutate(SA_HX_VOM = ifelse(SA_HX_VOM == "Y", TRUE, FALSE))

VLNAZSK %>% full_join(SA_merge1) %>% full_join(SA_merge2) %>% full_join(SA_merge3) #%>% names()
VLNAZSK <- VLNAZSK %>% full_join(SA_merge1) %>% full_join(SA_merge2) %>% full_join(SA_merge3)

# MP, RP, VS

# MP ----
MP %>% names()
MP %>% count(VISIT, MPTESTCD, MPLOC)

MP_merge <- MP %>% 
  filter(VISIT %in% c("Screening", "Day 29")) %>% 
  mutate(VISIT = ifelse(VISIT == "Screening", "BL", "IC")) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = VISIT,
    values_from = MPORRES,
    names_glue = "MP_{VISIT}_SPLEEN_LENGTH"
  )

MP_merge %>% print(n = 50)
VLNAZSK <- VLNAZSK %>% full_join(MP_merge)

# VS ----
VS %>% names()
VS %>% count(VISIT, VSTESTCD, VSDY, VSEVINTX) %>% print(n = 500)

VS %>% names()

VS_merge <- VS %>% 
  mutate(
    VISIT = ifelse(is.na(VISIT) & VSDY %in% c(-5, 1), "BL", VISIT)
  )

VS_merge %>% count(VISIT, VSTESTCD, VSEVINTX) %>% print(n = 500)
VS_merge %>% count_dup(VISIT, VSTESTCD) %>% print(n = Inf)

VS_merge1 <- VS_merge %>% filter(!is.na(VISIT)) %>% 
  filter(VISIT %in% c("BL", "Day 1", "Day 29")) %>%
  filter(!(VISIT == "BL" & VSTESTCD != "WEIGHT")) %>%
  mutate(
    VISIT = case_when(
      VISIT %in% c("BL", "Day 1") ~ "BL",
      VISIT == "Day 29" ~ "IC",
      .default = "ERROR"
    )
  ) %>% 
  pivot_wider(
    id_cols = USUBJID,
    names_from = c(VISIT, VSTESTCD),
    values_from = VSSTRESN,
    names_glue = "VS_{VISIT}_{VSTESTCD}"
  ) 

VS %>% count(VSTESTCD)
VS_merge2 <- VS %>% filter(VSTESTCD == "HEIGHT") %>% 
  select(USUBJID, VS_BL_HEIGHT = VSSTRESN)

VLNAZSK <- VLNAZSK %>% full_join(VS_merge1) %>% full_join(VS_merge2)

# RP ----
RP %>% names()
RP %>% count(RPTESTCD, RPORRES)
neg <- RP %>% pull(USUBJID) %>% unique()
VLNAZSK <- VLNAZSK %>% 
  mutate(RP_PREG = ifelse(USUBJID %in% neg, FALSE, NA))

# SAVE ----
save(VLNAZSK, file = "Study/Data/VLNAZSK.RData")

