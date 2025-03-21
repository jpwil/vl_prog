###########
# clean.R #
###########

# this R script takes the merged dataset ("Analysis/ads.RData") and cleans

# - review distributions
# - harmonise units 
# - remove obvious outliers

rm(list = ls())
source("R/definitions.R")
load("data/ads.RData")  # loads one object called ads
library(readxl)

################
## EXCLUSIONS ##
################

# exclude studies with uncertain relapse outcomes
ads %>% count()
ads_clean1 <- ads %>% 
  filter(!(STUDYID %in% c("VLNXMEA", "VZUYLH"))) # exclude studies with uncertain relapse outcomes

# exclude patients with uncertain initial cure, HIV positive and pregnancy
ads_clean1 %>% count(OUT_IC, RP_PREG, MB_BL_HIVAB, !OUT_DC_DEATH) %>% print(n = Inf)

ads_clean1 <- ads_clean1 %>% 
  filter(
    OUT_IC, 
    MB_BL_HIVAB == FALSE | is.na(MB_BL_HIVAB),
    RP_PREG == FALSE | is.na(RP_PREG),
    !OUT_DC_DEATH
    ) 


################
## IC TIMINGS ##
################

ads_clean1_5 <- ads_clean1 %>% 
  mutate(
    IC_DAYS = case_when(
      STUDYID == "VAQMOU" ~ 19,
      STUDYID == "VDXALE" ~ 29,
      STUDYID == "VEZMZD" ~ 16,
      STUDYID == "VFEFCS" & DM_ARM == "PARO14" ~ 14,
      STUDYID == "VFEFCS" & DM_ARM == "PARO21" ~ 21,
      STUDYID == "VFETIZ" ~ 28,
      STUDYID == "VFFFOP" ~ 30,
      STUDYID == "VGKSTG" ~ 35,
      STUDYID == "VIVXJN" ~ 29,
      STUDYID == "VIZGFA" ~ 30,
      STUDYID == "VLAULV" ~ 29,
      STUDYID == "VLEALTT" ~ 31,
      STUDYID == "VLNAZSK" ~ 30,
      STUDYID == "VLZUKHR" ~ 29,
      STUDYID == "VQKRHN" ~ 17,
      STUDYID == "VRBQIF" ~ 30,
      STUDYID == "VSGPDL" ~ 30,
      STUDYID == "VVNGOE" ~ 30,
      STUDYID == "VWPJRM" & DM_ARM == "AMBDC" ~ 32,
      STUDYID == "VWPJRM" & DM_ARM == "PARO21" ~ 23,
      STUDYID == "VYDSGR" ~ 29
    )
  ) 

################################
# MERGE WITH TREATMENT DETAILS #
################################


ads_clean1_5 %>% count(DM_ARM, STUDYID) %>% print(n = Inf)

treat_temp <- read_excel(
    path = "data/treatment details.xlsx",
    sheet = "Sheet1",
    range = "C4:L37",
    trim_ws = TRUE 
)

treat_temp #%>% View()

treat_temp %>% count(DM_ARM) %>% print(n = Inf)
ads_clean1_5 %>% count(DM_ARM) %>% print(n = Inf) #SAGHOSP

ads_clean1_6 <- full_join(treat_temp, ads_clean1_5, by = c("STUDYID", "DM_ARM"))

######################
## TREATMENT GROUPS ##
######################

ads_clean1_6 %>% count(TREAT_DRUG_NAME, TREAT_TEXT, STUDYID) %>% print(n = Inf)
ads_clean1_6 %>% count(TREAT_DRUG_NAME, TREAT_TEXT, STUDYID, OUT_DC_RELAPSE) %>% print(n = Inf)

# group1: all treatment groups are shared between at least two VL datasets (at least two level two clusters)
# group2: removed combination group. created a new 'paromomycin 14D' group (which only exists in one dataset)
# group3: parsimonious group
# reference is set as miltefosine
ads_clean1_7 <- ads_clean1_6 %>% 
  mutate(
    TREAT_GRP1 = case_when(
      TREAT_DRUG_NAME == "Ampho."                           ~ "Ampho.",
      TREAT_DRUG_NAME == "L-AmB" & TREAT_TEXT == "10m 1D"   ~ "SDA",
      TREAT_DRUG_NAME == "MF"                               ~ "MF",
      TREAT_DRUG_NAME == "PM" & TREAT_TEXT == "21D"         ~ "PM",
      TREAT_DRUG_NAME == "SSG"                              ~ "SSG",
      TREAT_TEXT == "5m 1D / M14D"                          ~ "SDA/MF Combo",
      .default = "Other"
    ),
    TREAT_GRP2 = case_when(
      TREAT_DRUG_NAME == "Ampho."                           ~ "Ampho.",
      TREAT_DRUG_NAME == "L-AmB" & TREAT_TEXT == "10m 1D"   ~ "SDA",
      TREAT_DRUG_NAME == "MF"                               ~ "MF",
      TREAT_DRUG_NAME == "PM" & TREAT_TEXT == "21D"         ~ "PM21",
      TREAT_DRUG_NAME == "PM" & TREAT_TEXT == "14D"         ~ "PM14",      
      TREAT_DRUG_NAME == "SSG"                              ~ "SSG",
      .default = "Other"
    ),
    TREAT_GRP3 = case_when(
      TREAT_DRUG_NAME == "Ampho."                           ~ "Ampho.",
      TREAT_DRUG_NAME == "L-AmB" & TREAT_TEXT == "10m 1D"   ~ "SDA",
      TREAT_DRUG_NAME == "MF"                               ~ "MF",
      TREAT_DRUG_NAME == "PM" & TREAT_TEXT == "21D"         ~ "PM21", 
      TREAT_DRUG_NAME == "SSG"                              ~ "SSG", 
      .default = "Other"
    ),
    TREAT_GRP4 = case_when(
      TREAT_DRUG_NAME == "L-AmB" & TREAT_TEXT == "10m 1D"   ~ "SDA",
      TREAT_DRUG_NAME == "MF"                               ~ "MF",
      .default = "Other"
    )      
  ) %>% 
  mutate(
    TREAT_GRP1 = relevel(factor(TREAT_GRP1), ref = "MF"),
    TREAT_GRP2 = relevel(factor(TREAT_GRP2), ref = "MF"),
    TREAT_GRP3 = relevel(factor(TREAT_GRP3), ref = "MF"),
    TREAT_GRP4 = relevel(factor(TREAT_GRP4), ref = "MF")
  )

########
# SITE #
########

ads_clean1_8 <- ads_clean1_7 %>% 
  mutate(
    CLUSTER = paste0(STUDYID, "_", DM_SITE) 
  )

################
## VL_HISTORY ##
################

# STUDYIDs with no information on VL history:
nvh <- c("VAQMOU", "VFEFCS", "VFETIZ", "VFFFOP", "VIVXJN", "VLZUKHR", "VVNGOE", "VLNAZSK")
ads_clean2 <- ads_clean1_8 %>%
  mutate(
    VL_HISTORY = ifelse(
      (is.na(IN_MH_VL) | IN_MH_VL == FALSE) & (is.na(RS_MH_VL) | RS_MH_VL == FALSE) & (is.na(SA_HX_VL) | SA_HX_VL == FALSE), 
      FALSE,
      TRUE
    )
  ) %>% 
  mutate(
    VL_HISTORY = ifelse(STUDYID %in% nvh, NA, as.numeric(VL_HISTORY))
  )
ads_clean2 %>% count(STUDYID, VL_HISTORY, SA_HX_VL, IN_MH_VL, RS_MH_VL) %>% print(n = Inf) #%>% View()
ads_clean2 %>% mutate(VL_HISTORY = as.numeric(VL_HISTORY)) %>% var_sum(VL_HISTORY, group_var = STUDYID)
# systematically missing VL_HISTORY from 
# VAQMOU, VFEFCS, VFETIZ, VFFFOP, VIVXJN, VLZUKHR, VVNGOE, VLNAZSK

# VRBQIF and VLNAZSK have 0% previous relapse -> take a closer look at this
# VLNAZSK -> VL_HISTORY should be NA
# VRBQIF  -> In SA domain, all cases explicitly recorded as no previous relapse

ads_clean2 %>% filter(STUDYID == "VRBQIF") %>% count(VL_HISTORY)
ads_clean2 %>% filter(STUDYID == "VRBQIF") %>% count(IN_MH_VL, RS_MH_VL, SA_HX_VL) # explicitly stated in SA domain
class(ads_clean2$VL_HISTORY)

ads_clean2 %>% var_sum(VL_HISTORY)
ads_clean2 %>% var_sum(VL_HISTORY, STUDYID)

# overall; 11.0% of patients had previous history of VL
# ranges from 0% to 18.4% across STUDYID

##########
## LABS ##
##########

# HGB ----

# BL CORRECTIONS

ads_clean2 %>% var_sum(LB_BL_HGB)
ads_clean2 %>% var_sum(var = LB_BL_HGB, group_var = STUDYID) %>% print(n = Inf)

# 1) VIVXJN: out by a factor of 10 (BL & IC)
# 2) VLNAZSK: two uncertain values, 9090 and 5080. Set 9090 to 90, and 5080 to NA.
# 3) VLZUKHR: there are a number of values > 200 and < 600, assume these are 10x standard units

ads_clean2 %>% filter(STUDYID == "VLNAZSK") %>% select(LB_BL_HGB) %>% print(n = 40)
ads_clean2 %>% filter(STUDYID == "VLZUKHR") %>% select(LB_BL_HGB) %>% print(n = 40)

# IC CORRECTIONS

ads_clean2 %>% var_sum(LB_IC_HGB)
ads_clean2 %>% var_sum(var = LB_IC_HGB, group_var = STUDYID) %>% print(n = Inf)
ads_clean2 %>% filter(STUDYID == "VLZUKHR") %>% filter(!is.na(LB_IC_HGB)) %>% pull(LB_IC_HGB) %>% sort()
ads_clean2 %>% filter(STUDYID == "VWPJRM") %>% pull(LB_IC_HGB) %>% sort()

#ads_clean2 %>% var_sum(LB_IC_HGB - LB_BL_HGB, group_var = STUDYID) 
#ads_clean2 %>% filter(STUDYID == "VFEFCS") %>% select(LB_BL_HGB, LB_IC_HGB) %>% mutate(diff = LB_IC_HGB-LB_BL_HGB) %>% arrange(diff) %>% print(n = Inf)

ads_clean3 <- ads_clean2 %>% 
  mutate(
    LB_BL_HGB = case_when(
      !is.na(LB_BL_HGB) & STUDYID == "VIVXJN" ~ LB_BL_HGB * 10,   
      !is.na(LB_BL_HGB) & STUDYID == "VLNAZSK" & LB_BL_HGB == 9090 ~ 90,
      !is.na(LB_BL_HGB) & STUDYID == "VLNAZSK" & LB_BL_HGB == 5080 ~ NA,
      !is.na(LB_BL_HGB) & STUDYID == "VLZUKHR" & LB_BL_HGB > 200 & LB_BL_HGB < 600 ~ LB_BL_HGB / 10,
      .default = LB_BL_HGB
    ),
    LB_IC_HGB = case_when(
      !is.na(LB_IC_HGB) & STUDYID == "VIVXJN" ~ LB_IC_HGB * 10, 
      !is.na(LB_IC_HGB) & STUDYID == "VLZUKHR" & LB_IC_HGB == 600 ~ NA,
      !is.na(LB_IC_HGB) & STUDYID == "VWPJRM" & LB_IC_HGB == 199.0 ~ NA,
      .default = LB_IC_HGB  
    )
  )

# some plots to eyeball the cleaned variables
hgb_bl <- ads_clean3 %>% ggplot() + 
  geom_histogram(aes(x = LB_BL_HGB)) + 
  facet_wrap(~STUDYID)

hgb_bl_age <- ads_clean3 %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot() + 
    geom_jitter(
      aes(
        x = DM_AGE,
        y = LB_BL_HGB,
        colour = OUT_DC_RELAPSE
      )
    ) + 
    facet_wrap(~STUDYID)

hgb_ic_age <- ads_clean3 %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot() + 
    geom_jitter(
      aes(
        x = DM_AGE,
        y = LB_IC_HGB,
        colour = OUT_DC_RELAPSE
      )
    ) + 
    facet_wrap(~STUDYID)

hgb_diff_age <- ads_clean3 %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot() + 
    geom_jitter(
      aes(
        x = DM_AGE,
        y = LB_IC_HGB - LB_BL_HGB,
        colour = OUT_DC_RELAPSE
      )
    ) + 
    facet_wrap(~STUDYID)

hgb_ic <- ads_clean3 %>% ggplot() + 
  geom_histogram(aes(x = LB_IC_HGB)) + 
  facet_wrap(~STUDYID)

hgb_diff <- ads_clean3 %>% ggplot() + 
  geom_histogram(
    aes(
      x = LB_IC_HGB - LB_BL_HGB
    )
  ) +
  facet_wrap(~STUDYID)

# ggsave("Analysis/hgb/hgb_bl.png", plot = hgb_bl, width = 16, height = 9, dpi = 300) # sudden drop off at HGB = 100 in some studies
# ggsave("Analysis/hgb/hgb_bl_age.png", plot = hgb_bl_age, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/hgb/hgb_ic.png", plot = hgb_ic, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/hgb/hgb_ic_age.png", plot = hgb_ic_age, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/hgb/hgb_diff.png", plot = hgb_diff, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/hgb/hgb_diff_age.png", plot = hgb_diff_age, width = 16, height = 9, dpi = 300) 

ads_clean3 %>% count(STUDYID, LB_BL_HGB >= 165) %>% print(n = Inf)

# WBC ----

# BL CORRECTIONS
 
# VGKSTG and VIVXJN: wrong units, need to divide by 1000
# High of 22.7 for VLAULV is noted, keep for now
# Lows of 0.02 and 0.06 for VEZMZD is noted -> remove these for now 

ads_clean3 %>% var_sum(LB_BL_WBC, group_var = STUDYID) #%>% View()
ads_clean3 %>% filter(STUDYID == "VLAULV") %>% select(LB_BL_WBC) %>% pull(LB_BL_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VEZMZD") %>% select(LB_BL_WBC) %>% pull(LB_BL_WBC) %>% sort()

ads #%>% View()

# IC CORRECTIONS

# VIVXJN: wrong units, need to divide by 1000
# many studies have high IC_WBC.... let's set upper limit to 25
# VFFFOP: one value is 74.0: delete
# VQKRHN: one value is 46.0: delete

ads_clean3 %>% var_sum(LB_IC_WBC, group_var = STUDYID) #%>% View()
ads_clean3 %>% filter(STUDYID == "VLAULV") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VLAULV") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()

# high values
ads_clean3 %>% filter(STUDYID == "VFFFOP") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VQKRHN") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VFETIZ") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VIVXJN") %>% select(LB_IC_WBC) %>% mutate(LB_IC_WBC = LB_IC_WBC/1000) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VEZMZD") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VWPJRM") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort()
ads_clean3 %>% filter(STUDYID == "VLAULV") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort() # keep these values for now
ads_clean3 %>% filter(STUDYID == "VIZGFA") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort() # keep these values for now
ads_clean3 %>% filter(STUDYID == "VYDSGR") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort() 
ads_clean3 %>% filter(STUDYID == "VSGPDL") %>% select(LB_IC_WBC) %>% pull(LB_IC_WBC) %>% sort() 

# make changes

ads_clean4 <- ads_clean3 %>% 
  mutate(
    LB_BL_WBC = case_when(
      STUDYID == "VGKSTG" ~ LB_BL_WBC/1000,
      STUDYID == "VIVXJN" ~ LB_BL_WBC/1000,
      STUDYID == "VEZMZD" & LB_BL_WBC <= 0.06 & !is.na(LB_BL_WBC) ~ NA,
      .default = LB_BL_WBC
    ),
    LB_IC_WBC = case_when(
      STUDYID == "VIVXJN" ~ LB_IC_WBC/1000,
      STUDYID == "VFFFOP" & LB_IC_WBC == 74.0 ~ NA,
      STUDYID == "VQKRHN" & LB_IC_WBC == 46.00 & !is.na(LB_IC_WBC) ~ NA,
      STUDYID == "VFETIZ" & LB_IC_WBC == 87.0 & !is.na(LB_IC_WBC) ~ NA,
      STUDYID == "VEZMZD" & LB_IC_WBC == 91.0 & !is.na(LB_IC_WBC) ~ NA,
      STUDYID == "VWPJRM" & LB_IC_WBC == 83.0 & !is.na(LB_IC_WBC) ~ NA,
      STUDYID == "VIZGFA" & LB_IC_WBC == 83.0 & !is.na(LB_IC_WBC) ~ NA,
      STUDYID == "VYDSGR" & LB_IC_WBC == 44.4 & !is.na(LB_IC_WBC) ~ NA,
      STUDYID == "VSGPDL" & LB_IC_WBC >= 60 & !is.na(LB_IC_WBC) ~ NA,
      .default = LB_IC_WBC
    )
  )

ads_clean4 %>% var_sum(LB_BL_WBC, group_var = STUDYID) %>% arrange(n)
ads_clean4 %>% var_sum(LB_IC_WBC, group_var = STUDYID) %>% arrange(n)

wbc_bl <- ads_clean4 %>% ggplot() + 
  geom_histogram(aes(x = LB_BL_WBC))
wbc_ic <- ads_clean4 %>% ggplot() + 
  geom_histogram(aes(x = LB_IC_WBC))
wbc_diff1 <- ads_clean4 %>% ggplot() + 
  geom_histogram(
    aes(
      x = LB_IC_WBC - LB_BL_WBC
    )
  )
wbc_diff2 <- ads_clean4 %>% ggplot() + 
  geom_histogram(
    aes(
      x = LB_IC_WBC - LB_BL_WBC,
      y = after_stat(density),
      fill = OUT_DC_RELAPSE
    ),
    alpha = 0.5,
    position = "identity"
  )
# wbc_bl
# wbc_ic
# wbc_diff1
# wbc_diff2

# ALT ----

ads_clean4 %>% var_sum(LB_BL_ALT, STUDYID)
ads_clean4 %>% var_sum(LB_IC_ALT, STUDYID)

# BL

# Distributions / ranges all look fine

# IC

ads_clean4 %>% filter(STUDYID == "VEZMZD") %>% pull(LB_IC_ALT) %>% sort() # remove the one high value of 3425

ads_clean5 <- ads_clean4 %>% 
  mutate(LB_IC_ALT = ifelse(LB_IC_ALT == 3425, NA, LB_IC_ALT))

alt_bl <- ads_clean5 %>% ggplot() + 
  geom_histogram(
    aes(
      x = LB_BL_ALT
    )
  ) #+ facet_wrap(~STUDYID)
alt_ic <- ads_clean5 %>% ggplot() + 
  geom_histogram(
    aes(
      x = LB_IC_ALT
    )
  ) #+ facet_wrap(~STUDYID)

alt_diff <- ads_clean5 %>% mutate(diff = LB_IC_ALT - LB_BL_ALT) %>% ggplot() + 
geom_histogram(aes(x = diff)) + facet_wrap(~STUDYID)

# alt_bl
# alt_ic
# alt_diff

ads_clean5 %>% pull(LB_IC_ALT) %>% sort()

# PLAT ----

# BL

# VGKSTG and VIVXJN: platelet counts need to be divided by 1000
# VLEALTT: needs to be multiplied by 100

# IC

# VIVXJN: platelet counts need to be divided by 1000
# VLEALTT: needs to be multiplied by 100
# VEZMZD have two suspiciously low values of 0.11 and 0.22 (remove these)
# VFEFCS has a high outlier of 1850 (remove)

ads_clean5 %>% filter(STUDYID == "VEZMZD") %>% pull(LB_IC_PLAT) %>% sort()
ads_clean5 %>% filter(STUDYID == "VFEFCS") %>% pull(LB_IC_PLAT) %>% sort()


# make changes

ads_clean6 <- ads_clean5 %>% 
  mutate(across(c(LB_BL_PLAT, LB_IC_PLAT), ~ifelse(STUDYID %in% c("VGKSTG", "VIVXJN"), .x/1000, .x))) %>% 
  mutate(across(c(LB_BL_PLAT, LB_IC_PLAT), ~ifelse(STUDYID %in% c("VLEALTT"), .x*100, .x))) %>% 
  mutate(LB_IC_PLAT = ifelse(LB_IC_PLAT < 1 & !is.na(LB_IC_PLAT), NA, LB_IC_PLAT)) %>% 
  mutate(LB_IC_PLAT = ifelse(LB_IC_PLAT == 1850, NA, LB_IC_PLAT))

ads_clean6 %>% var_sum(LB_BL_PLAT, group_var = STUDYID) 
ads_clean6 %>% var_sum(LB_IC_PLAT, group_var = STUDYID) 

# some graphics

plat_bl <- ads_clean6 %>% ggplot() +
geom_histogram(aes(x = LB_BL_PLAT))

plat_ic <- ads_clean6 %>% ggplot() +
geom_histogram(aes(x = LB_IC_PLAT))

plat_diff <- ads_clean6 %>% ggplot() +
geom_histogram(aes(x = LB_IC_PLAT - LB_BL_PLAT)) + facet_wrap(~OUT_DC_RELAPSE)

# plat_bl
# plat_ic
# plat_diff

# CREAT ----

ads_clean6 %>% var_sum(LB_BL_CREAT)
ads_clean6 %>% var_sum(LB_BL_CREAT, group_var = STUDYID)
ads_clean6 %>% var_sum(LB_IC_CREAT, group_var = STUDYID)

ads_clean6 %>% filter(STUDYID == "VDXALE") %>% pull(LB_BL_CREAT) %>% sort()
ads_clean6 %>% filter(STUDYID == "VQKRHN") %>% pull(LB_BL_CREAT) %>% sort()
ads_clean6 %>% filter(STUDYID == "VQKRHN") %>% select(LB_BL_CREAT, LB_IC_CREAT) %>% arrange(LB_IC_CREAT) %>% print(n = Inf)
ads_clean6 %>% filter(STUDYID == "VQKRHN") %>% pull(LB_IC_CREAT) %>% sort()

ads_clean6 %>% filter(STUDYID == "VEZMZD") %>% pull(LB_IC_CREAT) %>% sort()
ads_clean6 %>% filter(STUDYID == "VWPJRM") %>% pull(LB_IC_CREAT) %>% sort()
ads_clean6 %>% filter(STUDYID == "VIZGFA") %>% pull(LB_IC_CREAT) %>% sort()


# VIVXJN need to convert to micromol/litres (x 88.42)

# BL

# VIVXJN need to convert to micromol/litres (x 88.42)
# VDXALE has a low value of 8 (keep for now)
# VQKRHN has a high value of 581.68 (exclude)

# IC

# VIVXJN need to convert to micromol/litres (x 88.42)
# VQKRHN has a high value of 747.88 (exclude - same pt as 581.68 BL value)
# VEZMZD has a high value of 586.10 (keep for now)
# VWPJRM has two high creatinine values (4331.68 and 5480.91) (remove)
# VIZGFA has two high creatinine values (5657.7100 and 7602.5500) (remove)


# make changes

ads_clean7 <- ads_clean6 %>% 
  mutate(across(c(LB_BL_CREAT, LB_IC_CREAT), ~ifelse(STUDYID == "VIVXJN", .x * 88.42, .x))) %>% 
  mutate(LB_BL_CREAT = ifelse(LB_BL_CREAT == 581.68, NA, LB_BL_CREAT)) %>% 
  mutate(LB_IC_CREAT = ifelse(LB_IC_CREAT > 600 & !is.na(LB_IC_CREAT), NA, LB_IC_CREAT))


ads_clean7 %>% var_sum(LB_BL_CREAT, group_var = STUDYID) #%>% View()
ads_clean7 %>% var_sum(LB_IC_CREAT, group_var = STUDYID) #%>% View()


# some plots

creat_bl <- ads_clean7 %>% ggplot() + 
geom_histogram(aes(x = LB_BL_CREAT)) # + facet_wrap(~STUDYID)

creat_ic <- ads_clean7 %>% ggplot() + 
geom_histogram(aes(x = LB_IC_CREAT)) # + facet_wrap(~STUDYID)

creat_diff1 <- ads_clean7 %>% ggplot() + 
geom_histogram(aes(x = LB_IC_CREAT - LB_BL_CREAT)) # + facet_wrap(~STUDYID)

creat_diff2 <- ads_clean7 %>% filter(LB_IC_CREAT < 300) %>% ggplot() + 
geom_histogram(aes(x = LB_IC_CREAT - LB_BL_CREAT)) # + facet_wrap(~STUDYID)

# creat_bl
# creat_ic
# creat_diff1
# creat_diff2

# BILI ----

ads_clean7 %>% var_sum(LB_BL_BILI)
ads_clean7 %>% var_sum(LB_BL_BILI, group_var = STUDYID)

# BL

# VFFFOP: baseline bilirubin rates are in the ordet 100 - 1000 (different patients). These are incorrect, unable to salvage -> delete
# VIVXJN: bilirubin rates are in mg/dL (multiply by 17.1 for micromol/L)
# VGKSTG: outlier of 124.8590 for BL bilirubin -> leave in for now (only baseline available)
# VEZMZD: outlier of 78.68 for BL bilirubin -> leave in for now
# VWPJRM: 5 outliers > 1000 -> reviewed original LB domain and unable to resolve -> remove
# VSGPDL: unexpectedly high values, by comparing original values, looks like JDF-0460 and JDF-0738 are incorrect -> remove

# IC (missing almost 50%)

# VSGPDL has two values > 100; leave in for now
# VIVXJN: bilirubin rates are in mg/dL (multiply by 17.1 for micromol/L)
# VWPJRM: outlier at 1043.34 (remove)
# VLAULV: two high outliers (124.35, 129.82) - leave for now
# VIZGFA: remove two outliers (> 1000)
# VLZUKHR: suspicious range of bilirubin


ads_clean8 <- ads_clean7 %>% 
  mutate(
    LB_BL_BILI = ifelse(STUDYID == "VFFFOP", NA, LB_BL_BILI),
    LB_BL_BILI = ifelse(STUDYID == "VIVXJN", LB_BL_BILI * 17.1, LB_BL_BILI),
    LB_IC_BILI = ifelse(STUDYID == "VIVXJN", LB_IC_BILI * 17.1, LB_IC_BILI),
    LB_BL_BILI = ifelse(STUDYID == "VWPJRM" & LB_BL_BILI >= 1000, NA, LB_BL_BILI),
    LB_BL_BILI = ifelse(STUDYID == "VSGPDL" & LB_BL_BILI %in% c(136.66, 148.80), NA, LB_BL_BILI),
    LB_IC_BILI = ifelse(LB_IC_BILI > 1000 & !is.na(LB_IC_BILI), NA, LB_IC_BILI)
  )

ads_clean8 %>% var_sum(LB_IC_BILI)
ads_clean8 %>% var_sum(LB_IC_BILI, group_var = STUDYID)
ads_clean8 %>% filter(STUDYID == "VSGPDL") %>% pull(LB_BL_BILI) %>% sort()
ads_clean8 %>% filter(STUDYID == "VLNAZSK") %>% pull(LB_IC_BILI) %>% sort()
ads_clean8 %>% filter(STUDYID == "VLZUKHR") %>% pull(LB_IC_BILI) %>% sort()

ads_clean8 %>% count(STUDYID)

# some plots

bili_bl <- ads_clean8 %>% filter(LB_BL_BILI < 100) %>% ggplot() + geom_histogram(aes(x = LB_BL_BILI)) + facet_wrap(~STUDYID)
bili_ic <- ads_clean8 %>% filter(LB_IC_BILI < 100) %>% ggplot() + geom_histogram(aes(x = LB_IC_BILI)) + facet_wrap(~STUDYID)

# bili_bl
# bili_ic

# ALB ----

# units are g/dL (normal range, 3.5-5.5 approximately)
# common units in UK are g/L (normal range 35-55 approximately)

ads_clean8 %>% var_sum(LB_BL_ALB)                        # missing in 45.7%
ads_clean8 %>% var_sum(LB_BL_ALB, group_var = STUDYID)   # and systematically missing in 9 studies

ads_clean8 %>% filter(STUDYID == "VGKSTG") %>% pull(LB_BL_ALB) %>% sort()

# BL

# VGKSTG, VLAULV and VSGPDL values look like they're in g/dL instead of g/L
# VSGPDL also has a low value of 4.3 g/L (0.43 g/dL) - keep this for now

# IC

# VLAULV values are in g/dL
# VLAULV has two very high values (8.48 and 9.06) and a very low value (0.67) - keep for now
# VWPJRM has two very low values (0.38 and 0.83) - keep for now

ads_clean9 <- ads_clean8 %>% 
  mutate(
    LB_BL_ALB = ifelse(STUDYID %in% c("VGKSTG", "VLAULV", "VSGPDL"), LB_BL_ALB / 10, LB_BL_ALB),
    LB_IC_ALB = ifelse(STUDYID == "VLAULV", LB_IC_ALB / 10, LB_IC_ALB)
  )

ads_clean9 %>% var_sum(LB_IC_ALB) # missing in 72.6%
ads_clean9 %>% var_sum(LB_IC_ALB, group_var = STUDYID) # systematically missing in 12 studies  


ads_clean9 %>% filter(STUDYID == "VLAULV") %>% pull(LB_IC_ALB) %>% sort()
ads_clean9 %>% var_sum(LB_BL_ALB, group_var = STUDYID)  

# some plots

alb_bl <- ads_clean9 %>% ggplot() + geom_histogram(aes(x = LB_BL_ALB))
alb_ic <- ads_clean9 %>% ggplot() + geom_histogram(aes(x = LB_IC_ALB))
alb_diff <- ads_clean9 %>% ggplot() + geom_histogram(aes(x = LB_IC_ALB-LB_BL_ALB))

# alb_bl
# alb_ic
# alb_diff

# UREAN

ads_clean9 %>% relocate(sort(names(ads_clean9))) %>% names()
# LB_XX_UREA also exists, but this is completely empty 

ads_clean9 %>% var_sum(LB_BL_UREAN)                        # missing in 29.2%
ads_clean9 %>% var_sum(LB_IC_UREAN)                        # missing in 58.4%

ads_clean9 %>% var_sum(LB_BL_UREAN, group_var = STUDYID)     # systematically missing in 5 studies
ads_clean9 %>% var_sum(LB_IC_UREAN, group_var = STUDYID)     # systematically missing in 9 studies

# BL

# VVNGOE has 2 high BL urea values (22.46 and 35.387) - keep for now
# VDXALE has 1 high BL urea value at 31.42 - keep for now
# some other studies with high BL ureas, up to 39 - keep for now

# IC

# VYDSGR has a urea of 0.20 -> delete

ads_clean9 %>% filter(STUDYID == "VYDSGR") %>% pull(LB_IC_UREAN) %>% sort()

ads_clean10 <- ads_clean9 %>% 
  mutate(
    LB_IC_UREAN = ifelse(LB_IC_UREAN < 1, NA, LB_IC_UREAN)
  )

# 

#######################
## WEIGHT AND HEIGHT ##
#######################

# corrections from anthro.R explorations
ads_clean10_1 <- ads_clean10 %>% 
  mutate(
    DM_AGE = ifelse(USUBJID %in% c("VLAULV_MUZAFFARPUR_AA-219", "VLEALTT_PATNA_52", "VRBQIF_MUZAFFARPUR_FST-008", "VLNAZSK_PATNA_2004", "VSGPDL_MUZAFFARPUR_JDF-0620", "VSGPDL_MUZAFFARPUR_JDF-0384"), NA, DM_AGE),
    VS_BL_WEIGHT = ifelse(USUBJID %in% c("VSGPDL_MUZAFFARPUR_JDF-0677", "VSGPDL_MUZAFFARPUR_JDF-0485"), NA, VS_BL_WEIGHT),
    VS_IC_WEIGHT = ifelse(USUBJID %in% c("VSGPDL_MUZAFFARPUR_JDF-0677", "VSGPDL_MUZAFFARPUR_JDF-0485"), NA, VS_IC_WEIGHT),
  )

# HEIGHT ----

ads_clean10 %>% var_sum(VS_BL_HEIGHT)                         # missing in 61.2%
ads_clean10 %>% var_sum(VS_BL_HEIGHT, group_var = STUDYID)    # systematically missing in 10 studies (over half of all studies)

ads_clean10 %>% count(STUDYID)
ads_clean10 %>% filter(STUDYID == "VWPJRM") %>% arrange(VS_BL_HEIGHT) %>% relocate(VS_BL_HEIGHT) #%>% View()

# VWPJRM includes 4 patients with height < 60cm which is not compatible with age -> remove
ads_clean11 <- ads_clean10_1 %>% mutate(VS_BL_HEIGHT = ifelse(VS_BL_HEIGHT <= 90, NA, VS_BL_HEIGHT))
ads_clean11 %>% var_sum(VS_BL_HEIGHT, group_var = STUDYID)    

# WEIGHT ----

# BL
ads_clean11 %>% var_sum(VS_BL_WEIGHT)                         # missing in 26.1%
ads_clean11 %>% var_sum(VS_BL_WEIGHT, group_var = STUDYID)    # systematically missing in 3 studies (over half of all studies)

# VLEALTT has a 7 year old that weights 5kg... may need to revisit these cases

# IC

# there are two patients (VDXALE) with IC weight of 1kg, not corresponding to BL weight of age -> remove

ads_clean11 %>% relocate(VS_IC_WEIGHT, VS_BL_WEIGHT, DM_AGE, VS_BL_HEIGHT) %>% arrange(VS_IC_WEIGHT) #%>% View()
ads_clean11 %>% var_sum(VS_IC_WEIGHT)
ads_clean11 %>% var_sum(VS_IC_WEIGHT - VS_BL_WEIGHT, group_var = STUDYID)# %>% View()
ads_clean11 %>% count(STUDYID, VS_IC_WEIGHT - VS_BL_WEIGHT) %>% print(n = Inf)

# looking at difference in weights - there are suspicious findings
## VLZUKHR has 414 patients with 0 kg difference (I suspect for many of these cases, the baseline weight was also used as the initial cure rate)
## VYDSGR also looks suspicious - keep for now
## VFEFCS has a very wide range of weight changes - from -47kg to +49 kg -> looks like VS_BL_WEIGHT is incorrect (when inspecting correlation with DM_AGE)
## let's limit weight change to maximum of 200g per day! 

ads_clean12 <- ads_clean11 %>% 
  mutate(
    VS_BL_WEIGHT = ifelse(STUDYID == "VFEFCS", NA, VS_BL_WEIGHT),
    VS_IC_WEIGHT = ifelse(VS_IC_WEIGHT == 1, NA, VS_IC_WEIGHT),
    VS_BL_HEIGHT = ifelse(VS_BL_HEIGHT < 90, NA, VS_BL_HEIGHT),
    VS_IC_WEIGHT = ifelse(STUDYID == "VLZUKHR", NA, VS_IC_WEIGHT),
    across(c(VS_IC_WEIGHT, VS_BL_WEIGHT), ~ifelse(abs((VS_IC_WEIGHT - VS_BL_WEIGHT) / IC_DAYS) > 0.2, NA, .x))
  )

weight_bl <- ads_clean12 %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_BL_WEIGHT,
      fill = OUT_DC_RELAPSE)) + 
  facet_wrap(~STUDYID)
weight_ic <- ads_clean12 %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_IC_WEIGHT,
      fill = OUT_DC_RELAPSE)) + 
  facet_wrap(~STUDYID)
weight_bl_age <- 
  ads_clean12 %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(aes(
    y = VS_BL_WEIGHT, 
    x = DM_AGE
  )) + 
  geom_jitter(
    aes(colour = OUT_DC_RELAPSE),
    width = 1, height = 1) + 
  scale_x_continuous(
    breaks = seq(0, 100, 10)
  ) +
  geom_smooth(method = "loess", se = TRUE, span = 0.75) + 
  coord_fixed(ratio = 0.5) + 
  facet_wrap(~STUDYID) 
weight_ic_age <- 
  ads_clean12 %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(aes(
    y = VS_IC_WEIGHT, 
    x = DM_AGE
  )) + 
  geom_jitter(
    aes(colour = OUT_DC_RELAPSE),
    width = 1, height = 1) + 
  scale_x_continuous(
    breaks = seq(0, 100, 10)
  ) +
  geom_smooth(method = "loess", se = TRUE, span = 0.75) + 
  coord_fixed(ratio = 0.5) + 
  facet_wrap(~STUDYID) 
weight_diff_dist <- 
  ads_clean12 %>% 
  mutate(diff = VS_IC_WEIGHT - VS_BL_WEIGHT) %>% 
  filter(diff >= -5 & diff <= 5) %>% 
  ggplot() +
  geom_histogram(
    aes(
      x = diff
    ),
    binwidth = 0.1
  ) + 
  scale_x_continuous(breaks = seq(-5, 5, 1)) + 
  coord_fixed(ratio = 0.01) +
  facet_wrap(~ STUDYID)
weight_diff_age <- 
  ads_clean12 %>% 
  mutate(diff = VS_IC_WEIGHT - VS_BL_WEIGHT) %>% 
  filter(diff >= -5 & diff <= 5) %>% 
  arrange(OUT_DC_RELAPSE) %>%  
  ggplot(aes(
    y = diff, 
    x = DM_AGE
  )) + 
  geom_jitter(
    aes(colour = OUT_DC_RELAPSE),
    width = 1, height = 0.1) + 
  scale_x_continuous(
    breaks = seq(0, 100, 10)
  ) +
  scale_y_continuous(
    breaks = seq(-5,5,1)
  ) + 
  geom_smooth(method = "loess", se = TRUE, span = 0.75) + 
  coord_fixed(ratio = 3) + 
  facet_wrap(~STUDYID) 

# VFEFCS - need to remove
weight_diff_age_vfefcs <- 
  ads_clean11 %>% 
  filter(STUDYID == "VFEFCS") %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(aes(
    y = VS_IC_WEIGHT - VS_BL_WEIGHT, 
    x = DM_AGE
  )) + 
  geom_jitter(
    aes(colour = factor(DM_ARM)),
    width = 1, height = 0.2) + 
  scale_x_continuous(
    breaks = seq(0, 100, 10)
  ) +
  geom_smooth(method = "loess", se = TRUE, span = 0.75) + 
  coord_fixed(ratio = 0.5) 

# ggsave("Analysis/weight/weight_ic_age.png", plot = weight_ic_age, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/weight/weight_bl_age.png", plot = weight_bl_age, width = 16, height = 9, dpi = 300)
# ggsave("Analysis/weight/weight_diff_age.png", plot = weight_diff_age, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/weight/weight_diff_age_vfefcs.png", plot = weight_diff_age_vfefcs, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/weight/weight_bl.png", plot = weight_bl, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/weight/weight_ic.png", plot = weight_ic, width = 16, height = 9, dpi = 300) 

###########################
## SPLEEN AND LIVER SIZE ##
###########################

# SPLEEN ----
ads_clean12 %>% var_sum(MP_BL_SPLEEN_LENGTH)                      # 3.69% missing
ads_clean12 %>% var_sum(MP_BL_SPLEEN_LENGTH, group_var = STUDYID) # systematically missing in VFETIZ only

ads_clean12 %>% var_sum(MP_IC_SPLEEN_LENGTH)                      # 4.52% missing
ads_clean12 %>% var_sum(MP_IC_SPLEEN_LENGTH, group_var = STUDYID) # systematically missing in VFETIZ only

ads_clean12 %>% count(MP_BL_SPLEEN_LENGTH) %>% print(n = Inf)
ads_clean12 %>% count(MP_IC_SPLEEN_LENGTH) %>% print(n = Inf)

# LIVER ----
ads_clean12 %>% var_sum(MP_BL_LIVER_LENGTH)                      # 11.6% missing
ads_clean12 %>% var_sum(MP_BL_LIVER_LENGTH, group_var = STUDYID) # systematically missing in 5 studies

ads_clean12 %>% var_sum(MP_IC_LIVER_LENGTH)                      # 14.6% missing
ads_clean12 %>% var_sum(MP_IC_LIVER_LENGTH, group_var = STUDYID) # systematically missing in 6 studies

ads_clean12 %>% count(MP_BL_LIVER_LENGTH) %>% print(n = Inf)
ads_clean12 %>% count(MP_IC_LIVER_LENGTH) %>% print(n = Inf)


# again, study VLZUKHR values appears suspicious
spleen_bl <- ads_clean12 %>% ggplot() + 
  geom_histogram(
    aes(x = MP_BL_SPLEEN_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)
spleen_ic <- ads_clean12 %>% ggplot() + 
  geom_histogram(
    aes(x = MP_IC_SPLEEN_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)
spleen_diff <- ads_clean12 %>% ggplot() + 
  geom_histogram(
    aes(x = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

# ggsave("Analysis/spleen_liver/spleen_bl.png", plot = spleen_bl, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/spleen_liver/spleen_ic.png", plot = spleen_ic, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/spleen_liver/spleen_diff.png", plot = spleen_diff, width = 16, height = 9, dpi = 300) 

liver_bl <- ads_clean11 %>% ggplot() + 
  geom_histogram(
    aes(x = MP_BL_LIVER_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)
liver_ic <- ads_clean11 %>% ggplot() + 
  geom_histogram(
    aes(x = MP_IC_LIVER_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)
liver_diff <- ads_clean11 %>% ggplot() + 
  geom_histogram(
    aes(x = MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

# ggsave("Analysis/spleen_liver/liver_bl.png", plot = liver_bl, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/spleen_liver/liver_ic.png", plot = liver_ic, width = 16, height = 9, dpi = 300) 
# ggsave("Analysis/spleen_liver/liver_diff.png", plot = liver_diff, width = 16, height = 9, dpi = 300) 

# remove VLZUKHR MP data
ads_clean125 <- ads_clean12 %>% 
  mutate(
    across(
      starts_with("MP_"), 
      ~ifelse(
        STUDYID == "VLZUKHR",
        NA,
        .x
      )
    ),
    MP_BL_LIVER_LENGTH = ifelse(MP_BL_LIVER_LENGTH > 10, NA, MP_BL_LIVER_LENGTH)

  )

#############
# PARASITES #
#############

ads_clean125 %>% names() %>% sort()

ads_clean125 %>% count(MB_BL_LSHMANIA_SPLEEN) %>% print(n = Inf)
ads_clean125 %>% count(MB_BL_LSHMANIA_BONE) %>% print(n = Inf)

ads_clean13 <- ads_clean125 %>% 
  mutate(
    across(
      c(MB_BL_LSHMANIA_SPLEEN, MB_BL_LSHMANIA_BONE),
      ~case_when(
        .x == "-" ~ 0,
        .x == "+" ~ 1,
        .x == "1+" ~ 1,
        .x == "2+" ~ 2,
        .x == "3+" ~ 3,
        .x == "4+" ~ 4,
        .x == "5+" ~ 5,
        .x == "0" ~ 0,
        .x == "1" ~ 1,
        .x == "2" ~ 2,
        .x == "3" ~ 3,
        .x == "4" ~ 4,
        .x == "5" ~ 5,
        .default = NA
      )
    )
  )

ads_clean13 %>% count(MB_BL_LSHMANIA_SPLEEN) %>% print(n = Inf)
ads_clean13 %>% count(MB_BL_LSHMANIA_BONE) %>% print(n = Inf)

ads_clean13 %>% count(STUDYID, MB_BL_LSHMANIA_SPLEEN, MB_BL_LSHMANIA_BONE) %>% print(n = Inf)

ads_clean13 %>% var_sum(MB_BL_LSHMANIA_SPLEEN)            # missing 53.9%
ads_clean13 %>% var_sum(MB_BL_LSHMANIA_SPLEEN, STUDYID)   # systematically missing in 4 studies

ads_clean13 %>% var_sum(MB_BL_LSHMANIA_BONE)              # missing 97.3%
ads_clean13 %>% var_sum(MB_BL_LSHMANIA_BONE, STUDYID)     # systematically missing in all but 2 studies

ads_clean14 <- ads_clean13 %>% 
  mutate(
    MB_COMBINED = ifelse(
      !is.na(MB_BL_LSHMANIA_SPLEEN),
      MB_BL_LSHMANIA_SPLEEN,
      ifelse(
        !is.na(MB_BL_LSHMANIA_BONE),
        MB_BL_LSHMANIA_BONE,
        NA
      )
    )
  )

ads_clean14 %>% count(MB_BL_LSHMANIA_SPLEEN, MB_BL_LSHMANIA_BONE, MB_COMBINED)

spleen_count_dist <- ads_clean13 %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_BL_LSHMANIA_SPLEEN
    ),
    binwidth = 1
  ) + 
  facet_wrap(~STUDYID)
bone_count_dist <- ads_clean13 %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_BL_LSHMANIA_BONE
    ),
    binwidth = 1
  ) + 
  facet_wrap(~STUDYID)
mb_combined <- ads_clean14 %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_COMBINED
    ),
    binwidth = 1
  ) + 
  facet_wrap(~STUDYID)

mb_combined_age <- 
  ads_clean14 %>% 
  ggplot() +
  geom_jitter(
    aes(
      x = DM_AGE,
      y = MB_COMBINED
    ),
    width = 1, height = 0.2
  ) +
  geom_smooth(
    aes(
      x = DM_AGE,
      y = MB_COMBINED
    ),
    method = "lm"
  ) +
  facet_wrap(~STUDYID) 


# ggsave(filename = "Analysis/parasite/spleen.png", plot = spleen_count_dist, width = 16, height = 9, dpi = 300)
# ggsave(filename = "Analysis/parasite/bone.png", plot = bone_count_dist, width = 16, height = 9, dpi = 300)
# ggsave(filename = "Analysis/parasite/combined.png", plot = mb_combined, width = 16, height = 9, dpi = 300)
# ggsave(filename = "Analysis/parasite/combined_age.png", plot = mb_combined_age, width = 16, height = 9, dpi = 300)

#################
## VL DURATION ##
#################

# use lubridate to convert from ISO 8601 duration to number of days
ads_clean15 <- ads_clean14 %>% 
  mutate(
    VL_DURATION = duration(SA_HX_FEV_DUR) / (60 * 60 * 24)
  )
class(ads_clean15$VL_DURATION) <- c()

# inspect... 
ads_clean15 %>% count(VL_DURATION, SA_HX_FEV_DUR) %>% print(n = Inf)
inspect <- ads_clean15 %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = log(VL_DURATION)
    )
  )

# there are a few variables which are very nearly systematically missing, but not quite. Let's correct these here

# VLZUKHR: Creatinine missing in 596/600 patients (remove)
# VSGPDL: Parasite grade missing in 846/928 patients (keep for now)

ads_clean16 <- ads_clean15 %>% 
  mutate(
    LB_BL_CREAT = if_else(STUDYID == "VLZUKHR", NA, LB_BL_CREAT)
  )

ads_dirty <- ads_clean2
ads_clean <- ads_clean16

saveRDS(ads_clean, file = "data/ads_clean.rds")
saveRDS(ads_dirty, file = "data/ads_dirty.rds")


df %>% count(OUT_IC, OUT_DC, OUT_DC_OTHER, OUT_DC_RELAPSE, OUT_DC_DEATH)