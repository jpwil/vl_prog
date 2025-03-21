############
# OVERVIEW #
############

# this dataset provides a summary of the different outcomes by STUDYID

rm(list = ls())
library(tidyverse)
# load(file = "data/ads_study.RData") # merge the different analysis datasets
# load(file = "data/ads.RData")
# load(file = "data/ads_clean.RData")

load("data/ads.RData")

################
# DEMOGRAPHICS #
################

ads %>% count(DM_AGE) %>% print(n = Inf)

# ads %>% filter(!is.na(DM_AGE)) %>% 
#   ggplot() +
#   geom_histogram(
#     aes(x = DM_AGE) 
#   ) + 
#   facet_wrap(~STUDYID, scales = "free_y")

# ads %>% filter(!is.na(DM_AGE)) %>% 
#   ggplot() +
#   geom_histogram(
#     aes(x = DM_AGE),
#     binwidth = 1 
#   ) + 
#   facet_wrap(~STUDYID, scales = "fixed")

# add age, sex and outcome information 
sum <- ads_clean %>% 
  group_by(STUDYID) %>% 
  summarise(
    num_data =        n(),
    dm_age_50 =       median(DM_AGE, na.rm = TRUE),
    dm_age_25 =       quantile(DM_AGE, prob = 0.25, na.rm = TRUE),
    dm_age_75 =       quantile(DM_AGE, prob = 0.75, na.rm = TRUE),
    dm_age_0  =       quantile(DM_AGE, prob = 0, na.rm = TRUE),
    dm_age_1 =        quantile(DM_AGE, prob = 1, na.rm = TRUE),
    dm_age_na =       sum(is.na(DM_AGE)),
    dm_sex_male =     sum(DM_SEX == "M" & !is.na(DM_SEX)),
    dm_sex_female =   sum(DM_SEX == "F" & !is.na(DM_SEX)), 
    dm_sex_unknown =  sum(is.na(DM_SEX)),
    out_na =          sum(OUT_NA),
    out_ic_true =     sum(OUT_IC),
    out_ic_false =    sum(!OUT_IC),
    out_dc_true =     sum(OUT_DC),
    out_dc_false =    sum(!OUT_DC),
    out_relapse_true =    sum(OUT_DC_RELAPSE),
    out_relapse_false =   sum(!OUT_DC_RELAPSE),
    out_ic_death =    sum(!is.na(OUT_IC_DEATH) & OUT_IC_DEATH == TRUE),
    out_dc_death =    sum(!is.na(OUT_DC_DEATH) & OUT_DC_DEATH == TRUE),
    out_xx_death =    sum(!is.na(OUT_XX_DEATH) & OUT_XX_DEATH == TRUE),
    vl_history   =    sum(VL_HISTORY)
  ) %>% 
  ungroup() %>% 
  mutate(
    dm_sex_pct_male = 100 * dm_sex_male / (dm_sex_male + dm_sex_female),
    out_pct_na = 100 * out_ic_true/num_data,
    out_pct_relapse = 100 * out_relapse_true / out_ic_true,
    out_total_death = out_ic_death + out_dc_death + out_xx_death,
    out_pct_death = 100 * out_total_death / num_data,
    vl_history_pct = 100 * vl_history / num_data,
  )
sum %>% View()
# combine study-level summary from analysis dataset with publication information
ads_merge <- ads_study %>% full_join(sum) 
ads_merge #%>% View()


# total relapses: 230/5158; 4.46 percent (range 0 - 10%)
relapse <- sum %>% ungroup() %>% summarise(
  relapses = sum(out_relapse_true),
  denominator = sum(out_ic_true), 
  relapse_pct = 100 * relapses / denominator,
  relapses_max = max(out_pct_relapse),
  relapses_min = min(out_pct_relapse)
)
relapse

# total deaths: 36/5499; 0.66 percent (range 0 - 11%)
death <- sum %>% ungroup() %>% summarise(
  deaths = sum(out_total_death),
  denominator = sum(num_data), 
  death_pct = 100 * deaths / denominator,
  deaths_max = max(out_total_death),
  deaths_min = min(out_total_death)
)
death


# prepare table for summary
ads_merge %>% names()
ads_summary_study <- ads_merge %>% 
  mutate(
    pmid_or_other = ifelse(!is.na(pub_pmid), pub_pmid, prot_id),
    relapse_pct_total = 100 * out_relapse_true / num_data,
    relapse_pct_ic = 100 * out_relapse_true / out_ic_true,
    title = ifelse(!is.na(pub_title), pub_title, prot_title)
    ) %>% 
  select(
    STUDYID, study_info_from, pmid_or_other, title, pub_lead_author, pub_year, study_location, study_age_min, study_age_max, study_arms, study_random,
    study_num
  )

ads_summary_study #%>% View()

ads_summary_outcomes <- ads_merge %>% 
  mutate(
    pmid_or_other = ifelse(!is.na(pub_pmid), pub_pmid, prot_id),
    relapse_pct_total = 100 * out_relapse_true / num_data,
    relapse_pct_ic = 100 * out_relapse_true / out_ic_true
  ) %>% 
  select(
    STUDYID, study_num, study_relapse, study_death, num_data, out_ic_true, out_relapse_true, out_total_death,
    relapse_pct_total, relapse_pct_ic,  dm_sex_pct_male, dm_age_50, dm_age_0, dm_age_1, dm_age_25, dm_age_75
  )
ads_summary_outcomes # %>% View()

# number of relapses by age
ads_relapse_age <- ads %>% 
  select(DM_AGE, starts_with("OUT_")) %>% 
  filter(!is.na(DM_AGE)) %>% 
  mutate(age_bin = cut(DM_AGE, breaks = c(0, 5, 10, 15, 20, 50, 100), right = FALSE)) %>% 
  group_by(age_bin) %>% 
  summarise(
    num_total = n(),
    num_relapse = sum(OUT_DC_RELAPSE),
    pct_relapse = 100 * num_relapse / num_total,
    num_death = sum(OUT_DC_DEATH + OUT_IC_DEATH + OUT_XX_DEATH),
    pct_death = 100 * num_death / num_total
  )
write_csv(ads_relapse_age, file = "Analysis/ads_summary_outcomes_by_age.csv")

ads %>% names()
ads_relapse_age

write_csv(ads_summary_outcomes, file = "Analysis/ads_summary_outcomes.csv")
write_csv(ads_summary_study, file = "Analysis/ads_summary_study.csv")
