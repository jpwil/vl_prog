#########
# STUDY #
#########

# this R file takes data from the Excel "relapse master.xlsx" spreadsheet
# with the aim of merging with the summary analysis dataset

rm(list = ls())
library(tidyverse)
library(readxl)

# PUBLICATIONS
pub <- read_excel(
    path = "other files/relapse master.xlsx",
    sheet = "Publications",
    range = "C3:J23",
    trim_ws = TRUE 
)

pub_merge <- pub %>% 
  rename(
    pub_pmid = `PubMed ID`,
    pub_doi = DOI,
    pub_title = Title,
    pub_journal = Journal,
    pub_lead_author = `Lead author`,
    pub_year = `Year published`
  ) %>% 
  select(STUDYID, starts_with("pub_")) %>% 
  filter(pub_pmid != 24941345) %>%  # VYDSGR is published in 2 studies - this study does not contain much added data (ignore for now to maintain 1:1 study:publication.)
  filter(STUDYID  != "VBNRQO")      # this study is excluded

# STUDY ARM
arm <- read_excel(
    path = "other files/relapse master.xlsx",
    sheet = "Study arms",
    range = "C3:K42",
    trim_ws = TRUE 
)

arm_merge <- arm %>% 
  filter(STUDYID != "VBNRQO") %>% 
  rename(
    drug1 = `Drug 1`,
    drug2 = `Drug 2`
  ) %>% 
  pivot_wider(
    id_cols = STUDYID,
    values_from = c(drug1, drug2),
    names_from = Arm,
    names_glue = "arm{Arm}_{.value}",
    names_sort = FALSE
  )

arm_merge <- arm_merge %>% relocate(STUDYID, sort(names(arm_merge))) #%>% View()

# PROTOCOLS
# there are only two protocols which are used to extract study design data
# VAQMOU, VFETIZ
prot <- read_excel(
    path = "other files/relapse master.xlsx",
    sheet = "Protocols",
    range = "C5:L11",
    trim_ws = TRUE 
) %>% 
  filter(STUDYID %in% c("VFETIZ", "VAQMOU")) %>% 
  select(STUDYID, prot_title = `Long / Official title`, prot_alt_id = `Other study ID`)

# STUDY DESIGN

study_design <- read_excel(
    path = "other files/relapse master.xlsx",
    sheet = "Study design",
    range = "C4:AR26",
    trim_ws = TRUE
)

study_design_merge <- study_design %>%  
  filter(STUDYID != "VBNRQO") %>%  # excluded due to data provenance concerns
  mutate(
    study_info_from = ifelse(Published == "Yes", "Publication", "Protocol"),
    prot_id   = ifelse(Published == "No", `Ref ID`, NA)
  ) %>% 
  rename(
    study_arms = `Number of arms`,
    study_random = `Randomisation?`,
    study_location = `Location patients treated 1`,
    study_country = `Country 1`,
    study_age_min = `Age min`,
    study_age_max = `Age max`,
    study_hiv_excl = `HIV excluded?` # study explicitly reports HIV excluded
  ) %>% 
  select(STUDYID, starts_with("study_"), starts_with("prot_"))

# STUDY OUTCOME

study_outcome <- read_excel(
    path = "other files/relapse master.xlsx",
    sheet = "Study outcome",
    range = "B3:AI22",
    trim_ws = TRUE
)

study_outcome %>% names()
study_outcome_merge <- study_outcome %>% 
  rename(
    study_num = "Number of enrolled patients",
    study_relapse = "Number of patients who relapsed between initial cure and definitive cure assessment",
    study_death = "Deaths - overall how many deaths occurred"
  ) %>% 
  select(
    STUDYID, starts_with("study_")
  )

# MERGE DATASETS

ads_study <- pub_merge %>% full_join(study_design_merge) %>% full_join(prot) %>% full_join(arm_merge)
ads_study <- ads_study %>% full_join(study_outcome_merge, join_by(STUDYID))

save(ads_study, file = "Analysis/ads_study.RData")
