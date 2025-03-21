# explore IDDO VL repository document (2024-04 version)

library(readxl)
library(tidyverse)


repo <- read_xlsx(
    "other files/IDDO VL Inventory_2024-04.xlsx",
    col_names = TRUE,
    skip = 1
    )

repo %>% View()
repo %>% names()

repo1 <- repo %>% 
  rename(
    country = "Country(s) of Data Collection",
    ttype = "Trial Type",
    num = "Number of Individual Patients",
    status = "Curation Status",
    disease = "Trial Disease/Condition Indication"
    ) %>% 
  filter(!is.na(country))

repo1  %>% count(disease)
repo1  %>% summarise(n_patients = sum(num), n_trial = n())
repo1 %>% names()
repo1 %>% count(country)
repo1 %>% summarise(n = n())

repo_sum <- repo1 %>% 
  filter(
    status == "Completed", 
    ttype == "INTERVENTIONAL",
    disease == "Visceral leishmaniasis"
    ) %>% 
    group_by(country, ttype, status) %>% summarise(n_total = sum(num), n_trial = n()) %>% ungroup() 

repo_sum %>% ungroup() %>% mutate(
  n_patients_prop = n_total/sum(n_total), 
  n_trial_prop = n_trial/sum(n_trial)
  )

repo_sum %>% summarise(n = sum(n_total))
repo_sum %>% summarise(n = sum(n_trial))

# 58 studies 
# India = 40; Nepal = 4