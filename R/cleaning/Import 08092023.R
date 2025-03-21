library(tidyverse)
library(openxlsx)

### GDPR: BE CAREFUL NOT UPLOAD IPD TO SHARED/CLOUD FOLDER (EG GITHUB, ONEDRIVE, etc) ##
### this R script imports the curated IDDO SDTM data and performs some low-level data exploration
### should only need to be executed once for each curated data update

source("definitions.R")
setwd(wd)

domains_csv <- dir() %>%
  str_extract("(^[:upper:]{2}).csv$", group = 1) %>%
  na.omit()
domains_csv

# DOMAIN (* core)                                    OBSERVATION CLASS

# AU	Audiometry Test Results	                       Findings Observation Class
# CC	Contributor-Coded Clinical and Adverse Events	 Events Observation Class
# DD	Death Details	                                 Events Observation Class
# *DM	Demographics	                                 Special Purpose
# DS	Disposition	                                   Events Observation Class
# HO	Healthcare Encounters	                         Events Observation Class
# *IN	Treatments and Interventions	                 Interventions Observation Class
# *LB	Laboratory Test Results	                       Findings Observation Class
# *MB	Microbiology Specimen	                         Findings Observation Class
# *MP	Morphology and Physiology	                     Findings Observation Class
# PE	Physical Examination	                         Findings Observation Class
# PT	Per-Protocol Treatments and Interventions	     Interventions Observation Class
# QS	Questionnaires	                               Findings Observation Class
# *RP	Reproductive System Findings	                 Findings Observation Class
# RS	Disease Response and Clinical Classification	 Findings Observation Class
# *SA	Clinical and Adverse Events	                   Events Observation Class
# SC	Subject Characteristics	                       Findings Observation Class
# TI	Trial Inclusion Exclusion Criteria	           Trial Design
# *TS	Trial Summary	                                 Trial Design
# *TV	Trial Visits	                                 Trial Design
# *VS	Vital Signs	                                   Findings Observation Class

# import csv
n_max <- 1000000
for (i in 1:length(domains_csv)) {
  assign(
    paste0(domains_csv[i]),
    read.csv(
      paste0(wd, "/", domains_csv[i], ".csv"), 
      stringsAsFactors = FALSE,
      na.strings = c("NA", ""))
  )
}

# for each domain, create dataframes for each domain and study as D_X (D = domain, X = IDDO study ID)
for (i in 1:length(domains_csv)) {
  studyid_list <- get(domains_csv[i]) %>%
    pull(STUDYID) %>%
    unique() # create a vector of strings containing all the study IDs
  for (j in 1:length(studyid_list)) {
    assign(
      paste0(domains_csv[i], "_", studyid_list[j]),
      value = get(domains_csv[i]) %>% 
        filter(STUDYID == studyid_list[j])
    )
  }
}

# create a vector of strings corresponding to R objects for saving
datasets <- str_extract(ls(), "(^[:upper:]{2}$)|(^[:upper:]{2}_[:upper:]+$)") %>% na.omit()

# strip the domains of empty variables (columns), including all NAs or all empty strings
for (dataset in datasets) {
  df <- get(dataset)
  df <- df  %>% 
    select(where(~ !all(is.na(.)) & !all(. == "")))
  assign(dataset, df)
}

# save the R objects to RData files - this takes a few seconds
for (i in 1:length(datasets)) {
  save(
    list = datasets[[i]],
    file = paste0(datasets[[i]], ".RData"),
    envir = .GlobalEnv
  )
}

# save(list = domain_datasets, file = "domainsR.RData", envir = .GlobalEnv)