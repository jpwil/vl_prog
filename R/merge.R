#########
# MERGE #
#########

# this R file merges all the STUDY analysis datasets in the "Study" directory
rm(list = ls())
library(tidyverse)

study_list <- dir("Study/Data", pattern = ".RData$")
ads <- tibble()

# iterate over each study list, create a new column containing the study ID and merge together into a single dataset

# run R files
for (i in study_list) {
    print(i)
    rscript <- str_remove(i, "Data$")
    source(str_c("Study/", rscript))
    }

study_list <- dir("Study/Data", pattern = ".RData$")
ads <- tibble()

# inspect data types here (must be the same before binding rows)
# for (i in study_list) {
#     load(str_c("Study/Data/", i))
#     study_name <- str_remove(i, ".RData")
#     dataset_temp <- get(study_name) %>% 
#       mutate(STUDYID = study_name)
#     class <- class(get(study_name)[["LB_BL_HGB"]])
#     print(str_c(study_name,": ", class))
#     }

# bind rows
for (i in study_list) {
    load(str_c("Study/Data/", i))
    study_name <- str_remove(i, ".RData")
    dataset_temp <- get(study_name) %>% 
      mutate(STUDYID = study_name)
    ads <- bind_rows(ads, dataset_temp) 
    print(i) 
    }

# for analysis dataset, ONLY include patients who have reasonable confidence of achieving initial cure (OUT_IC == TRUE)
# and excluding (i) pregnant women (ii) HIV positive patients (iii) the patients that died after initiL cure (also did not relapse)
ads %>% count(OUT_IC, OUT_DC_DEATH, OUT_XX_DEATH, OUT_DC_RELAPSE)

save(ads, file = "Analysis/ads.RData")
