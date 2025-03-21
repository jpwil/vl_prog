
###########################
## TV DOMAIN EXPLORATION ##
###########################

library(tidyverse)
source("definitions.R")
load(paste0(wd, "/","domainsR.RData"))

missingness("TV")

TV %>% group_by(STUDYID) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  summarise(q0 = min(total),
            q25 = quantile(total, 0.25),
            q50 = median(total),
            q75 = quantile(total, 0.75),
            q100 = max(total))

# TVSTRL – Visit Start Rule
TV %>% count(TVSTRL) %>% print(n = Inf)
TV %>% filter(STUDYID == "VIZGFA") %>% print(n = Inf) #VIZGFA is missing a TVSTRL value
TV %>% count(TVENRL) %>% print(n = Inf) # not in IDDO WIKI

# VISITDY inconsistently completed, and not always accurate
# Sometimes screening is Day 1 (not always)
# For example; VBNRQO has VISITDY as '9' for 6 month folow/////-u/p

# duplicated visits (regardless of whether adding VISITDY) 
TV %>% 
  arrange(STUDYID, VISITNUM) %>% 
  add_count(STUDYID, VISIT, VISITNUM, VISITDY) %>% 
  filter(n>1) 

# when included ARM* as well, there are still 98 duplicates in VGKSTG
TV %>% 
  arrange(STUDYID, VISITNUM) %>% 
  add_count(STUDYID, VISIT, VISITNUM, VISITDY, ARMCD, ARM) %>% 
  filter(n>1) %>% 
  print(n = 100)

# 96 of these duplicates are explained by TVSTRL (Baseline assessments)
TV %>% 
  arrange(STUDYID, VISITNUM) %>% 
  add_count(STUDYID, VISIT, VISITNUM, VISITDY, ARMCD, ARM, TVSTRL) %>% 
  filter(n>1) %>% 
  print(n = 100)

# all VISITNUMS are sequential once you remove the duplicates
# EXCEPT VUFCZW which doesn't have visit number 32
TV %>% 
  arrange(STUDYID, VISITNUM) %>% 
  distinct(STUDYID, VISITNUM) %>% 
  group_by(STUDYID) %>% 
  mutate(test = VISITNUM == row_number())
