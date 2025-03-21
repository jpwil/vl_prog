###########################
## VS DOMAIN EXPLORATION ##
###########################

# VITAL SIGNS

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","VS.RData"))

missingness("VS")

VS %>% count(USUBJID)

#sequence
VS %>% count(USUBJID, VSSEQ) %>% count(n)

# 672 with the same USUBJID and VSSEQ (all belonging to STUDYID VGKSTG)
# All consecutive except VGKSTG - all rows for VGKSTG are all duplicates (exact duplicates)
VS_nonsec <- VS %>% 
  arrange(USUBJID, VSSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = VSSEQ == row_number())
VS_nonsec %>% group_by(USUBJID) %>% 
  filter(!all(test == TRUE)) %>% 
  ungroup() %>% 
  arrange(USUBJID, VSSEQ) %>% count(STUDYID, test)

VS_distinct <- VS %>% distinct()

VS_distinct %>% count(VSTESTCD, VSTEST)
VS_distinct %>% filter(STUDYID == "VOAYGC" & VSTESTCD == "SYSBP") %>% arrange(USUBJID, VSSEQ) %>% View()


  
