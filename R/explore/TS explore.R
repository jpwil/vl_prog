
###########################
## TS DOMAIN EXPLORATION ##
###########################

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","TS.RData"))
load(paste0(wd, "/","DM.RData"))

TS %>% filter(STUDYID=="VFEFCS") %>% View()

TS %>% count(STUDYID) %>% dim()     # 36 studies as expected
TS %>% count(DOMAIN)                # only one value as expected
TS %>% count(TSSEQ)

TS %>% count(TSGRPID) %>% print(n = Inf)

TS %>% count(TSPARMCD) %>% arrange(desc(n)) %>% print(n = Inf) 
TS %>% count(TSPARMCD)
