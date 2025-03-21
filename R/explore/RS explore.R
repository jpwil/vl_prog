
###########################
## RS DOMAIN EXPLORATION ##
###########################

###################################################

library(tidyverse)

wd <- if_else(
  Sys.info()["sysname"] == "Darwin",
  "/Users/jameswilson/Documents/SDTM June 2023",
  "C:/Users/jameswilson/Documents/R projects/SDTM Import")
setwd(wd)
load("domainsR.RData")

##

colnames(RS)

RS %>% count(STUDYID) # 36 STUDYIDs (completed for all studies)
RS %>% count(DOMAIN) # Just RS domain as expected

RS %>% count(USUBJID) %>% count(n)
# each USUBJID appears up to 10 times

RS %>% count(USUBJID, RSSEQ) %>% count(n)
# each USUBJID and RSSEQ are unique combinations

RS %>% count(RSTESTCD, RSTEST)
# there are only two possible disease response tests

# OVRLRESP (Overall Response): the overall treatment response for the diseases specified
## FINAL CURE
## FAILURE - RELAPSE
## FAILURE

# TOC: Test of cure for VL; Initial Cure Assessment
## INITIAL CURE (Complete recovery; initial cure)
## INITIAL FAILURE (Drug change; incomplete; ongoing; slow responder)

RS %>% count(RSCAT, RSSCAT)
RS %>% count(RSSCAT)

RS %>% count(RSORRESU) # all NA
RS %>% count(RSSTRESN) # all NA
RS %>% count(RSSTRESU) # all NA
RS %>% count(RSSTAT)   # all NA
RS %>% count(RSREASND) # all NA

RS %>% count(RSORRES) %>% arrange(desc(n)) %>% print(n = Inf)
RS %>% count(RSSTRESC)

RS %>% count(RSTESTCD, RSCAT, RSSCAT, RSSTRESC) %>% print(n = Inf)
RS %>% count(RSTESTCD, RSCAT, RSSCAT, RSSTRESC) %>% arrange(desc(n)) %>% print(n = Inf)
# not as straightforward as it may seem

# look at the timings as well
RS %>% count(RSTESTCD, RSCAT, RSSCAT, RSSTRESC, VISIT) %>% arrange(VISIT) %>% print(n = 500)

RS %>% 
  count(RSTESTCD, RSCAT)

RS %>% 
  filter(RSCAT != "PKDL" | is.na(RSCAT)) %>% 
  count(RSTESTCD, RSCAT, RSSCAT)

RS %>% 
  filter(RSCAT != "PKDL" | is.na(RSCAT)) %>% 
  count(RSTESTCD, RSSCAT, RSSTRESC) %>% 
  arrange(RSTESTCD, RSSCAT, desc(n)) %>% 
  print(n = Inf)

RS %>% 
  filter(RSCAT != "PKDL" | is.na(RSCAT)) %>% 
  count(RSSCAT) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)
