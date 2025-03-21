
###########################
## DS DOMAIN EXPLORATION ##
###########################

###################################################

library(tidyverse)
library(naniar)
source("R/definitions.R")
load(paste0(wd, "/data/","DS.RData"))
load(paste0(wd, "/data/","DM.RData"))

missingness("DS")


DS %>% count(STUDYID)               # VHTYWO and VQKRHN do not have DS domains
DS %>% count(DOMAIN)                # only one value as expected

DS %>% count(USUBJID) %>% count(n)   
# 1     1  6348
# 2     2   422
# 3     3    19
# 4     4     5
# 5     5   200
# 6     6    19

DS %>% count(STUDYID, USUBJID) %>% count(STUDYID, n)
# most studies there is only one disposition per subject ID
# 5 studies have subject IDs corresponding to more than one disposition
# Of these studies, VGKSTG and VKRFLS usually have 5 or 2 dispositions per subject ID (mode), respectively

# 11 VGKSTG      4     5
# 12 VGKSTG      5   200
# 13 VGKSTG      6    19
# 
# 14 VHNNMS      1   272
# 15 VHNNMS      2     2
# 
# 18 VKRFLS      1   154
# 19 VKRFLS      2   408
# 20 VKRFLS      3    18
# 
# 34 VSGPDL      1   616
# 35 VSGPDL      2     7
# 
# 36 VUFCZW      1   193
# 37 VUFCZW      2     5
# 38 VUFCZW      3     1

DS %>% count(STUDYID, USUBJID, DSSEQ) %>% count(STUDYID, n, DSSEQ) %>% print(n = Inf)
# VAQMOU: each USUBJID has a unique disposition, but a different DSSEQ (likely curation error)
# VGKSTG: DSSEQ is consistent with above
# VHNNMS: DSSEQ is consistent with above      
# VKRFLS: DSSEQ is consistent with above
# VSGPDL: DSSEQ is consistent with above
# VUFCZW: DSSEQ is consistent with above

# EXPLORE OUTCOMES #
DS %>% count(DSDECOD, DSTERM) %>% arrange(DSDECOD,desc(n)) %>% print(n = Inf)
DS %>% filter(DSDECOD == "DEATH") %>% count(USUBJID) %>% count(n) %>% print(n = Inf)

168/n_distinct(DS$USUBJID) # death rate of 2.4% (n = 168)

# look at relapse
DS %>% count(DSDECOD, DSTERM) %>% arrange(DSDECOD,desc(n)) %>% print(n = Inf)

relapse <- DS %>% 
  select(STUDYID, USUBJID, DSTERM) %>% 
  mutate(relapse = str_detect(DSTERM, "(?i)relapse"),
         relapse = 
           case_when(str_detect(DSTERM, "Final cure:Intial cure for 6 months follow-up without") ~ FALSE,
                     str_equal(DSTERM, "NO RELAPSE") ~ FALSE,
                     str_detect(DSTERM, "VL (SSG failure: slow or non-responder or relapse) cured with") ~ FALSE,
                     .default = relapse))

relapse %>% count(relapse, DSTERM) %>% arrange(desc(relapse)) %>% print(n = 500)
relapse %>% filter(relapse == TRUE)
relapse %>% filter(relapse == TRUE) %>% count(USUBJID) %>% dim() # 330 relapses

DS %>% count(USUBJID) %>% dim()
330/n_distinct(DS$USUBJID) # 4.7% relapses

############################
# EXPLORE TIMING VARIABLES #
############################

# need to make this a function

# first let's check that there is concordance between the DY and DTC variables
all(is.na(DS$DSDTC) == is.na(DS$DSDY))
all(is.na(DS$VISITNUM) == is.na(DS$VISIT))
all(is.na(DS$DSSTDTC) == is.na(DS$DSSTDY)) # FALSE VGKSTG_BPKIHS_165 has DSSTDTC but not DSSTDY (no RFSTDTC)
all(is.na(DS$DSTPT) == is.na(DS$DSTPTREF)) 

# merge with RFSTDTC
vars <- c("STUDYID", "USUBJID", "VISIT", "VISITNUM", "VISITDY", "EPOCH", "DSDTC", "DSDY", "DSSTDTC", "DSSTDY",
          "DSENDTC", "DSENDY", "DSTPT", "DSTPTREF", "DSCDSTDY", "DSRPOC")
DS %>% 
  select(any_of(vars)) %>% 
  left_join(DM %>% select(USUBJID, RFSTDTC), by = join_by(USUBJID == USUBJID), keep = FALSE) %>% 
  mutate_all(~ifelse(is.na(.), 0L, 1L)) %>% as.data.frame() %>%  # need to convert to dataframe!!!!
  upset(nsets = 15, nintersects = 50, order.by = c("freq"), set_size.show = TRUE, set_size.scale_max = nrow(DS)+nrow(DS)/10)
  
as.integer(DS_merge_)

DS %>% count(DSEVINTX)

DS %>%
  select(starts_with("VISIT")) %>% 
  gg_miss_upset(nsets = 10)

DS %>%
  select(starts_with("VISIT"), DSDY, DSSTDY, DSTPT, DSTPTREF) %>% 
  gg_miss_upset(nsets = 10)



# Are all timings unique per USUBJID?
# no, 'VGKSTG' has two DSTERMS, 'definite cure' and 'responder: definite cure' for each subject, with identitical timings 
# also 'VUFCZW' has one subject (VUFCZW_DENSHA_016) with two DSTERMS with the same time stamp; relapse and died both on day 210
tvars1 <- colnames(DS)[8:length(colnames(DS))]
tvars1 <- c("USUBJID", tvars1)
tvars2 <- c("DSTERM", tvars1)
tvars_results1 <- DS %>% count(across(all_of(tvars1))) %>% arrange(desc(n))
tvars_results1 %>% print(n = 500)
tvars_results2 <- DS %>% count(across(all_of(tvars2))) %>% arrange(desc(n))

tvars_results2 <- DS %>% count(across(all_of(tvars2))) %>% arrange(desc(n))
tvars_results2 # when you add DSTERM they are unique

# VFETIZ and VOAYGC have no planned visits recorded (no VISIT* variables populated)
# VOAYGC has no date/time data recorded at all except for EPOCH
DS %>% filter(STUDYID=="VOAYGC") %>% count(EPOCH) # all treatment

DS %>% count(VISITNUM) %>% print(n = Inf)
DS %>% count(STUDYID, VISITNUM, VISIT, VISITDY) %>% print(n = Inf)
# VISIT and VISITNUM *(+/- VISITDY) all have a 1 to 1 relationship within each STUDYID
# VISITDY is incorrectly specified for VBNRQO

# in all but one study DSDTC and DSSTDTC are mutually exclusive
DS %>% count(STUDYID,DSDTC)   %>% print(n = Inf)
DS %>% count(STUDYID,DSSTDTC) %>% print(n = Inf)
DS %>% filter(STUDYID == "VFETIZ") %>% count(DSDTC, DSSTDTC, DSDECOD) %>% print(n = Inf)

DS %>% count(DSDY) %>% print(n = Inf) # negative for 8 entries
DS %>% count(DSSTDY) %>% print(n = Inf) # negative for 10 entries
DS %>% count(STUDYID, DSDTC, DSDY) %>% print(n = Inf) 

DS %>% count(DSTPTREF, DSTPT)
DS %>% count(DSSTRTPT, DSSTTPT)
DS %>% count(DSEVINTX)
DS %>% count(DSCDSTDY, DSRPOC)

# GRAPHICAL DISPLAY OF DIFFERENT TIMING VARIABLES BY STUDY * could make this into a function
# study missing, all missing, none missing, some missing (gradient scale)
# function with DOMAIN as first argument and COLS as second argument (if blank, include all columns)

DS %>% count(EPOCH, DSDECOD) %>% 
  arrange(EPOCH, desc(n)) %>% 
  print(n = 300)


