
###########################
## MP DOMAIN EXPLORATION ##
###########################
# 
# "A findings domain relevant to the science of the form and structure of an 
# organism or of its parts. Macroscopic results (e.g., size, shape, color, and 
# abnormalities of body parts or specimens) that are seen by the naked eye or 
# observed via procedures such as imaging modalities, endoscopy, or other 
# technologies. Many morphology results are obtained from a procedure, although 
# information about the procedure may or may not be collected."

# This is an IDDO-created Custom Domain (utilizing a generic morphology/physiology 
# specification), as there is not a currently existing body-system domain that 
# encompasses the liver and spleen measurements we curate. This may be revisited 
# in a future version of the SDTMiG.

library(tidyverse)
library(naniar)
source("definitions.R")
load(paste0(wd, "/","MP.RData"))

missingness("MP")

# number of USUBJID per STUDYID with non-consecutive MBSEQ entries
MP_nonsec <- MP %>% 
  arrange(USUBJID, MPSEQ) %>% 
  group_by(USUBJID) %>% 
  mutate(test = MPSEQ == row_number()) %>% 
  mutate(nonsec = !all(test == TRUE)) %>% 
  ungroup() %>% 
  group_by(STUDYID) %>% 
  mutate(select = all(nonsec == FALSE))
MP_nonsec %>% 
  filter(select==FALSE) %>% 
  count(STUDYID, USUBJID, nonsec) %>% count(STUDYID, nonsec) 

MP %>% filter(STUDYID=="VVNGOE") %>% arrange(MPSEQ) %>% print(n = Inf)

MP %>% count(MPTESTCD, MPTEST) # either 'length' or 'width'!
MP %>% count(MPCAT) # '346 records relate to rescue treatment...'

MP %>% count(MPORRES)

MP %>% filter(MPORRES < 50 & MPTEST == "Length") %>% 
  ggplot(aes(x = MPORRES, fill = MPLOC)) +
  geom_histogram() + 
  facet_wrap(~ MPLOC, scales = "free")
  

MP %>% count(MPTEST, MPORRESU)
MP %>% count(MPSTRESC)
MP %>% count(MPSTRESN)
MP %>% count(MPSTRESU)
MP %>% count(MPLOC)
MP %>% count(MPMETHOD)

MP %>% filter(MPORRES < 50) %>% 
  ggplot(aes(x = MPSTRESN, fill = factor(MPTEST))) +
  geom_histogram()
