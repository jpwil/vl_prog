# EXCLUDE (due to data provenance concerns)

##########
# VBNRQO #
########## 

## PUBLICATION

# Protocol only CTRI/2017/05/008656
# https://trialsearch.who.int/Trial2.aspx?TrialID=CTRI/2017/05/008656
# https://www.ctri.nic.in/Clinicaltrials/pmaindet2.php?EncHid=MTkwNTE=&Enc=&userName=CTRI/2017/05/008656

# A multicenter, open label, randomized, two treatment, parallel design, steady state study to compare the bioavailability of the Test product 
# [Amphotericin B (Liposome for injection) 50 mg/vial, Cipla Ltd., India] with Reference product [AmBisomeÂ® (Amphotericin B) Liposome for injection 50 mg/vial), 
# Gilead life sciences, USA] in adult patients with visceral leishmaniasis.

# Trial registered 25/05/2017 (prospectively)
# Last edited 16/10/2018

# Primary sponsor is Cipla Ltd India

## DATASET

# DATA PROVENANCE ISSUES
# Only 85 identified as cure at 6 months, all complete recovery after initial cure assessment at 6 days
# no relapses identified

rm(list = ls())
source("definitions.R")
load_domains("VBNRQO")

mg <- ld_missingness("VBNRQO")
mg %>%
  count_na() %>%
  print(width = Inf)

# DM ----
DM %>% count()
DM %>% names()
DM %>% mutate(DMDTC = dmy(DMDTC)) # %>% View() recruited from 16th July 2017 to 31st October 2018

# OUTCOME ---

# All DS outcomes are 6 month follow-up
DS %>% count(DSTERM) # 85 cure and 21 unknown

# RS has two time points, Day 6(TOC) and 6-month follow-up (OVRLRESP) 
# TOC: COMPLETE RECOVERY (100%) and OVRLRESP: CURE in 85 (no other entries)
RS %>% names()
RS %>% count(VISIT, RSTESTCD, RSCAT)
RS %>% count_dup(VISIT, RSTESTCD, RSORRES)

RS #%>% View()

# LB ----
# Day 0 and Day 6 values
LB %>% count(VISIT, LBTESTCD) %>% print(n = 200)

MB %>% count(VISIT, MBTESTCD, MBORRES, MBLOC) # baseline splenic aspirate parasitaemia is available

SA %>% count(VISIT, SATERM, !is.na(SADUR)) # no duration of illness, a few baseline symptoms/signs

MP %>% count(VISIT, MPTESTCD, MPLOC) # spleen and liver sizes at baseline and day 6

VS %>% count(VISIT, VSTESTCD) # observations available

PE %>% count(VISIT, PETESTCD, PEORRES) # all physical examinations reported as normal

QS %>% count(VISIT, QSTESTCD) # KPSS01

RP %>% count(VISIT, RPTESTCD, RPORRES) # negative pregnancy test in 36

TI
TS
TV