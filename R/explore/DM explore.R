###############
## DM DOMAIN ##
###############

# on mac; date character strings here are "YYYY-MM-DD"

library(tidyverse)
source("definitions.R")
load(paste0(wd, "/","DM.RData"))
source("definitions_dep.R")

missingness("DM")

DM %>% 
  group_by(STUDYID) %>% 
  summarise(num = n()) %>%
  ggplot() +
  geom_bar(aes(x = num, y = STUDYID),
           stat = "identity")

ds <- DM %>% 
  group_by(STUDYID) %>% 
  summarise(num = n()) %>% 
  arrange(num) %>% 
  mutate(STUDYID = as_factor(STUDYID))
ds %>% ggplot() +
  geom_bar(aes(x = num, y = STUDYID),
           stat = "identity")

quantile(ds$num, probs = seq(0,1, 0.25))

# RFSTDTC is a string, all same format dd/mm/yyyy
DM %>% group_by(STUDYID) %>% 
  summarise(na = sum(is.na(RFSTDTC)),
            num = n()) %>%
  mutate(missing = na/num) %>% 
  arrange(desc(missing))

DM %>% mutate(
  reg_exp = str_detect(RFSTDTC, ISO8601)) %>% 
  select(RFSTDTC, reg_exp) %>% 
  count(reg_exp)

### graphically present first clinical encounter dates for all participants

DM %>% 
  mutate(RFSTDTC = dmy(RFSTDTC),
         STUDYID = factor(STUDYID, levels = studyid_ce)) %>% 
  ggplot() + 
  geom_point(aes(x = STUDYID, y = RFSTDTC),
             na.rm = TRUE,
             size = 1.3,
             shape = "cross") +
  geom_boxplot(aes(x = STUDYID, y = RFSTDTC),
               na.rm = TRUE) + 
  scale_y_date(
    name = "Date of first clinical encounter",
    date_breaks = "1 year",
    date_labels = "%Y") + 
  coord_flip()

DM %>% 
  mutate(RFSTDTC = dmy(RFSTDTC),
  STUDYID = factor(STUDYID, levels = studyid_ce)) %>%
  group_by(STUDYID) %>% 
  summarise(RFSTDTC_0 = quantile(unclass(RFSTDTC), probs = 0, na.rm = TRUE),
            RFSTDTC_25 = quantile(unclass(RFSTDTC),probs = 0.25, na.rm = TRUE),
            RFSTDTC_50 = quantile(unclass(RFSTDTC), probs = 0.50, na.rm = TRUE),
            RFSTDTC_75 = quantile(unclass(RFSTDTC), probs = 0.75, na.rm = TRUE),
            RFSTDTC_100 = quantile(unclass(RFSTDTC), probs = 1.00, na.rm = TRUE)) %>% 
  mutate(across(-STUDYID, as_date)) %>% 
  arrange(RFSTDTC_0) 


# site ID

# distribution of countries per STUDYID
DM %>% count(STUDYID, COUNTRY) %>% 
  pivot_wider(names_from = COUNTRY,
              values_from = n,
              values_fill = 0
  ) %>% 
  mutate(across(-STUDYID, ~100*(./sum(.)),
                .names = "{.col}_p")) %>% 
  relocate(sort(colnames(test))) %>%
  relocate(STUDYID) %>% 
  print(n = Inf)

DM %>% count(STUDYID, COUNTRY, SITEID) %>% print(n = 50)

# arms
DM %>% count(STUDYID, ARMCD, ARM) %>% print(n = Inf)

## look at DMDTC and DMDY
DM %>% mutate(
  reg_exp = str_detect(DMDTC, ISO8601)) %>%
  count(reg_exp)

DM %>% count(is.na(DMDTC), is.na(DMDY))
DM %>% count(is.na(RFSTDTC), is.na(DMDTC))

DM %>% mutate(
  DMDIFF = dmy(DMDTC)-dmy(RFSTDTC)) %>% 
  count(DMDIFF)

DM %>% mutate(
  DMDIFF = dmy(DMDTC)-dmy(RFSTDTC)) %>% 
  count(DMDIFF)

DM %>% group_by(STUDYID) %>% 
  summarise(total = n(),
            RFSTDTC_missing = sum(is.na(RFSTDTC)),
            DMDTC_missing = sum(is.na(DMDTC)),
            RFSTDTC_missing_prop = sum(is.na(RFSTDTC))/n(),
            DMDTC_missing_prop = sum(is.na(DMDTC))/n()) %>% 
  print(n = Inf)

# AGE and SEX
DM %>% count(is.na(AGE), is.na(AGEU), is.na(SEX)) 
DM %>% count(AGEU, AGE)

# population pyramids

DM %>% 
  filter(!is.na(AGE)) %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, COUNTRY) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  scale_x_continuous(name = "Number of patients") + 
  scale_y_continuous(name = "Age (years)") + 
  facet_wrap(~COUNTRY, ncol = 2)


# ETHIOPIA
DM %>% 
  filter(!is.na(AGE), COUNTRY == "ETH") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# ETHIOPIA
DM %>% 
  filter(!is.na(AGE), COUNTRY == "ETH") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# BRAZIL
DM %>% 
  filter(!is.na(AGE), COUNTRY == "BRA") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# SUDAN
DM %>% 
  filter(!is.na(AGE), COUNTRY == "SDN") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# INDIA
DM %>% 
  filter(!is.na(AGE), COUNTRY == "IND") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# KENYA
DM %>% 
  filter(!is.na(AGE), COUNTRY == "KEN") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# GREECE
DM %>% 
  filter(!is.na(AGE), COUNTRY == "GRC") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# GREECE
DM %>% 
  filter(!is.na(AGE), COUNTRY == "NPL") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)

# UGANDA
DM %>% 
  filter(!is.na(AGE), COUNTRY == "UGA") %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  count(AGE_YR, SEX, STUDYID) %>% 
  ggplot() + 
  geom_bar(aes(x = AGE_YR, y= ifelse(SEX == "M", -n, n), fill = SEX),
           colour = "black",
           stat = "identity") +
  coord_flip() + 
  facet_wrap(~STUDYID)


DS %>% count(DSDECOD)
DS %>% count(DSDECOD, DSTERM) %>% 
  filter(DSDECOD=="COMPLETED") %>% 
  arrange(desc(n)) %>% 
  print(n = 300)
