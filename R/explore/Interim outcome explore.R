######################
## INTERIM OUTCOMES ##
######################

library(tidyverse)
library(UpSetR)
library(gridExtra)

source("definitions.R")

# load datasets for merging
load(paste0(wd, "/", "DM.RData"))
load(paste0(pd, "/RData/", "DD_merge.RData"))
load(paste0(pd, "/RData/", "DS_relapse.RData"))
load(paste0(pd, "/RData/", "DS_death.RData"))
load(paste0(pd, "/RData/", "RS_merge1.RData"))
load(paste0(pd, "/RData/", "RS_merge2.RData"))

# merge by USUBJIB

# DD death data
DM_v1 <- DM %>% 
  left_join(DD_merge,
            unmatched = "error") %>% 
  select(STUDYID, DTHFL, COUNTRY, USUBJID, DD_present) %>% 
  mutate(DD_present = if_else(is.na(DD_present),0,DD_present))

# DS relapse data
DM_v2 <- DM_v1 %>% 
  left_join(DS_relapse,
            unmatched = "error") %>% 
  select(STUDYID, DTHFL, COUNTRY, USUBJID, DD_present, DS_relapse) %>% 
  mutate(DS_relapse = if_else(is.na(DS_relapse),0,DS_relapse))

# DS death data
DM_v3 <- DM_v2 %>% 
  left_join(DS_death,
            unmatched = "error") %>% 
  select(STUDYID, DTHFL, COUNTRY, USUBJID, DS_death, DD_present, DS_relapse) %>% 
  mutate(DS_death = if_else(is.na(DS_death),0,DS_death))

# RS relapse data1
DM_v4 <- DM_v3 %>% 
  left_join(RS_merge1,
            unmatched = "error") %>% 
  select(STUDYID, DTHFL, COUNTRY, USUBJID, RS_relapse1, DS_death, DD_present, DS_relapse) %>% 
  mutate(RS_relapse1 = if_else(is.na(RS_relapse1),0,RS_relapse1)) %>% 
  mutate(DM_death = if_else(is.na(DTHFL), FALSE, TRUE)) %>% 
  select(-DTHFL)

# RS relapse data2
DM_v5 <- DM_v4 %>% 
  left_join(RS_merge2,
            unmatched = "error") %>% 
  select(STUDYID, COUNTRY, USUBJID, RS_relapse1, RS_relapse2, DM_death, DS_death, DD_present, DS_relapse) %>% 
  mutate(RS_relapse2 = if_else(is.na(RS_relapse2),0,RS_relapse2))

# let's look at the inter-relatedness of the outcomes for the interim analysis

DM_v5 %>%
  rename(DD_death = DD_present) %>% 
  select(-STUDYID, -COUNTRY, -DS_relapse, -RS_relapse1, -RS_relapse2) %>% 
  mutate_all(~ifelse(.==0, 0L, 1L)) %>% as.data.frame() %>%  # need to convert to dataframe!!!!
  upset(nsets = 15, 
        nintersects = 50, 
        order.by = c("freq"), 
        set_size.show = TRUE, 
        set_size.scale_max = nrow(DM_v4)+nrow(DM_v4)/5)

DM_v5 %>%
  rename(DD_death = DD_present) %>% 
  select(-STUDYID, -COUNTRY, -DD_death, -DS_death, -DM_death) %>% 
  mutate_all(~ifelse(.==0, 0L, 1L)) %>% as.data.frame() %>%  # need to convert to dataframe!!!!
  upset(nsets = 15, 
        nintersects = 50, 
        order.by = c("freq"), 
        set_size.show = TRUE, 
        set_size.scale_max = nrow(DM_v4)+nrow(DM_v4)/5)

# can create an indicator for death and relapse
DM_outcome <- DM_v5 %>% 
  mutate(death = (DD_present == TRUE | DS_death == TRUE),
         relapse = (RS_relapse2 == TRUE | DS_relapse == TRUE)) %>%
  select(STUDYID,COUNTRY, USUBJID,death,relapse)
  
# summarise number of deaths at study level
DM_outcome_aggregated1 <- DM_outcome %>%
  group_by(STUDYID) %>% 
  summarise(n = n(),
         death = sum(death),
         relapse = sum(relapse))

# summarise number of deaths at country level
DM_outcome_aggregated2 <- DM_outcome %>%
  group_by(COUNTRY) %>% 
  summarise(n = n(),
            death = sum(death),
            relapse = sum(relapse))

# summarise number of deaths at country AND study level
DM_outcome_aggregated3 <- DM_outcome %>%
  group_by(STUDYID, COUNTRY) %>% 
  summarise(n = n(),
            death = sum(death),
            relapse = sum(relapse))

# overall number of deaths and relapse
DM_outcome_aggregated1  %>% 
  ggplot() +
  geom_col(mapping = aes(x = STUDYID, y = n))

# match with some demographic data
demo1 <- DM %>% 
  filter(!is.na(AGE)) %>% 
  filter(!is.na(SEX)) %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  select(STUDYID, COUNTRY, AGE_YR, SEX) %>% 
  group_by(STUDYID) %>% 
  summarise(total = n(),
            age_min = quantile(AGE_YR, 0),
            age_25 = quantile(AGE_YR, 0.25),
            age_50 = quantile(AGE_YR, 0.50),
            age_75 = quantile(AGE_YR, 0.75),
            age_max = quantile(AGE_YR, 1.00),
            male = sum(SEX=="M"),
            female = sum(SEX=="F"),
            male_perc = sum(SEX=="M")/n())

demo_out1 <- DM_outcome_aggregated1 %>% 
  left_join(demo1)

# match with some demographic data (country)
demo2 <- DM %>% 
  filter(!is.na(AGE)) %>% 
  filter(!is.na(COUNTRY)) %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  select(COUNTRY, AGE_YR, SEX) %>% 
  group_by(COUNTRY) %>% 
  summarise(total = n(),
            age_min = quantile(AGE_YR, 0),
            age_25 = quantile(AGE_YR, 0.25),
            age_50 = quantile(AGE_YR, 0.50),
            age_75 = quantile(AGE_YR, 0.75),
            age_max = quantile(AGE_YR, 1.00),
            male = sum(SEX=="M"),
            female = sum(SEX=="F"),
            male_perc = sum(SEX=="M")/n())
demo_out2 <- DM_outcome_aggregated2 %>% 
  left_join(demo2)

# match with some demographic data (country AND study ID)
demo3 <- DM %>% 
  filter(!is.na(AGE)) %>% 
  filter(!is.na(COUNTRY)) %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  select(STUDYID, COUNTRY, AGE_YR, SEX) %>% 
  group_by(COUNTRY, STUDYID) %>% 
  summarise(total = n(),
            age_min = quantile(AGE_YR, 0),
            age_25 = quantile(AGE_YR, 0.25),
            age_50 = quantile(AGE_YR, 0.50),
            age_75 = quantile(AGE_YR, 0.75),
            age_max = quantile(AGE_YR, 1.00),
            male = sum(SEX=="M"),
            female = sum(SEX=="F"),
            male_perc = sum(SEX=="M")/n())
demo_out3 <- DM_outcome_aggregated3 %>% 
  left_join(demo3)

# plot sex by STUDYID
study_sex <- demo_out1 %>% 
  pivot_longer(
    cols = c("male", "female"),
    names_to = "sex") %>% 
  select(STUDYID, n, sex, value) %>% 
  arrange((STUDYID)) 

plot1 <- study_sex %>% 
  ggplot() + 
  geom_col(aes(x = reorder(STUDYID, n), y = value, fill = sex),
           position = "stack") + 
  coord_flip() +
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_discrete(name = "IDDO Study ID") +
  theme(legend.position = "none")
plot1


plot2 <- study_sex %>% 
  ggplot() + 
  geom_col(aes(x = reorder(STUDYID, n), y = value, fill = sex),
           position = "fill") + 
  coord_flip() +
  scale_y_continuous(name = "Sex proportion") + 
  scale_x_discrete(position = "top", name = "")
plot2

grid.arrange(plot1, plot2, ncol = 2)

# plot sex by COUNTRY
study_sex <- demo_out2 %>% 
  pivot_longer(
    cols = c("male", "female"),
    names_to = "sex") %>% 
  select(COUNTRY, n, sex, value) %>% 
  arrange((COUNTRY)) 

plot1 <- study_sex %>% 
  ggplot() + 
  geom_col(aes(x = reorder(COUNTRY, n), y = value, fill = sex),
           position = "stack") + 
  coord_flip() +
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_discrete(name = "Country",
                   label = c("Greece",
                             "Uganda",
                             "Kenya",
                             "Brazil",
                             "Nepal",
                             "Ethiopia",
                             "Sudan (The)",
                             "India")) +
  theme(legend.position = "none")
plot1


plot2 <- study_sex %>% 
  ggplot() + 
  geom_col(aes(x = reorder(COUNTRY, n), y = value, fill = sex),
           position = "fill") + 
  coord_flip() +
  scale_y_continuous(name = "Sex proportion") + 
  scale_x_discrete(position = "top", 
                   name = "",
                   label = c("Greece",
                             "Uganda",
                             "Kenya",
                             "Brazil",
                             "Nepal",
                             "Ethiopia",
                             "Sudan (The)",
                             "India"))
plot2

grid.arrange(plot1, plot2, ncol = 2)

# PLOT SEX BY COUNTRY AND STUDYID **
study_sex <- demo_out3 %>% 
  pivot_longer(
    cols = c("male", "female"),
    names_to = "sex") %>% 
  select(COUNTRY, STUDYID, n, sex, value)

study_sex2 <- study_sex %>% 
  ungroup() %>% 
  pivot_wider(names_from = sex, values_from = value) %>% 
  arrange(COUNTRY, n) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = c("male", "female"),
               names_to = "sex")
label <- study_sex2$STUDYID

plot1 <- study_sex2 %>% 
  ungroup() %>% 
  ggplot() + 
  coord_flip() + 
  geom_col(aes(x = id, y = value, fill = sex),
           position = "stack") + 
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_continuous(name = "Country",
                     label = study_sex2$STUDYID,
                     breaks = study_sex2$id,
                     expand = c(0,0)) +
  facet_grid(rows = vars(COUNTRY), switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  theme(legend.position = "none")
plot1

plot2 <- study_sex2 %>% 
  ungroup() %>% 
  ggplot() + 
  coord_flip() + 
  geom_col(aes(x = id, y = value, fill = sex),
           position = "fill") + 
  scale_y_continuous(name = "Proportion of subjects") + 
  scale_x_continuous(name = "Country",
                     label = study_sex2$STUDYID,
                     breaks = study_sex2$id,
                     expand = c(0,0),
                     position = "top") +
  scale_fill_discrete(name = "Sex", label = c("Male", "Female")) + 
  facet_grid(rows = vars(COUNTRY), switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  theme(legend.position = "right")
plot2

grid.arrange(plot1, plot2, ncol = 2)


# create bar charts of death and relapse outcomes
temp <- demo_out1 %>%
  mutate(death_no = n - death) %>% 
  pivot_longer(cols = c("death_no", "death")) %>% 
  select(STUDYID, n, name, value)
temp

# by studyid
plot1 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(STUDYID, n), y = value, fill = name),
           position = "stack") + 
  coord_flip() +
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_discrete(name = "IDDO Study ID") +
  theme(legend.position = "none")
plot1


plot2 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(STUDYID, n), y = value, fill = name),
           position = "fill") + 
  coord_flip() +
  scale_y_continuous(name = "Proportion alive") + 
  scale_x_discrete(position = "top", name = "") + 
  scale_fill_discrete(name = "Alive at study end?",
                    label = c("No",
                              "Yes"))
plot2

grid.arrange(plot1, plot2, ncol = 2)

# create bar charts of DEATH outcomes (COUNTRY)
temp <- demo_out2 %>%
  mutate(death_no = n - death) %>% 
  pivot_longer(cols = c("death_no", "death")) %>% 
  select(COUNTRY, n, name, value)
temp

# by country
plot1 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(COUNTRY, n), y = value, fill = name),
           position = "stack") + 
  coord_flip() +
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_discrete(name = "Country",
                   label = c("Greece",
                             "Uganda",
                             "Kenya",
                             "Brazil",
                             "Nepal",
                             "Ethiopia",
                             "Sudan (The)",
                             "India")) +
  theme(legend.position = "none")

# by country and studyid

temp <- demo_out3 %>%
  ungroup() %>% 
  mutate(death_no = n - death) %>% 
  arrange(COUNTRY, n) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = c("death_no", "death"),
               names_to = "death") %>% 
  select(COUNTRY, STUDYID, id, n, death, value)

plot1 <- temp %>% 
  ggplot() + 
  coord_flip() + 
  geom_col(aes(x = id, y = value, fill = death),
           position = "stack") + 
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_continuous(name = "Country",
                     label = temp$STUDYID,
                     breaks = temp$id,
                     expand = c(0,0)) +
  facet_grid(rows = vars(COUNTRY), switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  theme(legend.position = "none")
plot1

plot2 <- temp %>% 
  ungroup() %>% 
  ggplot() + 
  coord_flip() + 
  geom_col(aes(x = id, y = value, fill = death),
           position = "fill") + 
  scale_y_continuous(name = "Proportion of subjects") + 
  scale_x_continuous(name = "Country",
                     label = temp$STUDYID,
                     breaks = temp$id,
                     expand = c(0,0),
                     position = "top") +
  scale_fill_discrete(name = "Died?", label = c("Yes", "No")) + 
  facet_grid(rows = vars(COUNTRY), switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  theme(legend.position = "right")
plot2

grid.arrange(plot1, plot2, ncol = 2)



plot1


plot2 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(COUNTRY, n), y = value, fill = name),
           position = "fill") + 
  coord_flip() +
  scale_y_continuous(name = "Proportion alive") + 
  scale_x_discrete(position = "top", name = "",
                   label = c("Greece",
                             "Uganda",
                             "Kenya",
                             "Brazil",
                             "Nepal",
                             "Ethiopia",
                             "Sudan (The)",
                             "India")) + 
  scale_fill_discrete(name = "Alive at study end?",
                      label = c("No",
                                "Yes"))
plot2

grid.arrange(plot1, plot2, ncol = 2)


# create bar charts of RELAPSE outcomes (COUNTRY)
temp <- demo_out2 %>%
  mutate(relapse_no = n - relapse) %>% 
  pivot_longer(cols = c("relapse_no", "relapse")) %>% 
  select(COUNTRY, n, name, value)
temp

# by country
plot1 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(COUNTRY, n), y = value, fill = name),
           position = "stack") + 
  coord_flip() +
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_discrete(name = "Country",
                   label = c("Greece",
                             "Uganda",
                             "Kenya",
                             "Brazil",
                             "Nepal",
                             "Ethiopia",
                             "Sudan (The)",
                             "India")) +
  theme(legend.position = "none")
plot1


plot2 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(COUNTRY, n), y = value, fill = name),
           position = "fill") + 
  coord_flip() +
  scale_y_continuous(name = "Proportion free of relapse") + 
  scale_x_discrete(position = "top", name = "",
                   label = c("Greece",
                             "Uganda",
                             "Kenya",
                             "Brazil",
                             "Nepal",
                             "Ethiopia",
                             "Sudan (The)",
                             "India")) + 
  scale_fill_discrete(name = "Relapse by study end?",
                      label = c("Yes",
                                "No"))
plot2

grid.arrange(plot1, plot2, ncol = 2)


# create bar charts of RELAPSE outcomes (STUDYID)
temp <- demo_out1 %>%
  mutate(relapse_no = n - relapse) %>% 
  pivot_longer(cols = c("relapse_no", "relapse")) %>% 
  select(STUDYID, n, name, value)
temp

# by study ID
plot1 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(STUDYID, n), y = value, fill = name),
           position = "stack") + 
  coord_flip() +
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_discrete(name = "Study ID") +
  theme(legend.position = "none")
plot1


plot2 <- temp %>% 
  ggplot() + 
  geom_col(aes(x = reorder(STUDYID, n), y = value, fill = name),
           position = "fill") + 
  coord_flip() +
  scale_y_continuous(name = "Proportion free of relapse") + 
  scale_x_discrete(position = "top", name = "") + 
  scale_fill_discrete(name = "Relapse by study end?",
                      label = c("Yes",
                                "No"))
plot2

grid.arrange(plot1, plot2, ncol = 2)

# RELAPSE by country and studyid

temp <- demo_out3 %>%
  ungroup() %>% 
  mutate(relapse_no = n - relapse) %>% 
  arrange(COUNTRY, n) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = c("relapse_no", "relapse"),
               names_to = "relapse") %>% 
  select(COUNTRY, STUDYID, id, n, relapse, value)

plot1 <- temp %>% 
  ggplot() + 
  coord_flip() + 
  geom_col(aes(x = id, y = value, fill = relapse),
           position = "stack") + 
  scale_y_continuous(name = "Number of subjects") + 
  scale_x_continuous(name = "Country",
                     label = temp$STUDYID,
                     breaks = temp$id,
                     expand = c(0,0)) +
  facet_grid(rows = vars(COUNTRY), switch = "y", scales = "free_y", space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  theme(legend.position = "none")
plot1

plot2 <- temp %>% 
  ungroup() %>% 
  ggplot() + 
  coord_flip() + 
  geom_col(aes(x = id, y = value, fill = relapse),
           position = "fill") + 
  scale_y_continuous(name = "Proportion of subjects") + 
  scale_x_continuous(name = "Country",
                     label = temp$STUDYID,
                     breaks = temp$id,
                     expand = c(0,0),
                     position = "top") +
  scale_fill_discrete(name = "Relapse?", label = c("Yes", "No")) + 
  facet_grid(rows = vars(COUNTRY), 
             switch = "y", 
             scales = "free_y", 
             space = "free_y") +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside") +
  theme(legend.position = "right")
plot2

grid.arrange(plot1, plot2, ncol = 2)

# save tables for presentation
demo_out1 %>% write_csv(file = "demo_out1.csv")
demo_out2 %>% write_csv(file = "demo_out2.csv")
demo_out3 %>% arrange(COUNTRY, desc(n)) %>% write_csv(file = "demo_out3.csv")


DM %>% 
  filter(!is.na(AGE)) %>%
  filter(!is.na(SEX)) %>% 
  mutate(AGE_YR = ifelse(AGEU == "MONTHS", round(AGE/12), round(AGE))) %>% 
  ungroup() %>% 
  summarise(n = n(),
            median = median(AGE_YR),
            age_0 = min(AGE_YR),
            age_25 = quantile(AGE_YR, 0.25),
            age_75 = quantile(AGE_YR, 0.75),
            age_100 = quantile(AGE_YR, 1),
            male = sum(SEX=="M"))


demo_out3 %>% 
  group_by(COUNTRY) %>% 
  summarise(
    study_no = n_distinct(STUDYID), 
    patients_total = sum(n), 
    deaths = sum(death), 
    deaths_pct = 100 * sum(death) / sum(n),
    relapses = sum(relapse),
    relapses_pct = 100 * sum(relapse) / sum(n)
  )

demo_out4 <- demo_out3 %>% 
  mutate(
    region = case_when(
      COUNTRY %in% c("BRA") ~ "Americas",
      COUNTRY %in% c("ETH", "KEN", "SDN", "UGA") ~ "East Africa",
      COUNTRY %in% c("IND", "NPL") ~ "Indian Subcontinent",
      COUNTRY %in% c("GRC") ~ "Mediterranean",
      .default = "ERROR"
    ) 
  ) %>% 
  group_by(region) %>% 
  summarise(
    study_no = n_distinct(STUDYID), 
    patients_total = sum(n), 
    deaths = sum(death), 
    deaths_pct = 100 * sum(death) / sum(n),
    relapses = sum(relapse),
    relapses_pct = 100 * sum(relapse) / sum(n)
  )
demo_out4

demo_out3 %>% ungroup() %>% 
  summarise(
    study_no = n_distinct(STUDYID), 
    patients_total = sum(n), 
    deaths = sum(death), 
    deaths_pct = 100 * sum(death) / sum(n),
    relapses = sum(relapse),
    relapses_pct = 100 * sum(relapse) / sum(n)
  )
