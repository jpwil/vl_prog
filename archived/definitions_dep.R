
# order STUDYIDs by first RFSTDTC per study

studyid_ce <- DM %>%
  mutate(date = dmy(RFSTDTC)) %>% 
  group_by(STUDYID) %>% 
  summarise(min_date = min(date)) %>% 
  filter(!is.na(min_date)) %>% 
  arrange(desc(min_date)) %>% pull(STUDYID)
