#############
## OUTCOME ##
#############

rm(list = ls())
library(tidyverse)
library(lme4)
library(micemd)
ads_clean <- readRDS("data/ads_clean.rds")

# create a bar chart demontrating number of relapses in each cluster
ads_clean %>% names()
ads_clean %>% count(STUDYID, MB_COMBINED == 0) %>% print(n = Inf)
ads_outcome <- ads_clean %>% 
  select(STUDYID, OUT_DC_RELAPSE) %>% 
  group_by(STUDYID) %>% 
  summarise(n = n(), relapse = sum(OUT_DC_RELAPSE), prop = 100 * relapse / n) %>% 
  arrange(desc(n)) %>% 
  mutate(STUDYID = reorder(STUDYID, -n))

ads_outcome_long <- ads_outcome %>% 
  mutate(no_relapse = n - relapse) %>% 
  pivot_longer(
    cols = c("relapse", "no_relapse"),
    names_to = "relapse"
  )

ads_outcome$prop_low <- 0
ads_outcome$prop_high <- 0

for (i in seq_len(nrow(ads_outcome))) {
  ads_outcome[i, 5] <- binom.test(ads_outcome[[i, 3]], ads_outcome[[i, 2]], conf.level = 0.95)$conf.int[[1]]
  ads_outcome[i, 6] <- binom.test(ads_outcome[[i, 3]], ads_outcome[[i, 2]], conf.level = 0.95)$conf.int[[2]]
}

bar_chart1 <- ads_outcome_long %>% 
  ggplot() + 
  geom_bar(
    aes(
      x = STUDYID,
      y = value,
      fill = factor(relapse)
    ),
    stat = "identity"
  ) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Number of patients by IDDO VL Dataset and outcome",
    x = "IDDO VL Dataset",
    y = "Number of patients",
    subtitle = "Patients who achieve initial cure"
  ) + 
  scale_fill_manual(
    values = c("relapse" = "#bd5e5e", "no_relapse" = "#80ba80"),
    labels = c("No relapse", "Relapse"),
    name = "Relapse status"
  )

bar_chart1


ads_outcome

bar_chart2 <- ads_outcome_long %>% 
  ggplot() + 
  geom_bar(
    aes(
      x = STUDYID,
      y = value,
      fill = factor(relapse)
    ),
    stat = "identity",
    position = "fill"
  ) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Proportion of patients by outcome across datasets",
    x = "IDDO VL Dataset",
    y = "Proportion of patients",
    subtitle = "With 95% confidence intervals"
  ) + 
  scale_fill_manual(
    values = c("relapse" = "#bd5e5e", "no_relapse" = "#80ba80"),
    labels = c("No relapse", "Relapse"),
    name = "Relapse status"
  ) + 
  geom_errorbar(
    data = ads_outcome,
    aes(
      x = STUDYID,
      ymin = prop_low,
      ymax = prop_high
    ),
    width = 0.15
  )

bar_chart2


