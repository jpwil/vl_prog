###############
# HAEMOGLOBIN #
###############

rm(list = ls())
library(tidyverse)
library(lme4)
load("Analysis/ads_clean.RData")
load("Analysis/ads_dirty.RData")

source("definitions.R")

ads_model <- ads_clean %>% 
  mutate(
    DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10,
    LB_BL_HGBs = (LB_BL_HGB - mean(LB_BL_HGB, na.rm = TRUE)) / 10,
    LB_IC_HGBs = (LB_IC_HGB - mean(LB_IC_HGB, na.rm = TRUE)) / 10,
    LB_DIFF_HGB = (LB_IC_HGB - LB_BL_HGB) / IC_DAYS,
    LB_IC_WBCs = (LB_IC_WBC - mean(LB_IC_WBC, na.rm = TRUE)),
    LB_IC_WBCls = log(LB_IC_WBC),
    LB_DIFF_WBCs = (LB_IC_WBC - LB_BL_WBC),
    LB_DIFF_WBCls = log(LB_IC_WBC) - log(LB_BL_WBC),
    LB_DIFF_BILIls = log(LB_IC_BILI) - log(LB_BL_BILI),
    LB_IC_ALTs = (LB_IC_ALT - mean(LB_IC_ALT, na.rm = TRUE)) / 10,
    LB_IC_ALTls = log(LB_IC_ALT),
    LB_IC_PLATs = (LB_IC_PLAT - mean(LB_IC_PLAT, na.rm = TRUE)) / 100,
    LB_IC_PLATls = log(LB_IC_PLAT),
    LB_IC_CREATs = (LB_IC_CREAT - mean(LB_IC_CREAT, na.rm = TRUE)) / 100,
    LB_IC_CREATls = log(LB_IC_CREAT),
    LB_IC_BILIls = log(LB_IC_BILI),
    LB_IC_ALBs = (LB_IC_ALB - mean(LB_IC_ALB, na.rm = TRUE)),
    LB_IC_UREANls = log(LB_IC_UREAN),
    MP_DIFF_SPLEEN = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH
    )

# plots ----

# baseline haemoglobin distribution
bhgb_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_HGB
    )
  ) +
  geom_histogram(
  )

bhgb_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_HGB
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_dist1.png", plot = bhgb_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_dist2.png", plot = bhgb_dist2, width = 16, height = 9)

# initial cure haemoglobin distribution

ihgb_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_HGB
    )
  ) +
  geom_histogram(
  )

ihgb_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_HGB
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/ihgb_dist1.png", plot = ihgb_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/ihgb_dist2.png", plot = ihgb_dist2, width = 16, height = 9)

# delta haemoglobin distribution

dhgb_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_HGB
    )
  ) +
  geom_histogram(
  )

dhgb_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_HGB
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/dhgb_dist1.png", plot = dhgb_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/dhgb_dist2.png", plot = dhgb_dist2, width = 16, height = 9)

# baseline and initial cure correlation

bhgb_ihgb1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_HGB,
      y = LB_IC_HGB
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bhgb_ihgb2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_HGB,
      y = LB_IC_HGB
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_ihgb1.png", plot = bhgb_ihgb1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_ihgb2.png", plot = bhgb_ihgb2, width = 16, height = 9)

# baseline and diff correlation

bhgb_dhgb1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_HGB,
      y = LB_DIFF_HGB
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bhgb_dhgb2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_HGB,
      y = LB_DIFF_HGB
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_dhgb1.png", plot = bhgb_dhgb1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_dhgb2.png", plot = bhgb_dhgb2, width = 16, height = 9)

# associations with outcome

bhgb_out1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_HGB,
      y = as.numeric(OUT_DC_RELAPSE)
    ) 
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  coord_cartesian(ylim = c(0, 0.2))

bhgb_out2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_HGB,
      y = as.numeric(OUT_DC_RELAPSE)
    ) 
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.4
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  coord_cartesian(ylim = c(0, 0.2)) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_out1.png", plot = bhgb_out1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_out2.png", plot = bhgb_out2, width = 16, height = 9)

# looks at other associations (age)

bhgb_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bhgb_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_age1.png", plot = bhgb_age1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_age2.png", plot = bhgb_age2, width = 16, height = 9)

# looks at other associations (spleen size)

bhgb_spleen1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = MP_BL_SPLEEN_LENGTH
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bhgb_spleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = MP_BL_SPLEEN_LENGTH
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_spleen1.png", plot = bhgb_spleen1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_spleen2.png", plot = bhgb_spleen2, width = 16, height = 9)

# looks at other associations (VL DURATION)

bhgb_dur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = VL_DURATION
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bhgb_dur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = VL_DURATION
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_dur1.png", plot = bhgb_dur1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_dur2.png", plot = bhgb_dur2, width = 16, height = 9)

# looks at other associations (log VL DURATION)

bhgb_ldur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = log(VL_DURATION)
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bhgb_ldur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = log(VL_DURATION)
    )
  ) + 
  geom_jitter(
    aes(
      colour = OUT_DC_RELAPSE
    )
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/hgb/bhgb_ldur1.png", plot = bhgb_ldur1, width = 16, height = 9)
ggsave(filename = "Analysis/hgb/bhgb_ldur2.png", plot = bhgb_ldur2, width = 16, height = 9)

# models ----

bhgb_uni1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_HGBs + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_uni1)
AIC(bhgb_uni1)

bhgb_uni2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_HGBs + I(LB_BL_HGBs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_uni2)
AIC(bhgb_uni2)

bhgb_uni3 <- glmer(
  OUT_DC_RELAPSE ~ LB_IC_HGBs + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_uni3)
AIC(bhgb_uni3)

bhgb_uni4 <- glmer(
  OUT_DC_RELAPSE ~ LB_DIFF_HGB + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_uni4)
AIC(bhgb_uni4)

bhgb_multi1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_HGBs + LB_IC_HGBs + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_multi1)
AIC(bhgb_multi1)

bhgb_multi2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_HGBs + LB_DIFF_HGB + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_multi2)
AIC(bhgb_multi2)

bhgb_multi3 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_HGBs + DM_SEX + DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_multi3)
AIC(bhgb_multi3)

bhgb_multi4 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_HGBs + DM_SEX + DM_AGEs + I(DM_AGEs^2) + MP_BL_SPLEEN_LENGTH + MB_COMBINED + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bhgb_multi4)
AIC(bhgb_multi4)