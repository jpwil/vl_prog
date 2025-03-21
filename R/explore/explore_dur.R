##############
## DURATION ##
##############

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
    MP_DIFF_SPLEEN = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH,
    DURl = log(VL_DURATION)
    )

ads_model %>% var_sum(DURl) # 41.5% missing

# plots ----

# distributions

dur_dist1a <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VL_DURATION
    )
  )

dur_dist1b <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DURl
    )
  )

dur_dist2a <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VL_DURATION
    )
  ) + 
  facet_wrap(~STUDYID)

dur_dist2b <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DURl
    )
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/dur/dur_dist1a.png", plot = dur_dist1a, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_dist1b.png", plot = dur_dist1b, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_dist2a.png", plot = dur_dist2a, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_dist2b.png", plot = dur_dist2b, width = 16, height = 9)

# outcome

dur_out1a <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

dur_out2a <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  facet_wrap(~STUDYID)

dur_out2b <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  facet_wrap(~DM_ARM)

dur_out1b <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  coord_cartesian(
    ylim = c(-0.1, 0.2)
  ) +
  labs(caption = "LOESS")

dur_out2b <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  coord_cartesian(
    ylim = c(-0.1, 0.2)
  ) +
  labs(caption = "LOESS") +
  facet_wrap(~STUDYID)

dur_out1c <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
  ) +
  coord_cartesian(
    ylim = c(-0.1, 0.2)
  ) +
  labs(caption = "binomial glm")


dur_out2c <- ads_model %>% 
  ggplot(
    aes(
      x = DURl,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "glm", 
    method.args = list(family = "binomial"),
    se = TRUE
  ) +
  coord_cartesian(
    ylim = c(-0.1, 0.2)
  ) +
  labs(caption = "binomial glm") +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/dur/dur_out1a.png", plot = dur_out1a, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_out1b.png", plot = dur_out1b, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_out1c.png", plot = dur_out1c, width = 16, height = 9)

ggsave(filename = "Analysis/dur/dur_out2a.png", plot = dur_out2a, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_out2b.png", plot = dur_out2b, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_out2c.png", plot = dur_out2c, width = 16, height = 9)

# associations (age and sex)

dur_age_sex1 <- ads_model %>% 
  ggplot(
    aes(
      x = DM_AGE,
      y = DURl,
      colour = DM_SEX
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  )

dur_age_sex2 <- ads_model %>% 
  ggplot(
    aes(
      x = DM_AGE,
      y = DURl,
      colour = DM_SEX
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/dur/dur_age_sex1.png", plot = dur_age_sex1, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_age_sex2.png", plot = dur_age_sex2, width = 16, height = 9)

# associations (haemoglobin)

dur_hgb1 <- ads_model %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = DURl
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  )

dur_hgb2 <- ads_model %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = DURl
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/dur/dur_hgb1.png", plot = dur_hgb1, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_hgb2.png", plot = dur_hgb2, width = 16, height = 9)

# haemoglobin and sex
dur_hgb_sex1 <- ads_model %>% 
  ggplot(
    aes(
      y = LB_BL_HGB,
      x = DURl,
      colour = DM_SEX
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) 

ggsave(filename = "Analysis/dur/dur_hgb_sex1.png", plot = dur_hgb_sex1, width = 16, height = 9)

# spleen size
dur_spleen1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = DURl,
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) 

ggsave(filename = "Analysis/dur/dur_spleen1.png", plot = dur_spleen1, width = 16, height = 9)

# spleen size and sex
dur_spleen_sex1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = DURl,
      colour = DM_SEX
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) 

ggsave(filename = "Analysis/dur/dur_spleen_sex1.png", plot = dur_spleen_sex1, width = 16, height = 9)

# parasitaemia
dur_para1 <- ads_model %>% 
  ggplot(
    aes(
      y = MB_COMBINED,
      x = DURl,
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) 

dur_para2 <- ads_model %>% 
  ggplot(
    aes(
      y = MB_COMBINED,
      x = DURl,
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/dur/dur_para1.png", plot = dur_para1, width = 16, height = 9)
ggsave(filename = "Analysis/dur/dur_para2.png", plot = dur_para2, width = 16, height = 9)

# VL_HISTORY

# spleen size and sex
dur_vlhx1 <- ads_model %>% 
  ggplot(
    aes(
      y = VL_HISTORY,
      x = DURl
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.2
  ) + 
  geom_smooth(
    method = "loess"
  ) 

ggsave(filename = "Analysis/dur/dur_vlhx1.png", plot = dur_vlhx1, width = 16, height = 9)


# models ----

# univariable models

dur_uni1a <- glmer(
  OUT_DC_RELAPSE ~ DURl + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_uni1)

dur_uni1b <- glmer(
  OUT_DC_RELAPSE ~ VL_DURATION + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_uni1b)

# multivariable models
dur_multi1 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + DURl + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi1)

dur_multi2 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + DURl + LB_BL_HGB + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi2)

dur_multi3 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + DURl + LB_BL_HGB + MP_BL_SPLEEN_LENGTH + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi3)

dur_multi4 <- glmer(
  OUT_DC_RELAPSE ~ DM_SEX + DM_AGEs + I(DM_AGEs^2) + DURl + LB_BL_HGB + MP_BL_SPLEEN_LENGTH + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi4)

dur_multi5 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + DURl + LB_BL_HGB + MB_COMBINED + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi5)

dur_multi6 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + DURl + LB_DIFF_HGB + MB_COMBINED + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi6)

dur_multi7 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + DURl + MB_COMBINED + MP_BL_SPLEEN_LENGTH + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi7)

dur_multi8 <- glmer(
  OUT_DC_RELAPSE ~  DURl + VL_HISTORY + (1 | STUDYID),
  family = binomial(),
  data = ads_model
)
summary(dur_multi8)
