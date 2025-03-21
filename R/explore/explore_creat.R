##############
# CREATININE #
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
    LB_BL_WBCls = log(LB_BL_WBC),
    LB_IC_WBCs = (LB_IC_WBC - mean(LB_IC_WBC, na.rm = TRUE)),
    LB_IC_WBCls = log(LB_IC_WBC),
    LB_DIFF_WBC = (LB_IC_WBC - LB_BL_WBC) / IC_DAYS,
    LB_DIFF_BILIls = log(LB_IC_BILI) - log(LB_BL_BILI),
    LB_IC_ALTs = (LB_IC_ALT - mean(LB_IC_ALT, na.rm = TRUE)) / 10,
    LB_BL_ALTls = log(LB_BL_ALT),
    LB_IC_ALTls = log(LB_IC_ALT),
    LB_DIFF_ALT = (LB_IC_ALT - LB_BL_ALT) / IC_DAYS,
    LB_IC_PLATs = (LB_IC_PLAT - mean(LB_IC_PLAT, na.rm = TRUE)) / 100,
    LB_BL_PLATls = log(LB_BL_PLAT),
    LB_IC_PLATls = log(LB_IC_PLAT),
    LB_DIFF_PLAT = (LB_IC_PLAT - LB_BL_PLAT) / IC_DAYS,
    LB_IC_CREATs = (LB_IC_CREAT - mean(LB_IC_CREAT, na.rm = TRUE)) / 100,
    LB_DIFF_CREAT = (LB_IC_CREAT - LB_BL_CREAT) / IC_DAYS,
    LB_BL_CREATls = log(LB_BL_CREAT),
    LB_IC_CREATls = log(LB_IC_CREAT),
    LB_IC_BILIls = log(LB_IC_BILI),
    LB_IC_ALBs = (LB_IC_ALB - mean(LB_IC_ALB, na.rm = TRUE)),
    LB_IC_UREANls = log(LB_IC_UREAN),
    MP_DIFF_SPLEEN = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS
    )

# plots ----

# baseline creat distribution
bcreat_dist1a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREAT
    )
  ) +
  geom_histogram(
  )

bcreat_dist1b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREATls
    )
  ) +
  geom_histogram(
  )

bcreat_dist2a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREAT
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

bcreat_dist2b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREATls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/creat/bcreat_dist1a.png", plot = bcreat_dist1a, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_dist2a.png", plot = bcreat_dist2a, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_dist1b.png", plot = bcreat_dist1b, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_dist2b.png", plot = bcreat_dist2b, width = 16, height = 9)

# initial cure creat distribution

icreat_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_CREATls
    )
  ) +
  geom_histogram(
  )

icreat_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_CREATls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/creat/icreat_dist1.png", plot = icreat_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/icreat_dist2.png", plot = icreat_dist2, width = 16, height = 9)

# delta creat distribution

dcreat_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_CREAT
    )
  ) +
  geom_histogram(
  )

dcreat_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_CREAT
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/creat/dcreat_dist1.png", plot = dcreat_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/dcreat_dist2.png", plot = dcreat_dist2, width = 16, height = 9)

# baseline and initial cure correlation

bcreat_icreat1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREAT,
      y = LB_IC_CREAT
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bcreat_icreat2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREAT,
      y = LB_IC_CREAT
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/creat/bcreat_icreat1.png", plot = bcreat_icreat1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_icreat2.png", plot = bcreat_icreat2, width = 16, height = 9)

# baseline and diff correlation

bcreat_dcreat1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_CREAT,
      y = LB_DIFF_CREAT
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

bcreat_dcreat2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_CREAT,
      y = LB_DIFF_CREAT
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

ggsave(filename = "Analysis/creat/bcreat_dcreat1.png", plot = bcreat_dcreat1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_dcreat2.png", plot = bcreat_dcreat2, width = 16, height = 9)

# associations with outcome

bcreat_out1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREATls,
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

icreat_out1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_CREATls,
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

bcreat_out2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_CREATls,
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

icreat_out2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_CREATls,
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

ggsave(filename = "Analysis/creat/bcreat_out1.png", plot = bcreat_out1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_out2.png", plot = bcreat_out2, width = 16, height = 9)
ggsave(filename = "Analysis/creat/icreat_out1.png", plot = icreat_out1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/icreat_out2.png", plot = icreat_out2, width = 16, height = 9)

# looks at other associations (age)

bcreat_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREATls,
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

bcreat_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREATls,
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

ggsave(filename = "Analysis/creat/bcreat_age1.png", plot = bcreat_age1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_age2.png", plot = bcreat_age2, width = 16, height = 9)

# looks at other associations (spleen size)

bcreat_spleen1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREAT,
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

bcreat_spleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREAT,
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

ggsave(filename = "Analysis/creat/bcreat_spleen1.png", plot = bcreat_spleen1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_spleen2.png", plot = bcreat_spleen2, width = 16, height = 9)

# looks at other associations (VL DURATION)

bcreat_dur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREATls,
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

bcreat_dur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREATls,
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

ggsave(filename = "Analysis/creat/bcreat_dur1.png", plot = bcreat_dur1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_dur2.png", plot = bcreat_dur2, width = 16, height = 9)

# looks at other associations (log VL DURATION)

bcreat_ldur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREATls,
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

bcreat_ldur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_CREATls,
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

ggsave(filename = "Analysis/creat/bcreat_ldur1.png", plot = bcreat_ldur1, width = 16, height = 9)
ggsave(filename = "Analysis/creat/bcreat_ldur2.png", plot = bcreat_ldur2, width = 16, height = 9)

# models ----

bcreat_uni1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_CREATls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_uni1)

AIC(bcreat_uni1)

bcreat_uni2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_CREATls + I(LB_BL_CREATls^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_uni2)
AIC(bcreat_uni2)

# initial cure creatinine is strongly associated with relapse risk
bcreat_uni3 <- glmer(
  OUT_DC_RELAPSE ~ LB_IC_CREATls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_uni3)
AIC(bcreat_uni3)

bcreat_uni4 <- glmer(
  OUT_DC_RELAPSE ~ LB_DIFF_CREAT + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_uni4)
AIC(bcreat_uni4)

bcreat_multi1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_CREATls + LB_IC_CREATls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_multi1)
AIC(bcreat_multi1)

bcreat_multi2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_CREATls + LB_DIFF_CREAT + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_multi2)
AIC(bcreat_multi2)

bcreat_multi3 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_CREATls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_multi3)
AIC(bcreat_multi3)

bcreat_multi4 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_CREATls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + MP_BL_SPLEEN_LENGTH + MB_COMBINED + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bcreat_multi4)
AIC(bcreat_multi4)


