#######
# ALT #
#######

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
    LB_DIFF_WBC = (LB_IC_WBC - LB_BL_WBC)/IC_DAYS,
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
    LB_IC_CREATls = log(LB_IC_CREAT),
    LB_IC_BILIls = log(LB_IC_BILI),
    LB_IC_ALBs = (LB_IC_ALB - mean(LB_IC_ALB, na.rm = TRUE)),
    LB_IC_UREANls = log(LB_IC_UREAN),
    MP_DIFF_SPLEEN = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH
    )

# plots ----

# baseline alt distribution
balt_dist1a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALT
    )
  ) +
  geom_histogram(
  )

balt_dist1b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALTls
    )
  ) +
  geom_histogram(
  )

balt_dist2a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALT
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

balt_dist2b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALTls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/alt/balt_dist1a.png", plot = balt_dist1a, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_dist2a.png", plot = balt_dist2a, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_dist1b.png", plot = balt_dist1b, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_dist2b.png", plot = balt_dist2b, width = 16, height = 9)

# initial cure alt distribution

ialt_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_ALTls
    )
  ) +
  geom_histogram(
  )

ialt_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_ALTls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/alt/ialt_dist1.png", plot = ialt_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/ialt_dist2.png", plot = ialt_dist2, width = 16, height = 9)

# delta alt distribution

dalt_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_ALT
    )
  ) +
  geom_histogram(
  )

dalt_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_ALT
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/alt/dalt_dist1.png", plot = dalt_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/dalt_dist2.png", plot = dalt_dist2, width = 16, height = 9)

# baseline and initial cure correlation

balt_ialt1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALT,
      y = LB_IC_ALT
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) 

balt_ialt2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALT,
      y = LB_IC_ALT
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/alt/balt_ialt1.png", plot = balt_ialt1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_ialt2.png", plot = balt_ialt2, width = 16, height = 9)

# baseline and diff correlation

balt_dalt1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_ALT,
      y = LB_DIFF_ALT
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

balt_dalt2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_ALT,
      y = LB_DIFF_ALT
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

ggsave(filename = "Analysis/alt/balt_dalt1.png", plot = balt_dalt1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_dalt2.png", plot = balt_dalt2, width = 16, height = 9)

# associations with outcome

balt_out1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALTls,
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

balt_out2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_ALTls,
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

ggsave(filename = "Analysis/alt/balt_out1.png", plot = balt_out1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_out2.png", plot = balt_out2, width = 16, height = 9)

# looks at other associations (age)

balt_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALTls,
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

balt_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALTls,
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

ggsave(filename = "Analysis/alt/balt_age1.png", plot = balt_age1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_age2.png", plot = balt_age2, width = 16, height = 9)

# looks at other associations (spleen size)

balt_spleen1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALT,
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

balt_spleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALT,
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

ggsave(filename = "Analysis/alt/balt_spleen1.png", plot = balt_spleen1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_spleen2.png", plot = balt_spleen2, width = 16, height = 9)

# looks at other associations (VL DURATION)

balt_dur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALTls,
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

balt_dur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALTls,
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

ggsave(filename = "Analysis/alt/balt_dur1.png", plot = balt_dur1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_dur2.png", plot = balt_dur2, width = 16, height = 9)

# looks at other associations (log VL DURATION)

balt_ldur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALTls,
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

balt_ldur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_ALTls,
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

ggsave(filename = "Analysis/alt/balt_ldur1.png", plot = balt_ldur1, width = 16, height = 9)
ggsave(filename = "Analysis/alt/balt_ldur2.png", plot = balt_ldur2, width = 16, height = 9)

# models ----

balt_uni1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_ALTls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_uni1)
AIC(balt_uni1)

# interesting, there is a weakly significant quadratic association
balt_uni2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_ALTls + I(LB_BL_ALTls^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_uni2)
AIC(balt_uni2)

balt_uni3 <- glmer(
  OUT_DC_RELAPSE ~ LB_IC_ALTls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_uni3)
AIC(balt_uni3)

balt_uni4 <- glmer(
  OUT_DC_RELAPSE ~ LB_DIFF_ALT + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_uni4)
AIC(balt_uni4)

balt_multi1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_ALTls + LB_IC_ALTls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_multi1)
AIC(balt_multi1)

balt_multi2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_ALTls + LB_DIFF_ALT + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_multi2)
AIC(balt_multi2)

balt_multi3 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_ALTls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_multi3)
AIC(balt_multi3)

balt_multi4 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_ALTls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + MP_BL_SPLEEN_LENGTH + MB_COMBINED + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(balt_multi4)
AIC(balt_multi4)


