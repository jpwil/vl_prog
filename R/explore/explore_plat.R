#############
# PLATELETS #
#############

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
    LB_IC_ALTls = log(LB_IC_ALT),
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

# baseline plat distribution
bplat_dist1a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLAT
    )
  ) +
  geom_histogram(
  )

bplat_dist1b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLATls
    )
  ) +
  geom_histogram(
  )

bplat_dist2a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLAT
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

bplat_dist2b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLATls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/plat/bplat_dist1a.png", plot = bplat_dist1a, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_dist2a.png", plot = bplat_dist2a, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_dist1b.png", plot = bplat_dist1b, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_dist2b.png", plot = bplat_dist2b, width = 16, height = 9)

# initial cure plat distribution

iplat_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_PLATls
    )
  ) +
  geom_histogram(
  )

iplat_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_PLATls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/plat/iplat_dist1.png", plot = iplat_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/iplat_dist2.png", plot = iplat_dist2, width = 16, height = 9)

# delta plat distribution

dplat_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_PLAT
    )
  ) +
  geom_histogram(
  )

dplat_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_PLAT
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/plat/dplat_dist1.png", plot = dplat_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/dplat_dist2.png", plot = dplat_dist2, width = 16, height = 9)

# baseline and initial cure correlation

bplat_iplat1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLAT,
      y = LB_IC_PLAT
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bplat_iplat2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLAT,
      y = LB_IC_PLAT
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/plat/bplat_iplat1.png", plot = bplat_iplat1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_iplat2.png", plot = bplat_iplat2, width = 16, height = 9)

# baseline and diff correlation

bplat_dplat1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_PLAT,
      y = LB_DIFF_PLAT
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

bplat_dplat2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_PLAT,
      y = LB_DIFF_PLAT
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

ggsave(filename = "Analysis/plat/bplat_dplat1.png", plot = bplat_dplat1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_dplat2.png", plot = bplat_dplat2, width = 16, height = 9)

# associations with outcome

bplat_out1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLATls,
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

bplat_out2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_PLATls,
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

ggsave(filename = "Analysis/plat/bplat_out1.png", plot = bplat_out1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_out2.png", plot = bplat_out2, width = 16, height = 9)

# looks at other associations (age)

bplat_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLATls,
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

bplat_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLATls,
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

ggsave(filename = "Analysis/plat/bplat_age1.png", plot = bplat_age1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_age2.png", plot = bplat_age2, width = 16, height = 9)

# looks at other associations (spleen size)

bplat_spleen1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLAT,
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

bplat_spleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLAT,
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

ggsave(filename = "Analysis/plat/bplat_spleen1.png", plot = bplat_spleen1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_spleen2.png", plot = bplat_spleen2, width = 16, height = 9)

# looks at other associations (VL DURATION)

bplat_dur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLATls,
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

bplat_dur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLATls,
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

ggsave(filename = "Analysis/plat/bplat_dur1.png", plot = bplat_dur1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_dur2.png", plot = bplat_dur2, width = 16, height = 9)

# looks at other associations (log VL DURATION)

bplat_ldur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLATls,
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

bplat_ldur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_PLATls,
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

ggsave(filename = "Analysis/plat/bplat_ldur1.png", plot = bplat_ldur1, width = 16, height = 9)
ggsave(filename = "Analysis/plat/bplat_ldur2.png", plot = bplat_ldur2, width = 16, height = 9)

# models ----

bplat_uni1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_PLATls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_uni1)
AIC(bplat_uni1)

# interesting, there is a weakly significant quadratic association
bplat_uni2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_PLATls + I(LB_BL_PLATls^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_uni2)
AIC(bplat_uni2)

bplat_uni3 <- glmer(
  OUT_DC_RELAPSE ~ LB_IC_PLATls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_uni3)
AIC(bplat_uni3)

bplat_uni4 <- glmer(
  OUT_DC_RELAPSE ~ LB_DIFF_PLAT + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_uni4)
AIC(bplat_uni4)

bplat_multi1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_PLATls + LB_IC_PLATls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_multi1)
AIC(bplat_multi1)

bplat_multi2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_PLATls + LB_DIFF_PLAT + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_multi2)
AIC(bplat_multi2)

bplat_multi3 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_PLATls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_multi3)
AIC(bplat_multi3)

bplat_multi4 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_PLATls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + MP_BL_SPLEEN_LENGTH + MB_COMBINED + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bplat_multi4)
AIC(bplat_multi4)

