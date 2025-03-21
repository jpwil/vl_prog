#######
# WBC #
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

# baseline wbc distribution
bwbc_dist1a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBC
    )
  ) +
  geom_histogram(
  )

bwbc_dist1b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBCls
    )
  ) +
  geom_histogram(
  )

bwbc_dist2a <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBC
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

bwbc_dist2b <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBCls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/wbc/bwbc_dist1a.png", plot = bwbc_dist1a, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_dist2a.png", plot = bwbc_dist2a, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_dist1b.png", plot = bwbc_dist1b, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_dist2b.png", plot = bwbc_dist2b, width = 16, height = 9)

# initial cure wbc distribution

iwbc_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_WBCls
    )
  ) +
  geom_histogram(
  )

iwbc_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_IC_WBCls
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/wbc/iwbc_dist1.png", plot = iwbc_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/iwbc_dist2.png", plot = iwbc_dist2, width = 16, height = 9)

# delta wbc distribution

dwbc_dist1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_WBC
    )
  ) +
  geom_histogram(
  )

dwbc_dist2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_DIFF_WBC
    )
  ) +
  geom_histogram(
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/wbc/dwbc_dist1.png", plot = dwbc_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/dwbc_dist2.png", plot = dwbc_dist2, width = 16, height = 9)

# baseline and initial cure correlation

bwbc_iwbc1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBC,
      y = LB_IC_WBC
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) 

bwbc_iwbc2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBC,
      y = LB_IC_WBC
    )
  ) + 
  geom_jitter(
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/wbc/bwbc_iwbc1.png", plot = bwbc_iwbc1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_iwbc2.png", plot = bwbc_iwbc2, width = 16, height = 9)

# baseline and diff correlation

bwbc_dwbc1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_WBC,
      y = LB_DIFF_WBC
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

bwbc_dwbc2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = LB_BL_WBC,
      y = LB_DIFF_WBC
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

ggsave(filename = "Analysis/wbc/bwbc_dwbc1.png", plot = bwbc_dwbc1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_dwbc2.png", plot = bwbc_dwbc2, width = 16, height = 9)

# associations with outcome

bwbc_out1 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBCls,
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

bwbc_out2 <- ads_model %>% 
  ggplot(
    aes(
      x = LB_BL_WBCls,
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

ggsave(filename = "Analysis/wbc/bwbc_out1.png", plot = bwbc_out1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_out2.png", plot = bwbc_out2, width = 16, height = 9)

# looks at other associations (age)

bwbc_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBCls,
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

bwbc_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBCls,
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

ggsave(filename = "Analysis/wbc/bwbc_age1.png", plot = bwbc_age1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_age2.png", plot = bwbc_age2, width = 16, height = 9)

# looks at other associations (spleen size)

bwbc_spleen1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBC,
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

bwbc_spleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBC,
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

ggsave(filename = "Analysis/wbc/bwbc_spleen1.png", plot = bwbc_spleen1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_spleen2.png", plot = bwbc_spleen2, width = 16, height = 9)

# looks at other associations (VL DURATION)

bwbc_dur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBCls,
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

bwbc_dur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBCls,
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

ggsave(filename = "Analysis/wbc/bwbc_dur1.png", plot = bwbc_dur1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_dur2.png", plot = bwbc_dur2, width = 16, height = 9)

# looks at other associations (log VL DURATION)

bwbc_ldur1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBCls,
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

bwbc_ldur2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = LB_BL_WBCls,
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

ggsave(filename = "Analysis/wbc/bwbc_ldur1.png", plot = bwbc_ldur1, width = 16, height = 9)
ggsave(filename = "Analysis/wbc/bwbc_ldur2.png", plot = bwbc_ldur2, width = 16, height = 9)

# models ----

bwbc_uni1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_WBCls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_uni1)
AIC(bwbc_uni1)

# interesting, there is a weakly significant quadratic association
bwbc_uni2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_WBCls + I(LB_BL_WBCls^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_uni2)
AIC(bwbc_uni2)

bwbc_uni3 <- glmer(
  OUT_DC_RELAPSE ~ LB_IC_WBC + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_uni3)
AIC(bwbc_uni3)

bwbc_uni4 <- glmer(
  OUT_DC_RELAPSE ~ LB_DIFF_WBC + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_uni4)
AIC(bwbc_uni4)

bwbc_multi1 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_WBCls + LB_IC_WBCls + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_multi1)
AIC(bwbc_multi1)

bwbc_multi2 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_WBCls + LB_DIFF_WBC + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_multi2)
AIC(bwbc_multi2)

bwbc_multi3 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_WBCls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_multi3)
AIC(bwbc_multi3)

bwbc_multi4 <- glmer(
  OUT_DC_RELAPSE ~ LB_BL_WBCls + DM_SEX + DM_AGEs + I(DM_AGEs^2) + MP_BL_SPLEEN_LENGTH + MB_COMBINED + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(bwbc_multi4)
AIC(bwbc_multi4)
