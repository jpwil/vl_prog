###################
# HEIGHT & WEIGHT #
###################

rm(list = ls())
library(tidyverse)
library(lme4)
load("Analysis/ads_clean.RData")
load("Analysis/ads_dirty.RData")
source("definitions.R")

ads_model <- ads_clean %>% 
  mutate(
    LIVER_DIFF      = (MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) / IC_DAYS,
    SPLEEN_DIFF     = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS,
    DM_AGEs         = (DM_AGE - mean(DM_AGE)) / 10,
    MB_relevel      = relevel(factor(MB_COMBINED), ref = "1"),
    OUT_DC_RELAPSE  = as.numeric(OUT_DC_RELAPSE),
    WEIGHT_DIFF     = (VS_IC_WEIGHT - VS_BL_WEIGHT)/IC_DAYS,
    VS_BL_WEIGHTs   = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
    BMI             = VS_BL_WEIGHT / (VS_BL_HEIGHT / 100)^2,
    BMIs            = (BMI - mean(BMI, na.rm = TRUE)) / 10
  )

# ads_model <- ads_dirty %>% 
#   mutate(
#     WEIGHT_DIFF = (VS_IC_WEIGHT - VS_BL_WEIGHT) / IC_DAYS,
#     OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE)
#   )

ads_model %>% var_sum(VS_BL_WEIGHT)
ads_model %>% var_sum(VS_BL_WEIGHT, STUDYID)

ads_model %>% var_sum(VS_IC_WEIGHT)
ads_model %>% var_sum(VS_IC_WEIGHT, STUDYID)

ads_model %>% var_sum(WEIGHT_DIFF)
ads_model %>% var_sum(WEIGHT_DIFF, STUDYID) #%>% View()

# plots

# boxplots

height_boxplot1 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      y = VS_BL_HEIGHT
    )
  )

height_boxplot2 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      x = STUDYID,
      y = VS_BL_HEIGHT
    )
  ) 

bweight_boxplot1 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      y = VS_BL_WEIGHT
    )
  )

bweight_boxplot2 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      x = STUDYID,
      y = VS_BL_WEIGHT
    )
  )

iweight_boxplot1 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      y = VS_IC_WEIGHT
    )
  )

iweight_boxplot2 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      x = STUDYID,
      y = VS_IC_WEIGHT
    )
  )

dweight_boxplot1 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      y = WEIGHT_DIFF
    )
  )

dweight_boxplot2 <- ads_model %>% 
  ggplot() + 
  geom_boxplot(
    aes(
      x = STUDYID,
      y = WEIGHT_DIFF
    )
  )

ggsave(filename = "Analysis/anthro/height_boxplot1.png", plot = height_boxplot1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/height_boxplot2.png", plot = height_boxplot2, width = 16, height = 9)

ggsave(filename = "Analysis/anthro/bweight_boxplot1.png", plot = bweight_boxplot1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/iweight_boxplot1.png", plot = iweight_boxplot1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/dweight_boxplot1.png", plot = dweight_boxplot1, width = 16, height = 9)


ggsave(filename = "Analysis/anthro/bweight_boxplot2.png", plot = bweight_boxplot2, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/iweight_boxplot2.png", plot = iweight_boxplot2, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/dweight_boxplot2.png", plot = dweight_boxplot2, width = 16, height = 9)

# baseline height distributions
height_dist1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_BL_HEIGHT
    )
  )

height_dist2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_BL_HEIGHT
    )
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/height_dist1.png", plot = height_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/height_dist2.png", plot = height_dist2, width = 16, height = 9)


# baseline weight distributions
bweight_dist1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_BL_WEIGHT
    )
  )

bweight_dist2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_BL_WEIGHT
    )
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/bweight_dist1.png", plot = bweight_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/bweight_dist2.png", plot = bweight_dist2, width = 16, height = 9)

# initial cure weight distributions
iweight_dist1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_IC_WEIGHT
    )
  )

iweight_dist2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = VS_IC_WEIGHT
    )
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/iweight_dist1.png", plot = iweight_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/iweight_dist2.png", plot = iweight_dist2, width = 16, height = 9)

# delta weight distributions
dweight_dist1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = WEIGHT_DIFF
    )
  )

dweight_dist2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = WEIGHT_DIFF
    )
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/dweight_dist1.png", plot = dweight_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/dweight_dist2.png", plot = dweight_dist2, width = 16, height = 9)

# associations (weight - weight)
bweight_iweight1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_BL_WEIGHT,
      y = VS_IC_WEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

bweight_iweight2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_BL_WEIGHT,
      y = VS_IC_WEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/bweight_iweight1.png", plot = bweight_iweight1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/bweight_iweight2.png", plot = bweight_iweight2, width = 16, height = 9)

# associations (weight - delta weight)
bweight_dweight1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_BL_WEIGHT,
      y = WEIGHT_DIFF
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

bweight_dweight2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_BL_WEIGHT,
      y = WEIGHT_DIFF
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/bweight_dweight1.png", plot = bweight_dweight1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/bweight_dweight2.png", plot = bweight_dweight2, width = 16, height = 9)

# associations (bl weight - age)
bweight_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_BL_WEIGHT,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

bweight_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_BL_WEIGHT,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/bweight_age1.png", plot = bweight_age1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/bweight_age2.png", plot = bweight_age2, width = 16, height = 9)


# associations (ic weight - age)
iweight_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_IC_WEIGHT,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

iweight_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_IC_WEIGHT,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/iweight_age1.png", plot = iweight_age1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/iweight_age2.png", plot = iweight_age2, width = 16, height = 9)

# associations (d weight - age)
dweight_age1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = WEIGHT_DIFF,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

dweight_age2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = WEIGHT_DIFF,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/dweight_age1.png", plot = dweight_age1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/dweight_age2.png", plot = dweight_age2, width = 16, height = 9)

# associations (bl weight - height)
bweight_height1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_BL_WEIGHT,
      x = VS_BL_HEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

bweight_height2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_BL_WEIGHT,
      x = VS_BL_HEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/bweight_height1.png", plot = bweight_height1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/bweight_height2.png", plot = bweight_height2, width = 16, height = 9)

# associations (ic weight - height)
iweight_height1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_IC_WEIGHT,
      x = VS_BL_HEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

iweight_height2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = VS_IC_WEIGHT,
      x = VS_BL_HEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/anthro/iweight_height1.png", plot = iweight_height1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/iweight_height2.png", plot = iweight_height2, width = 16, height = 9)


# associations (d weight - height)
dweight_height1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = WEIGHT_DIFF,
      x = VS_BL_HEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
  )

dweight_height2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      y = WEIGHT_DIFF,
      x = VS_BL_HEIGHT
    )
  ) + 
  geom_jitter(
    aes(
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  coord_cartesian(ylim = c(-0.6, 0.25)) + 
  facet_wrap(~STUDYID) 

ggsave(filename = "Analysis/anthro/dweight_height1.png", plot = dweight_height1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/dweight_height2.png", plot = dweight_height2, width = 16, height = 9)

# associations (baseline weight and other)

# outcome (baseline weight)

bweight_out1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_BL_WEIGHT,
      y = OUT_DC_RELAPSE
    )
  ) + 
  geom_jitter(
    width = 0.02,
    height = 0.15
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
    )

bweight_out2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_BL_WEIGHT,
      y = OUT_DC_RELAPSE
    )
  ) + 
  geom_jitter(
    width = 0.02,
    height = 0.15
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
    ) +
  coord_cartesian(
    ylim = c(0, 0.2)
  )
  
ggsave(filename = "Analysis/anthro/bweight_out1.png", plot = bweight_out1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/bweight_out2.png", plot = bweight_out2, width = 16, height = 9)

# outcome (ic weight)

iweight_out1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_IC_WEIGHT,
      y = OUT_DC_RELAPSE
    )
  ) + 
  geom_jitter(
    width = 0.02,
    height = 0.15
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
    )

iweight_out2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = VS_IC_WEIGHT,
      y = OUT_DC_RELAPSE
    )
  ) + 
  geom_jitter(
    width = 0.02,
    height = 0.15
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
    ) +
  coord_cartesian(
    ylim = c(0, 0.2)
  )
  
ggsave(filename = "Analysis/anthro/iweight_out1.png", plot = iweight_out1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/iweight_out2.png", plot = iweight_out2, width = 16, height = 9)

# outcome (d weight)
dweight_out1 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = WEIGHT_DIFF,
      y = OUT_DC_RELAPSE
    )
  ) + 
  geom_jitter(
    width = 0.02,
    height = 0.15
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
    )

dweight_out2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = WEIGHT_DIFF,
      y = OUT_DC_RELAPSE
    )
  ) + 
  geom_jitter(
    width = 0.02,
    height = 0.15
  ) + 
  geom_smooth(
    method = "loess",
    se = TRUE
    ) +
  coord_cartesian(
    ylim = c(-0.2, 1.2)
  )
  
ggsave(filename = "Analysis/anthro/dweight_out1.png", plot = dweight_out1, width = 16, height = 9)
ggsave(filename = "Analysis/anthro/dweight_out2.png", plot = dweight_out2, width = 16, height = 9)

# models ----

model_uni1 <- glmer(
  OUT_DC_RELAPSE ~ VS_BL_WEIGHTs + (1 | STUDYID), 
  data = ads_model, 
  family = binomial
  )
summary(model_uni1)
AIC(model_uni1)

model_uni2 <- glmer(
  OUT_DC_RELAPSE ~ VS_BL_WEIGHTs + I(VS_BL_WEIGHTs^2) + (1 | STUDYID), 
  data = ads_model, 
  family = binomial
  )
summary(model_uni2)
AIC(model_uni2)

model_uni3 <- glmer(
  OUT_DC_RELAPSE ~ VS_BL_WEIGHTs + I(VS_BL_WEIGHTs^2) + I(VS_BL_WEIGHTs^3) + (1 | STUDYID), 
  data = ads_model, 
  family = binomial
  )
summary(model_uni3)
AIC(model_uni3)

model_uni4 <- glmer(
  OUT_DC_RELAPSE ~ WEIGHT_DIFF + (1 | STUDYID), 
  data = ads_model, 
  family = binomial
  )
summary(model_uni4)
AIC(model_uni4)

model_uni5 <- glmer(
  OUT_DC_RELAPSE ~ VS_BL_HEIGHT + (1 | STUDYID), 
  data = ads_model, 
  family = binomial
  )
summary(model_uni5)
AIC(model_uni5)

model_uni6 <- glmer(
  OUT_DC_RELAPSE ~ BMIs + (1 | STUDYID), 
  data = ads_model, 
  family = binomial
  )
summary(model_uni6)
AIC(model_uni6)

model_multi1 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(model_multi1)
AIC(model_multi1)

model_multi2 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + VS_BL_WEIGHTs + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(model_multi2)
AIC(model_multi2)

model_multi3 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + WEIGHT_DIFF + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(model_multi3)
AIC(model_multi3)

model_multi4 <- glmer(
  OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + BMIs + (1 | STUDYID),
  data = ads_model,
  family = binomial
)
summary(model_multi4)
AIC(model_multi4)

