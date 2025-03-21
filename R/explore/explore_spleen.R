###############
# SPLEEN SIZE #
###############

rm(list = ls())
library(tidyverse)
library(lme4)
load("Analysis/ads_clean.RData")
load("Analysis/ads_dirty.RData")

source("definitions.R")

ads_model <- ads_clean %>% 
  mutate(
    LIVER_DIFF = (MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) / IC_DAYS,
    SPLEEN_DIFF = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS,
    DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10,
    MB_relevel = relevel(factor(MB_COMBINED), ref = "1"),
    OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE),
    WEIGHT_DIFF = (VS_IC_WEIGHT - VS_BL_WEIGHT)/IC_DAYS,
    VS_BL_WEIGHTs = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
    BMI = VS_BL_WEIGHT / (VS_BL_HEIGHT/100)^2,
    BMIs = (BMI - mean(BMI, na.rm = TRUE)) / 10
  )

# ads_model <- ads_dirty %>% 
#   mutate(
#     LIVER_DIFF = (MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) / IC_DAYS,
#     SPLEEN_DIFF = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS,
#     DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10,
#     OUT_DC_RELAPSE = as.numeric(OUT_DC_RELAPSE),
#     WEIGHT_DIFF = (VS_IC_WEIGHT - VS_BL_WEIGHT)/IC_DAYS,
#     VS_BL_WEIGHTs = (VS_BL_WEIGHT - mean(VS_BL_WEIGHT, na.rm = TRUE)) / 10,
#     BMI = VS_BL_WEIGHT / (VS_BL_HEIGHT/100)^2,
#     BMIs = (BMI - mean(BMI, na.rm = TRUE)) / 10
#   )

# BASELINE #

# overall distribution varies from 0cm to 22cm
# missing 17.3%
# mean: 4.67cm
# median: 4cm 

ads_model %>% var_sum(MP_BL_SPLEEN_LENGTH)
ads_model %>% var_sum(MP_BL_SPLEEN_LENGTH, STUDYID)
ads_model %>% var_sum(SPLEEN_DIFF, STUDYID)

# plots ---

# baseline distributions
bspleen_dist1 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_BL_SPLEEN_LENGTH),
    binwidth = 0.1
  )

bspleen_dist2 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_BL_SPLEEN_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/spleen/bspleen_dist1.png", plot = bspleen_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_dist2.png", plot = bspleen_dist2, width = 16, height = 9)

# initial cure distributions
ispleen_dist1 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_SPLEEN_LENGTH),
    binwidth = 0.1
  )

ispleen_dist2 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_SPLEEN_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/spleen/ispleen_dist1.png", plot = ispleen_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/ispleen_dist2.png", plot = ispleen_dist2, width = 16, height = 9)

# difference distributions
dspleen_dist1 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH),
    binwidth = 0.1
  )

dspleen_dist2 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/spleen/dspleen_dist1.png", plot = dspleen_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/dspleen_dist2.png", plot = dspleen_dist2, width = 16, height = 9)

# bspleen - ispleen associations
bspleen_ispleen1 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = MP_IC_SPLEEN_LENGTH
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.1,
    alpha = 0.1
  ) + 
  geom_smooth(
    method = "loess"
  )

bspleen_ispleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = MP_IC_SPLEEN_LENGTH,
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )

bspleen_ispleen3 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = MP_IC_SPLEEN_LENGTH
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.1
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

bspleen_ispleen4 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = MP_IC_SPLEEN_LENGTH,
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~ STUDYID)

ggsave(filename = "Analysis/spleen/bspleen_ispleen1.png", plot = bspleen_ispleen1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_ispleen2.png", plot = bspleen_ispleen2, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_ispleen3.png", plot = bspleen_ispleen3, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_ispleen4.png", plot = bspleen_ispleen4, width = 16, height = 9)

# dspleen - bspleen associations
dspleen_ispleen1 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = SPLEEN_DIFF
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.1,
    alpha = 0.1
  ) + 
  geom_smooth(
    method = "loess"
  )

dspleen_ispleen2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = SPLEEN_DIFF,
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )

dspleen_ispleen3 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = SPLEEN_DIFF
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.1
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

dspleen_ispleen4 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_SPLEEN_LENGTH,
      y = SPLEEN_DIFF,
      colour = factor(OUT_DC_RELAPSE)
    )
  ) + 
  geom_jitter(
    width = 0.2,
    height = 0.2,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~ STUDYID)

ggsave(filename = "Analysis/spleen/dspleen_ispleen1.png", plot = dspleen_ispleen1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/dspleen_ispleen2.png", plot = dspleen_ispleen2, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/dspleen_ispleen3.png", plot = dspleen_ispleen3, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/dspleen_ispleen4.png", plot = dspleen_ispleen4, width = 16, height = 9)

# baseline spleen outcome associations
bspleen_out1a <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_SPLEEN_LENGTH, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.2, height = 0.2, alpha = 0.4
  ) +
    geom_smooth(
      method = "glm", 
      method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "glm/binomial fit") + 
  coord_cartesian(ylim = c(0, 0.2))

bspleen_out1b <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_SPLEEN_LENGTH, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.2, height = 0.2, alpha = 0.4
  ) +
    geom_smooth(
      method = "loess", 
      #method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "loess") + 
  coord_cartesian(ylim = c(0, 0.2))

bspleen_out2a <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_SPLEEN_LENGTH, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.2, height = 0.2, alpha = 0.4
  ) +
    geom_smooth(
      method = "glm", 
      method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "glm/binomial fit") + 
  coord_cartesian(ylim = c(0, 0.2)) + 
  facet_wrap(~STUDYID)

bspleen_out2b <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_SPLEEN_LENGTH, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.2, height = 0.2, alpha = 0.4
  ) +
    geom_smooth(
      method = "loess", 
      #method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "loess") + 
  coord_cartesian(ylim = c(0, 0.2)) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/spleen/bspleen_out1a.png", plot = bspleen_out1a, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_out1b.png", plot = bspleen_out1b, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_out2a.png", plot = bspleen_out2a, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_out2b.png", plot = bspleen_out2b, width = 16, height = 9)

# initial cure spleen and outcome associations

ispleen_out1 <- ads_model %>% 
  ggplot(
    aes(x = MP_IC_SPLEEN_LENGTH, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.2, height = 0.2, alpha = 0.4
  ) +
    geom_smooth(
      method = "glm", 
      method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "glm/binomial fit") + 
  coord_cartesian(ylim = c(0, 0.2))

ispleen_out2 <- ads_model %>% 
  ggplot(
    aes(x = MP_IC_SPLEEN_LENGTH, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.2, height = 0.2, alpha = 0.4
  ) +
    geom_smooth(
      method = "loess", 
      #method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "loess") + 
  coord_cartesian(ylim = c(0, 0.2))

ggsave(filename = "Analysis/spleen/ispleen_out1.png", plot = ispleen_out1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/ispleen_out2.png", plot = ispleen_out2, width = 16, height = 9)

# delta spleen and outcome associations

dspleen_out1 <- ads_model %>% 
  ggplot(
    aes(x = SPLEEN_DIFF, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.02, height = 0.02, alpha = 0.4
  ) +
    geom_smooth(
      method = "glm", 
      method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "glm/binomial fit") + 
  coord_cartesian(ylim = c(0, 0.2))

dspleen_out2 <- ads_model %>% 
  ggplot(
    aes(x = SPLEEN_DIFF, y = OUT_DC_RELAPSE)
  ) + 
  geom_jitter(
    width = 0.02, height = 0.02, alpha = 0.4
  ) +
    geom_smooth(
      method = "loess", 
      #method.args = list(family = "binomial"), 
      se = TRUE, 
      color = "red"
  ) + 
  labs(caption = "loess") + 
  coord_cartesian(ylim = c(0, 0.2))

ggsave(filename = "Analysis/spleen/dspleen_out1.png", plot = dspleen_out1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/dspleen_out2.png", plot = dspleen_out2, width = 16, height = 9)

# other associations

bspleen_age1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    width = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )

bspleen_age2 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    width = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~ STUDYID) + 
  coord_cartesian(ylim = c(0, 25))

bspleen_age_weight1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH / VS_BL_WEIGHT,
      x = DM_AGE
    )
  ) + 
  geom_jitter(
    width = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )
ggsave(filename = "Analysis/spleen/bspleen_age1.png", plot = bspleen_age1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_age2.png", plot = bspleen_age2, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_age_weight1.png", plot = bspleen_age_weight1, width = 16, height = 9)


dspleen_age1 <- ads_model %>% 
  ggplot(
    aes(
      y = SPLEEN_DIFF,
      x = DM_AGE
  ) + 
  geom_jitter(
    width = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )
ggsave(filename = "Analysis/spleen/dspleen_age1.png", plot = dspleen_age1, width = 16, height = 9)


bspleen_para1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = MB_COMBINED
    )
  ) + 
  geom_jitter(
    width = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )
ggsave(filename = "Analysis/spleen/bspleen_para1.png", plot = bspleen_para1, width = 16, height = 9)

bspleen_para2 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = factor(MB_COMBINED),
      fill = factor(MB_COMBINED)
    )
  ) + 
  geom_violin()

ggsave(filename = "Analysis/spleen/bspleen_para2.png", plot = bspleen_para2, width = 16, height = 9)

bspleen_weight1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = VS_BL_WEIGHT
    )
  ) +
  geom_jitter(
    height = 0.1,
    width = 0.1
  ) +
  geom_smooth(
    method = "loess"
  )

bspleen_weight2 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = VS_BL_WEIGHT
    )
  ) +
  geom_jitter(
    height = 0.1,
    width = 0.1
  ) +
  geom_smooth(
    method = "loess"
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/spleen/bspleen_weight1.png", plot = bspleen_weight1, width = 16, height = 9)
ggsave(filename = "Analysis/spleen/bspleen_weight2.png", plot = bspleen_weight2, width = 16, height = 9)


# spleen and liver associations
bspleen_bliver <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_SPLEEN_LENGTH,
      x = MP_BL_LIVER_LENGTH
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )
ggsave(filename = "Analysis/spleen/bspleen_bliver.png", plot = bspleen_bliver, width = 16, height = 9)

dspleen_dliver <- ads_model %>% 
  ggplot(
    aes(
      y = SPLEEN_DIFF,
      x = LIVER_DIFF
    )
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0.1,
    alpha = 0.4
  ) + 
  geom_smooth(
    method = "loess"
  )
ggsave(filename = "Analysis/spleen/dspleen_dliver.png", plot = dspleen_dliver, width = 16, height = 9)

# models ----

# fixed effects

model_uni1 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_SPLEEN_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni1)
AIC(model_uni1)

model_uni2 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_SPLEEN_LENGTH + I(MP_BL_SPLEEN_LENGTH^2) + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni2)
AIC(model_uni2)

model_uni3 <- glmer(
    OUT_DC_RELAPSE ~ MP_IC_SPLEEN_LENGTH  + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni3)
AIC(model_uni3)

# magnitude of decrease in spleen size is significantly associated with reduced risk of relapse  
model_uni4 <- glmer( 
    OUT_DC_RELAPSE ~ I(-SPLEEN_DIFF) + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni4)
AIC(model_uni4)

model_multi1 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_SPLEEN_LENGTH + MP_IC_SPLEEN_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi1)
AIC(model_multi1)

model_multi2 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_SPLEEN_LENGTH + SPLEEN_DIFF + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi2)
AIC(model_multi2)

model_multi3 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_SPLEEN_LENGTH + MP_BL_LIVER_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi3)
AIC(model_multi3)

model_multi4 <- glmer(
    OUT_DC_RELAPSE ~ SPLEEN_DIFF + LIVER_DIFF + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi4)
AIC(model_multi4)
