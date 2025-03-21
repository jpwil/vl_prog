#########
## AGE ##
#########

rm(list = ls())
library(tidyverse)
library(lme4)
load("Analysis/ads_clean.RData")
load("Analysis/ads_dirty.RData")
source("definitions.R")

ads_model <- ads_clean %>% 
  mutate(
    DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10
  )

# no missing data
# range: 1 - 80 years
# mean age: 22.5 years
# median age: 18 years

ads_clean %>% var_sum(DM_AGE)
ads_clean %>% var_sum(DM_AGE, STUDYID)

# plots ----

age_dist1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DM_AGE
    )
  )

age_dist1.1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DM_AGE,
      #y = after_stat(ncount)
    ),
  ) + 
  facet_wrap(~STUDYID)

age_dist2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DM_AGE,
      y = after_stat(ndensity),
      fill = OUT_DC_RELAPSE
    ),
    alpha = 0.5,
    position = "identity"
  )

age_dist2.1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DM_AGE,
      y = after_stat(ndensity),
      fill = OUT_DC_RELAPSE
    ),
    alpha = 0.5,
    position = "identity"
  ) + 
  facet_wrap(~STUDYID)

age_dist2.2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = DM_AGE,
      #y = after_stat(ndensity),
      fill = OUT_DC_RELAPSE
    ),
    alpha = 0.5,
    position = "identity"
  ) + 
  facet_wrap(~STUDYID)

age_uni1.1 <- ads_model %>% 
  ggplot(
    aes(
      x = DM_AGE,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) +
  geom_jitter(
    height = 0.02,
    width = 0.5
  ) + 
  geom_smooth(
    method = "loess", 
    #method.args = list(family = "binomial"), 
    se = TRUE, 
    color = "red"
  ) +
  coord_fixed(
    ratio = 60 / 2
  ) + 
  scale_y_continuous(
    breaks = seq(0, 1, 0.2)
  ) + 
  labs(caption = "Loess smoother")

age_uni1.2 <- ads_model %>% 
  ggplot(
    aes(
      x = DM_AGE,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) +
  geom_jitter(
    height = 0.02,
    width = 0.5
  ) + 
  geom_smooth(
    method = "loess", 
    #method.args = list(family = "binomial"), 
    se = TRUE, 
    color = "red"
  ) +
  coord_cartesian(
    ylim = c(0, 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.1, 0.01)
  ) +
  labs(caption = "Loess smoother")

age_uni2.1 <- ads_model %>% 
  ggplot(
    aes(
      x = DM_AGE,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) +
  geom_jitter(
    height = 0.02,
    width = 0.5
  ) + 
  geom_smooth(
    method = "glm", 
    method.args = list(family = "binomial"), 
    se = TRUE, 
    color = "red"
  ) +
  coord_fixed(
    ratio = 60 / 2
  ) + 
  scale_y_continuous(
    breaks = seq(0, 1, 0.2)
  ) + 
  labs(caption = "Logit fit")

age_uni2.2 <- ads_model %>% 
  ggplot(
    aes(
      x = DM_AGE,
      y = as.numeric(OUT_DC_RELAPSE)
    )
  ) +
  geom_jitter(
    height = 0.02,
    width = 0.5
  ) + 
  geom_smooth(
    method = "glm", 
    method.args = list(family = "binomial"), 
    se = TRUE, 
    color = "red"
  ) +
  coord_cartesian(
    ylim = c(0, 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.1, 0.01)
  ) +
  labs(caption = "Logit fit")

ggsave(filename = "Analysis/age/age_uni11.png", plot = age_uni1.1, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_uni12.png", plot = age_uni1.2, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_uni21.png", plot = age_uni2.1, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_uni22.png", plot = age_uni2.2, width = 16, height = 9)
#
ggsave(filename = "Analysis/age/age_dist1.png", plot = age_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_dist11.png", plot = age_dist1.1, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_dist2.png", plot = age_dist2, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_dist21.png", plot = age_dist2.1, width = 16, height = 9)
ggsave(filename = "Analysis/age/age_dist22.png", plot = age_dist2.2, width = 16, height = 9)

# models ----

# fixed effects
model_age_f1 <- glm(
    OUT_DC_RELAPSE ~ DM_AGEs,
    family = binomial(),
    data = ads_model
    )
summary(model_age_f1)
model_age_f1$aic

model_age_f2 <- glm(
    OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2),
    family = binomial(),
    data = ads_model
    )
summary(model_age_f2)
AIC(model_age_f2)

model_age_f3 <- glm(
    OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + I(DM_AGEs^3),
    family = binomial(),
    data = ads_model
    )
summary(model_age_f3)
AIC(model_age_f3)

# random effects
model_age_r1 <- glmer(
    OUT_DC_RELAPSE ~ DM_AGEs + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_age_r1)
AIC(model_age_r1)

model_age_r2 <- glmer(
    OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_age_r2)
AIC(model_age_r2)

model_age_r3 <- glmer(
    OUT_DC_RELAPSE ~ DM_AGEs + I(DM_AGEs^2) + I(DM_AGEs^3) + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_age_r3)
AIC(model_age_r3)