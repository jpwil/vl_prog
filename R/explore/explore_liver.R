##############
# LIVER SIZE #
##############

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

ads_model %>% var_sum(MP_BL_LIVER_LENGTH)
ads_model %>% var_sum(MP_BL_LIVER_LENGTH, STUDYID)
ads_model %>% var_sum(LIVER_DIFF, STUDYID)

# plots ---

# baseline distributions
bliver_dist1 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_BL_LIVER_LENGTH),
    binwidth = 0.1
  )

bliver_dist2 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_BL_LIVER_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/liver/bliver_dist1.png", plot = bliver_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_dist2.png", plot = bliver_dist2, width = 16, height = 9)

# initial cure distributions
iliver_dist1 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_LIVER_LENGTH),
    binwidth = 0.1
  )

iliver_dist2 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_LIVER_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/liver/iliver_dist1.png", plot = iliver_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/iliver_dist2.png", plot = iliver_dist2, width = 16, height = 9)

# difference distributions
dliver_dist1 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH),
    binwidth = 0.1
  )

dliver_dist2 <- ads_model %>% 
  ggplot() +
  geom_histogram(
    aes(x = MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH),
    binwidth = 0.1
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/liver/dliver_dist1.png", plot = dliver_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/dliver_dist2.png", plot = dliver_dist2, width = 16, height = 9)

# bliver - iliver associations
bliver_iliver1 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = MP_IC_LIVER_LENGTH
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

bliver_iliver2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = MP_IC_LIVER_LENGTH,
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

bliver_iliver3 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = MP_IC_LIVER_LENGTH
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

bliver_iliver4 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = MP_IC_LIVER_LENGTH,
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

ggsave(filename = "Analysis/liver/bliver_iliver1.png", plot = bliver_iliver1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_iliver2.png", plot = bliver_iliver2, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_iliver3.png", plot = bliver_iliver3, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_iliver4.png", plot = bliver_iliver4, width = 16, height = 9)

# dliver - bliver associations
dliver_iliver1 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = LIVER_DIFF
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

dliver_iliver2 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = LIVER_DIFF,
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

dliver_iliver3 <- ads_model %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = LIVER_DIFF
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

dliver_iliver4 <- ads_model %>% 
  arrange(OUT_DC_RELAPSE) %>% 
  ggplot(
    aes(
      x = MP_BL_LIVER_LENGTH,
      y = LIVER_DIFF,
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

ggsave(filename = "Analysis/liver/dliver_iliver1.png", plot = dliver_iliver1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/dliver_iliver2.png", plot = dliver_iliver2, width = 16, height = 9)
ggsave(filename = "Analysis/liver/dliver_iliver3.png", plot = dliver_iliver3, width = 16, height = 9)
ggsave(filename = "Analysis/liver/dliver_iliver4.png", plot = dliver_iliver4, width = 16, height = 9)

# baseline liver outcome associations
bliver_out1a <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_LIVER_LENGTH, y = OUT_DC_RELAPSE)
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

bliver_out1b <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_LIVER_LENGTH, y = OUT_DC_RELAPSE)
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

bliver_out2a <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_LIVER_LENGTH, y = OUT_DC_RELAPSE)
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

bliver_out2b <- ads_model %>% 
  ggplot(
    aes(x = MP_BL_LIVER_LENGTH, y = OUT_DC_RELAPSE)
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

ggsave(filename = "Analysis/liver/bliver_out1a.png", plot = bliver_out1a, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_out1b.png", plot = bliver_out1b, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_out2a.png", plot = bliver_out2a, width = 16, height = 9)
ggsave(filename = "Analysis/liver/bliver_out2b.png", plot = bliver_out2b, width = 16, height = 9)

# initial cure liver and outcome associations

iliver_out1 <- ads_model %>% 
  ggplot(
    aes(x = MP_IC_LIVER_LENGTH, y = OUT_DC_RELAPSE)
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

iliver_out2 <- ads_model %>% 
  ggplot(
    aes(x = MP_IC_LIVER_LENGTH, y = OUT_DC_RELAPSE)
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

ggsave(filename = "Analysis/liver/iliver_out1.png", plot = iliver_out1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/iliver_out2.png", plot = iliver_out2, width = 16, height = 9)

# delta liver and outcome associations

dliver_out1 <- ads_model %>% 
  ggplot(
    aes(x = LIVER_DIFF, y = OUT_DC_RELAPSE)
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

dliver_out2 <- ads_model %>% 
  ggplot(
    aes(x = LIVER_DIFF, y = OUT_DC_RELAPSE)
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

ggsave(filename = "Analysis/liver/dliver_out1.png", plot = dliver_out1, width = 16, height = 9)
ggsave(filename = "Analysis/liver/dliver_out2.png", plot = dliver_out2, width = 16, height = 9)

# other associations

bliver_age1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_LIVER_LENGTH,
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

dliver_age1 <- ads_model %>% 
  ggplot(
    aes(
      y = LIVER_DIFF,
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
ggsave(filename = "Analysis/liver/dliver_age1.png", plot = dliver_age1, width = 16, height = 9)

bliver_para1 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_LIVER_LENGTH,
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
ggsave(filename = "Analysis/liver/bliver_para1.png", plot = bliver_para1, width = 16, height = 9)

bliver_para2 <- ads_model %>% 
  ggplot(
    aes(
      y = MP_BL_LIVER_LENGTH,
      x = factor(MB_COMBINED),
      fill = factor(MB_COMBINED)
    )
  ) + 
  geom_violin()

ggsave(filename = "Analysis/liver/bliver_para2.png", plot = bliver_para2, width = 16, height = 9)

# models ----

# fixed effects

model_uni1 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_LIVER_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni1)
AIC(model_uni1)

model_uni2 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_LIVER_LENGTH + I(MP_BL_LIVER_LENGTH^2) + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni2)
AIC(model_uni2)

model_uni3 <- glmer(
    OUT_DC_RELAPSE ~ MP_IC_LIVER_LENGTH  + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni3)
AIC(model_uni3)

model_uni4 <- glmer( 
    OUT_DC_RELAPSE ~ I(-LIVER_DIFF) + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_uni4)
AIC(model_uni4)

model_multi1 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_LIVER_LENGTH + MP_IC_LIVER_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi1)
AIC(model_multi1)

model_multi2 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_LIVER_LENGTH + LIVER_DIFF + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi2)
AIC(model_multi2)

model_multi3 <- glmer(
    OUT_DC_RELAPSE ~ MP_BL_LIVER_LENGTH + MP_BL_LIVER_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi3)
AIC(model_multi3)

model_multi4 <- glmer(
    OUT_DC_RELAPSE ~ LIVER_DIFF + LIVER_DIFF + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(model_multi4)
AIC(model_multi4)