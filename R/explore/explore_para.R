##################
## PARASITAEMIA ##
##################

rm(list = ls())
library(tidyverse)
library(lme4)
library(micemd)
load("Analysis/ads_clean.RData")
source("definitions.R")

ads_model <- ads_clean %>% 
  mutate(
    LIVER_DIFF = (MP_IC_LIVER_LENGTH - MP_BL_LIVER_LENGTH) / IC_DAYS,
    SPLEEN_DIFF = (MP_IC_SPLEEN_LENGTH - MP_BL_SPLEEN_LENGTH) / IC_DAYS,
    DM_AGEs = (DM_AGE - mean(DM_AGE)) / 10,
    MB_relevel = relevel(factor(MB_COMBINED), ref = "1")
  )

# baseline distributions

para_dist1 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_COMBINED
    )
  )

para_dist2 <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_COMBINED
    )
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/parasite/para_dist1.png", plot = para_dist1, width = 16, height = 9)
ggsave(filename = "Analysis/parasite/para_dist2.png", plot = para_dist2, width = 16, height = 9)

# association with age, sex, liver and spleen size

para_age <- ads_model %>% 
  ggplot() + 
  geom_jitter(
    aes(
      x = DM_AGE,
      y = MB_COMBINED
    ),
    width = 0.2,
    height = 0.2
  ) + 
  geom_smooth(
    aes(
      x = DM_AGE,
      y = MB_COMBINED
    ),
    method = "loess",
    se = TRUE    
  )

para_sex <- ads_model %>% 
  ggplot() + 
  geom_histogram(
    aes(
      x = MB_COMBINED,
      y = after_stat(ndensity),
      fill = DM_SEX
    ),
    alpha = 0.5,
    position = "identity"
  )

para_spleen1 <- ads_model %>% 
  ggplot() + 
  geom_jitter(
    aes(
      x = MB_COMBINED,
      y = MP_BL_SPLEEN_LENGTH
    ),
    width = 0.2,
    height = 0.2
  ) + 
  geom_smooth(
    aes(
       x = MB_COMBINED,
       y = MP_BL_SPLEEN_LENGTH,
    ),
    method = "loess",
    se = TRUE    
  )

para_spleen2 <- ads_model %>% 
  ggplot() + 
  geom_jitter(
    aes(
      x = MB_COMBINED,
      y = MP_BL_SPLEEN_LENGTH
    ),
    width = 0.2,
    height = 0.2
  ) + 
  geom_smooth(
    aes(
       x = MB_COMBINED,
       y = MP_BL_SPLEEN_LENGTH,
    ),
    method = "loess",
    se = TRUE    
  ) + 
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/parasite/para_age.png", plot = para_age, width = 16, height = 9)
ggsave(filename = "Analysis/parasite/para_sex.png", plot = para_sex, width = 16, height = 9)
ggsave(filename = "Analysis/parasite/para_spleen1.png", plot = para_spleen1, width = 16, height = 9)
ggsave(filename = "Analysis/parasite/para_spleen2.png", plot = para_spleen2, width = 16, height = 9)

# association with outcome
para_out1 <- ads_model %>% 
  ggplot() + 
  geom_jitter(
    aes(
      x = MB_COMBINED,
      y = as.numeric(OUT_DC_RELAPSE)
    ),
    width = 0.2,
    height = 0.2
  ) + 
  geom_smooth(
    aes(
       x = MB_COMBINED,
       y = as.numeric(OUT_DC_RELAPSE)
    ),
    method = "loess",
    se = TRUE    
  ) 


para_out2 <- ads_model %>% 
  ggplot() + 
  geom_jitter(
    aes(
      x = MB_COMBINED,
      y = as.numeric(OUT_DC_RELAPSE)
    ),
    width = 0.2,
    height = 0.2
  ) + 
  geom_smooth(
    aes(
       x = MB_COMBINED,
       y = as.numeric(OUT_DC_RELAPSE)
    ),
    method = "loess",
    se = TRUE    
  ) +
  facet_wrap(~STUDYID)

ggsave(filename = "Analysis/parasite/para_out1.png", plot = para_out1, width = 16, height = 9)
ggsave(filename = "Analysis/parasite/para_out2.png", plot = para_out2, width = 16, height = 9)

# models

# fixed effects
para_uni1 <- glm(
    OUT_DC_RELAPSE ~ MB_COMBINED,
    family = binomial(),
    data = ads_model
    )

AIC(para_uni1)
summary(para_uni1)
tidy(para_uni1)
glance(para_uni1)

para_uni2 <- glm(
    OUT_DC_RELAPSE ~ MB_relevel,
    family = binomial(),
    data = ads_model %>% mutate(MB_relevel = relevel(factor(MB_COMBINED), ref = "1"))
    )
summary(para_uni2)
AIC(para_uni2)

# random effects
para_re1 <- glmer(
    OUT_DC_RELAPSE ~ MB_COMBINED + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(para_re1)
AIC(para_re1)

para_re2 <- glmer(
    OUT_DC_RELAPSE ~ MB_relevel + (1 | STUDYID),
    family = binomial(),
    data = ads_model %>% mutate(MB_relevel = relevel(factor(MB_COMBINED), ref = "1"))
    )
summary(para_re2)
AIC(para_re2) # including parasite count as a level seems to give a better fit...

# some multivariable random effects models 
para_re3 <- glmer(
    OUT_DC_RELAPSE ~ MB_COMBINED + MP_BL_SPLEEN_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(para_re3)
AIC(para_re3) 

para_re4 <- glmer(
    OUT_DC_RELAPSE ~ MB_COMBINED + MP_BL_SPLEEN_LENGTH + MP_IC_SPLEEN_LENGTH + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(para_re4)
AIC(para_re4) 

para_re5 <- glmer(
    OUT_DC_RELAPSE ~ MB_COMBINED + MP_BL_SPLEEN_LENGTH + DM_AGEs + I(DM_AGEs^2) + DM_SEX + (1 | STUDYID),
    family = binomial(),
    data = ads_model
    )
summary(para_re5)
AIC(para_re5)

# splines ----
library(splines)

spline_fit1 <- glm(
  formula = OUT_DC_RELAPSE ~ MB_COMBINED,
  family = binomial(),
  data = ads_model
)
summary(spline_fit1)
tidy(spline_fit1)

spline_plot1 <- ads_model %>% 
  mutate(MB_FACTOR = relevel(factor(MB_COMBINED), ref = "1")) %>% 
  ggplot() +
  geom_smooth(
    aes(
      x = MB_COMBINED,
      y = as.numeric(OUT_DC_RELAPSE)
    ),
    method = "glm",
    formula = y ~ x,
    method.args = list(family = "binomial"),
    se = TRUE
  )
ggplot_build(spline_plot1)$data[[1]] %>% View()

spline_fit2 <- glm(
  formula = OUT_DC_RELAPSE ~ bs(MB_COMBINED),
  family = binomial(),
  data = ads_model
)
summary(spline_fit2)
tidy(spline_fit2)

spline_plot2 <- ads_model %>% 
  mutate(MB_FACTOR = relevel(factor(MB_COMBINED), ref = "1")) %>% 
  ggplot() +
  geom_smooth(
    aes(
      x = MB_COMBINED,
      y = as.numeric(OUT_DC_RELAPSE)
    ),
    method = "glm",
    formula = y ~ bs(x),
    method.args = list(family = "binomial"),
    se = TRUE
  )
spline_plot2
ggplot_build(spline_plot1)$data[[1]] #%>% View()

######


