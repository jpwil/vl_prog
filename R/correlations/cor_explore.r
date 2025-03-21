########################################################################################################
## Look at the relationship bewteen relapse, duration of illness, anaemia, spleen size, history of VL ##
########################################################################################################

rm(list = ls())
library(tidyverse)
library(mgcv)
library(pROC)

# load the non-scaled, cleaned dataset (including missing data)
df <- readRDS("data/ads_clean.rds")
sort(names(df))

## RELAPSE AND FEVER DURATION ##

# look at univariable relationships between relapse (binary) and fever duration, spleen size, anaemia (hb)
df_relapse_dur <- df %>% 
  select(VL_DURATION, OUT_DC_RELAPSE) 

# only have duration of illness data for 104/225 relapses (2586/4421 total patients)
df %>% count(!is.na(VL_DURATION), OUT_DC_RELAPSE)

gam_model <- gam(
  OUT_DC_RELAPSE ~ s(VL_DURATION),
  family = binomial, 
  data = df_relapse_dur)

new_data <- data.frame(VL_DURATION = seq(min(df_relapse_dur$VL_DURATION, na.rm = TRUE), max(df_relapse_dur$VL_DURATION, na.rm = TRUE), length.out = 1e5))
pred <- predict(gam_model, newdata = new_data, type = "link", se.fit = TRUE)

new_data$prob <- plogis(pred$fit)
new_data$prob_lower <- plogis(pred$fit - 1.96 * pred$se.fit)
new_data$prob_upper <- plogis(pred$fit + 1.96 * pred$se.fit)

ggplot(data = new_data) +
  geom_line(
    aes(x = VL_DURATION, y = prob), 
    color = "#1e34c1", linewidth = 1.2) +  # GAM smooth line
  geom_ribbon(
    aes(x = VL_DURATION, ymin = prob_lower, ymax = prob_upper), 
    alpha = 0.3, fill = "#629ed6") +  # 95% CI band
  labs(
    x = "Fever Duration (days)", 
    y = "Probability of Relapse", 
    title = "Non-Linear Association Between Fever Duration and Relapse Probability") +
  coord_cartesian(
    ylim = c(0, 0.2)
  ) +
  scale_x_continuous(
    trans = "log10",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank())

ggplot(df, aes(x = log(VL_DURATION), fill = factor(OUT_DC_RELAPSE))) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(
    breaks = log(c(1, 2, 5, 10, 20, 50, 100, 200, 500)),
    labels = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) + 
  labs(x = "Fever duration", y = "Relapse", title = "Density of fever duration (log scale) in patients who relapse vs patients who do not relapse within 6 months") +
  scale_fill_manual(values = c("blue", "red"), name = "Relapse") +
  theme_minimal()

auc <- roc(df$OUT_DC_RELAPSE, df$VL_DURATION)
auc
ci.auc(auc)

## RELAPSE AND SPLEEN SIZE ##

# look at univariable relationships between relapse (binary) and fever duration, spleen size, anaemia (hb)

names(df) %>% sort()

# have spleen length for 3,658/4,421 patients (83%)
# 166/225 relapse cases (74%)
df %>% count(!is.na(MP_BL_SPLEEN_LENGTH), OUT_DC_RELAPSE)
hist(df$MP_BL_SPLEEN_LENGTH)
hist(log(df$MP_BL_SPLEEN_LENGTH))

gam_model <- gam(
  OUT_DC_RELAPSE ~ s(MP_BL_SPLEEN_LENGTH),
  family = binomial, 
  data = df)

new_data <- data.frame(MP_BL_SPLEEN_LENGTH = seq(min(df$MP_BL_SPLEEN_LENGTH, na.rm = TRUE), max(df$MP_BL_SPLEEN_LENGTH, na.rm = TRUE), length.out = 1e5))
pred <- predict(gam_model, newdata = new_data, type = "link", se.fit = TRUE)

new_data$prob <- plogis(pred$fit)
new_data$prob_lower <- plogis(pred$fit - 1.96 * pred$se.fit)
new_data$prob_upper <- plogis(pred$fit + 1.96 * pred$se.fit)

ggplot(data = new_data) +
  geom_line(
    aes(x = MP_BL_SPLEEN_LENGTH, y = prob), 
    color = "#1e34c1", linewidth = 1.2) +  # GAM smooth line
  geom_ribbon(
    aes(x = MP_BL_SPLEEN_LENGTH, ymin = prob_lower, ymax = prob_upper), 
    alpha = 0.3, fill = "#629ed6") +  # 95% CI band
  labs(
    x = "Spleen size (cm)", 
    y = "Probability of Relapse", 
    title = "Non-Linear Association Between Spleen Size and Relapse Probability") +
  coord_cartesian(
    ylim = c(0, 0.2)
  ) +
  scale_x_continuous(breaks = seq(1, 22)) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank())

ggplot(df, aes(x = MP_BL_SPLEEN_LENGTH, fill = factor(OUT_DC_RELAPSE))) +
  geom_density(alpha = 0.2) +
  scale_x_continuous() +
  labs(x = "Spleen size (cm)", y = "Relapse", title = "Density of spleen size in patients who relapse vs patients who do not relapse within 6 months") +
  scale_fill_manual(values = c("blue", "red"), name = "Relapse") +
  theme_minimal()

auc <- roc(df$OUT_DC_RELAPSE, df$MP_BL_SPLEEN_LENGTH)
auc
ci.auc(auc)

## RELAPSE AND BASELINE HAEMOGLOBIN ##

names(df) %>% sort()


df %>% count(!is.na(LB_BL_HGB), OUT_DC_RELAPSE)
hist(df$LB_BL_HGB)
hist(log(df$LB_BL_HGB))

gam_model <- gam(
  OUT_DC_RELAPSE ~ s(LB_BL_HGB),
  family = binomial, 
  data = df)

new_data <- data.frame(LB_BL_HGB = seq(min(df$LB_BL_HGB, na.rm = TRUE), max(df$LB_BL_HGB, na.rm = TRUE), length.out = 1e5))
pred <- predict(gam_model, newdata = new_data, type = "link", se.fit = TRUE)

new_data$prob <- plogis(pred$fit)
new_data$prob_lower <- plogis(pred$fit - 1.96 * pred$se.fit)
new_data$prob_upper <- plogis(pred$fit + 1.96 * pred$se.fit)

ggplot(data = new_data) +
  geom_line(
    aes(x = LB_BL_HGB, y = prob), 
    color = "#1e34c1", linewidth = 1.2) +  # GAM smooth line
  geom_ribbon(
    aes(x = LB_BL_HGB, ymin = prob_lower, ymax = prob_upper), 
    alpha = 0.3, fill = "#629ed6") +  # 95% CI band
  labs(
    x = "Haemoglobin (g/dL)", 
    y = "Probability of Relapse", 
    title = "Non-Linear Association Between Baseline Haemoglobin and Relapse Probability") +
  coord_cartesian(
    ylim = c(0, 0.2)
  ) +
  scale_x_continuous() + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank())

ggplot(df, aes(x = LB_BL_HGB, fill = factor(OUT_DC_RELAPSE))) +
  geom_density(alpha = 0.2) +
  scale_x_continuous() +
  labs(x = "Haemoglobin (g/dL)", y = "Relapse", title = "Density of baseline haemoglobin in patients who relapse vs patients who do not relapse within 6 months") +
  scale_fill_manual(values = c("blue", "red"), name = "Relapse") +
  theme_minimal()

auc <- roc(df$OUT_DC_RELAPSE, df$LB_BL_HGB)
auc
ci.auc(auc)

## HAEMOGLOBIN AND FEVER DURATION ##

df %>% ggplot(aes(x = VL_DURATION, y = LB_BL_HGB)) + 
  geom_jitter(
    width = 0.03, height = 1, alpha = 0.5) + 
  scale_x_continuous(
    trans = "log10",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) + 
  geom_smooth(method = "lm", level = 0.95) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(
    x = "Duration of fever (days, log scale)",
    y = "Baseline haemoglobin (g/dL)",
    title = "Scatter plot of baseline haemoglobin against duration of fever (log scale)"
  )

cor.test(df$LB_BL_HGB, df$VL_DURATION)

## HAEMOGLOBIN AND SPLEEN SIZE ## 

df %>% ggplot(aes(x = MP_BL_SPLEEN_LENGTH, y = LB_BL_HGB)) + 
  geom_jitter(
    width = 0.2, height = 1, alpha = 0.5) + 
  scale_x_continuous() +
  geom_smooth(method = "lm", level = 0.95) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(
    x = "Spleen size (cm)",
    y = "Baseline haemoglobin (g/dL)",
    title = "Scatter plot of baseline haemoglobin (g/dL) against baseline spleen size (cm)"
  )

cor.test(df$LB_BL_HGB, df$MP_BL_SPLEEN_LENGTH)

## DURATINO OF FEVER AND SPLEEN SIZE ## 

df %>% ggplot(aes(x = VL_DURATION, y = MP_BL_SPLEEN_LENGTH)) + 
  geom_jitter(
    width = 0.03, height = 1, alpha = 0.5) + 
  scale_x_continuous(
    trans = "log10",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) + 
  geom_smooth(method = "lm", level = 0.95) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank()) + 
  labs(
    x = "Duration of fever (days, log scale)",
    y = "Spleen size (cm)",
    title = "Scatter plot of baseline spleen size against duration of fever (log scale)"
  )

cor.test(df$MP_BL_SPLEEN_LENGTH, df$VL_DURATION)
