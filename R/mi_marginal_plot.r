#############################################
## calculate & plot marginal probabilities ##
#############################################
library(patchwork)
#library("AICcmodavg")

## CHOOSE IMPUTATION DATASETS
setwd("/Users/jameswilson/Library/CloudStorage/OneDrive-Personal/Documents/R/vl_relapse_model/results/analysis/kamrc_anaemia")
files <- dir()
files
file <- files[1]
prog <- readRDS(file)

data_original <- complete(prog$mice$result_grp, action = 0)
prog$fit$result
prog$fit$result$model_sum
prog$cstat$result

data <- complete(prog$mice$result_grp, "long")
data %>% names()

ads_model <- prog$mice$original
model <- prog$fit$result$model_sum
model_mira <- prog$fit$result$model_mira
model_mipo <- pool(model_mira)

# get the full matrix ubar (instead of only the diagonal)
m <- prog$mice$result$m
vw <- Reduce("+", lapply(model_mira, vcov)) / m
qbar <- getqbar(model_mipo)
qhats <- sapply(model_mira, fixef)

vw <- Reduce("+", lapply(model_mira, vcov)) / m
vb <- (1 / (m - 1)) * (qhats - qbar) %*% t(qhats - qbar)
vt <- vw + (1 + 1 / (m)) * vb  # this is the variance-covariance matrix

## adjust calibration intercept to match STUDYID = 16 (i.e. we are not interested in calibration of intercept)
lp1 <- predict_lp(data = data, model = model)
#lp2 <- predict(model_mira[[1]], newdata = data, re.form = ~0, type = "link", se.fit = TRUE)$fit
#lp3 <- predictSE(mod = model_mira[[1]], newdata = data, type = "link", se.fit = TRUE)

df <- data.frame("lp" = lp1, "response" = data$OUT_DC_RELAPSE, "dataset" = data$STUDYID)

model_adjust <- glm(
  formula = response ~ offset(lp),
  data = df[df$dataset == 9, ],
  family = binomial()
)

citl_adjust <- coef(model_adjust)[[1]]
variables <- dimnames(vt)[[1]]
variables[variables == "(Intercept)"] <- "INT"

# calculate 'average' values for STUDYID = 16 for marginal probabilities
data16 <- data[data$STUDYID == 9, ]
data16 <- data16 %>% dplyr::select(all_of(variables))
data16_means <- data16 %>% summarise(across(everything(), ~mean(.x)))

# need to store means for reconstruction of marginal probabilities
# ads_model <- prog$mice$original
# var_adj <- data.frame(variable = character(), centring = numeric(), scale = numeric(), fun = character())
# var_adj <- rbind(var_adj, data.frame(variable = "STUDYID",                centring = 0,                                               scale = 0,  fun = ""))
# var_adj <- rbind(var_adj, data.frame(variable = "VL_HISTORY",             centring = 0,                                               scale = 0,  fun = ""))
# var_adj <- rbind(var_adj, data.frame(variable = "DM_SEX",                 centring = 0,                                               scale = 0,  fun = ""))
# var_adj <- rbind(var_adj, data.frame(variable = "LB_BL_HGBs",             centring = mean(ads_model$LB_BL_HGB, na.rm = TRUE),         scale = 10, fun = ""))
# var_adj <- rbind(var_adj, data.frame(variable = "LB_BL_ALTs",             centring = mean(log(ads_model$LB_BL_ALT), na.rm = TRUE),    scale = 0,  fun = "log"))
# var_adj <- rbind(var_adj, data.frame(variable = "LB_BL_WBCs",             centring = mean(log(ads_model$LB_BL_WBC), na.rm = TRUE),    scale = 0,  fun = "log"))
# var_adj <- rbind(var_adj, data.frame(variable = "LB_BL_PLATs",            centring = mean(log(ads_model$LB_BL_PLAT), na.rm = TRUE),   scale = 0,  fun = "log"))
# var_adj <- rbind(var_adj, data.frame(variable = "LB_BL_CREATs",           centring = mean(log(ads_model$LB_BL_CREAT), na.rm = TRUE),  scale = 0,  fun = "log"))
# var_adj <- rbind(var_adj, data.frame(variable = "MP_BL_SPLEEN_LENGTHs2",  centring = -1,                                              scale = 0,  fun = "log (first)")) # antilog first!
# var_adj <- rbind(var_adj, data.frame(variable = "DM_AGEs",                centring = mean(ads_model$DM_AGE, na.rm = TRUE),            scale = 10, fun = "")) 
# var_adj <- rbind(var_adj, data.frame(variable = "MB_COMBINEDs",           centring = 1,                                               scale = 0,  fun = "")) 
# var_adj <- rbind(var_adj, data.frame(variable = "VL_DURATIONs",           centring = mean(log(ads_model$VL_DURATION), na.rm = TRUE),  scale = 0,  fun = "log")) 
# var_adj <- rbind(var_adj, data.frame(variable = "LB_BL_HGB_GRP2",         centring = 0,                                               scale = 0,  fun = "")) 
# var_adj <- rbind(var_adj, data.frame(variable = "ZZ_MAL",                 centring = 0,                                               scale = 0,  fun = "")) 
# var_adj <- rbind(var_adj, data.frame(variable = "TREAT_GRP4",             centring = 0,                                               scale = 0,  fun = "")) 

################
# VL_DURATIONs #
################

VL_DURATIONsO <- seq(1, 750, 0.5)
VL_DURATIONs <- log(VL_DURATIONsO) - mean(log(ads_model$VL_DURATION), na.rm = TRUE)

data16_mean_matrix <- data16_means[rep(1, length(VL_DURATIONsO)), ]
data16_mean_matrix[, "VL_DURATIONs"] <- VL_DURATIONs
#data16_mean_matrix[, "VL_DURATIONsO"] <- VL_DURATIONsO
#data16_mean_matrix[, c("LP_EST", "LP_SE")] <- NA

dm <- as.matrix(data16_mean_matrix) 
coef <- model_mipo$pool$estimate
dm %>% head()
est <- (dm %*% coef) + citl_adjust

se <- numeric(length(VL_DURATIONsO))
for (i in 1:length(VL_DURATIONsO)) {
  se[i] <- sqrt((dm[i, ]) %*% vt %*% dm[i, ])
}

uci <- plogis(est + 1.96 * se)
lci <- plogis(est - 1.96 * se)
prob <- plogis(est)

marg_plot_vl <- data.frame(
  prob = prob,
  prob_uci = uci,
  prob_lci = lci,
  VL_DURATIONsO = VL_DURATIONsO/7
)

plot_vld1 <- marg_plot_vl %>% filter(VL_DURATIONsO >=1) %>% ggplot() + 
  geom_ribbon(aes(x = VL_DURATIONsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.4, fill = "#ECF6E0") +
  geom_line(aes(x = VL_DURATIONsO, y = prob)) + 
  geom_line(aes(x = VL_DURATIONsO, y = prob_lci), linetype = "dashed") + 
  geom_line(aes(x = VL_DURATIONsO, y = prob_uci), linetype = "dashed") + 
  theme_minimal(base_size = 15) + 
  labs(x = "Fever duration (weeks)", y = "Relapse probability (95% CI)") + 
  scale_x_log10()

plot_vld2 <- marg_plot_vl %>% filter(VL_DURATIONsO >=1 ) %>% ggplot() + 
  geom_line(aes(x = VL_DURATIONsO, y = prob)) + 
  geom_line(aes(x = VL_DURATIONsO, y = prob_lci), linetype = "dashed") + 
  geom_line(aes(x = VL_DURATIONsO, y = prob_uci), linetype = "dashed") + 
  geom_ribbon(aes(x = VL_DURATIONsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.4, fill = "#ECF6E0") +
  theme_minimal(base_size = 15) + 
  scale_x_continuous(
    minor_breaks = seq(1,26,1),
    breaks = seq(1,26,1),
    labels = c("1", rep("", 3), "5", rep("", 4), "10", rep("", 4), "15", rep("", 4), "20", rep("", 4), 25, "")
  ) + 
  scale_y_continuous(
    breaks = seq(0,0.11,0.01),
    minor_breaks = seq(0,0.11,0.005),
    labels = c("0","","2","","4", "", "6", "", "8", "", "10", "")
  ) +
  coord_cartesian(xlim = c(1, 26), ylim = c(0,0.12)) +
  labs(x = "Duration of fever (weeks)", y = "% Relapse probability (95% CI)")
plot_vld2
p
data16 <- data %>% mutate(VL_DURATIONsO =  exp(VL_DURATIONs + mean(log(ads_model$VL_DURATION), na.rm = TRUE)) / 7)
plot_vld3 <- data16 %>% ggplot() + 
  geom_histogram(aes(x = VL_DURATIONsO), binwidth = 1) + 
  coord_cartesian(xlim = c(1, 26)) +
  labs(y = "", x = "Fever duration (weeks)") +
  scale_x_continuous(
    minor_breaks = seq(1,26,1),
    breaks = seq(1,26,1),
    labels = c("1", rep("", 3), "5", rep("", 4), "10", rep("", 4), "15", rep("", 4), "20", rep("", 4), 25, "")
  ) + 
  theme_minimal() + 
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

plot_vld2 / plot_vld3 + plot_layout(heights = c(6, 1))
plot_vld1

data16 %>% count(VL_DURATIONsO)
ggplot(data16) + geom_boxplot(aes(x = VL_DURATIONsO))

########################
# PARASITE SMEAR GRADE #
########################

MB_COMBINEDsO <- c(1, 2, 3, 4, 5)
MB_COMBINEDs <- MB_COMBINEDsO - 1

data16_mean_matrix <- data16_means[rep(1, length(MB_COMBINEDsO)), ]
data16_mean_matrix[, "MB_COMBINEDs"] <- MB_COMBINEDs
#data16_mean_matrix[, "VL_DURATIONsO"] <- VL_DURATIONsO
#data16_mean_matrix[, c("LP_EST", "LP_SE")] <- NA

dm <- as.matrix(data16_mean_matrix) 
coef <- model_mipo$pool$estimate

est <- (dm %*% coef) + citl_adjust

se <- numeric(length(MB_COMBINEDsO))
for (i in 1:length(MB_COMBINEDsO)) {
  se[i] <- sqrt(t(dm[i, ]) %*% vt %*% dm[i, ])
}

uci <- plogis(est + 1.96 * se)
lci <- plogis(est - 1.96 * se)
prob <- plogis(est)

marg_plot_smear <- data.frame(
  prob = prob,
  prob_uci = uci,
  prob_lci = lci,
  MB_COMBINEDsO = MB_COMBINEDsO
)

plot_smear1 <- marg_plot_smear %>% ggplot() + 
  geom_ribbon(aes(x = MB_COMBINEDsO, y = prob, ymin = prob_lci, ymax = prob_uci), fill = "#ECF6E0", alpha = 0.4) +
  geom_errorbar(aes(x = MB_COMBINEDsO, y = prob, ymin = prob_lci, ymax = prob_uci), width = 0.1) + 
  geom_point(aes(x = MB_COMBINEDsO, y = prob)) + 
  geom_line(aes(x = MB_COMBINEDsO, y = prob), linetype = "dashed") +  
  scale_y_continuous(
    breaks = seq(0,0.16,0.01),
    minor_breaks = seq(0,0.16,0.01),
    labels = c("0","","2","","4", "", "6", "", "8", "", "10", "", "12", "", "14", "", "16")
  ) +
  coord_cartesian(xlim = c(1, 5), ylim = c(0,0.16)) +
  theme_minimal(base_size = 15) + 
  scale_x_continuous(
    breaks = seq(1,5,1),
    minor_breaks = seq(1,5,1),
    labels = c("1+", "2+", "3+", "4+", "5+")
  ) +
  labs(x = "Splenic smear grade", y = "% Relapse probability (95% CI)")
plot_smear1


data16 <- data %>% mutate(MB_COMBINEDsO = MB_COMBINEDs + 1)
plot_smear2 <- data16 %>% ggplot() + 
  geom_histogram(aes(x = MB_COMBINEDsO)) + 
  coord_cartesian(xlim = c(1, 5)) +
  labs(y = "", x = "Parasite smear") +
  theme_minimal() + 
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

plot_smear1
plot_smear2

plot_smear1 / plot_smear2 + plot_layout(heights = c(6, 1))

#########
## AGE ##
#########

DM_AGEsO <- seq(0, 70, 0.1)
DM_AGEs <- (DM_AGEsO - mean(ads_model$DM_AGE, na.rm = TRUE)) / 10

data16_mean_matrix <- data16_means[rep(1, length(DM_AGEsO)), ]
data16_mean_matrix[, "DM_AGEs"] <- DM_AGEs
data16_mean_matrix[, "ZZ_AGEs2"] <- DM_AGEs^2
#data16_mean_matrix[, "VL_DURATIONsO"] <- VL_DURATIONsO
#data16_mean_matrix[, c("LP_EST", "LP_SE")] <- NA

data16_mean_matrix

dm <- as.matrix(data16_mean_matrix) 
dm[]
coef <- model_mipo$pool$estimate

est <- (dm %*% coef) + citl_adjust

se <- numeric(length(DM_AGEsO))
for (i in 1:length(DM_AGEsO)) {
  se[i] <- sqrt(t(dm[i, ]) %*% vt %*% dm[i, ])
}

uci <- plogis(est + 1.96 * se)
lci <- plogis(est - 1.96 * se)
prob <- plogis(est)

marg_plot_age <- data.frame(
  prob = prob,
  prob_uci = uci,
  prob_lci = lci,
  DM_AGEsO = DM_AGEsO
)

# compare with automatic prediction methods
# data16_compare <- data16_mean_matrix %>% 
#   rename("(Intercept)" = "INT") %>% 
#   mutate(
#     TREAT_GRP4 = case_when(
#       TREAT_GRP4SDA == 1 ~ "SDA",
#       TREAT_GRP4OTHER == 1 ~ "OTHER",
#       .default = "MF"),
#     TREAT_GRP4 = factor(TREAT_GRP4, levels = c("MF", "SDA", "OTHER"))
#     )

# lp2 <- predictSE(mod = model_mira[[1]], newdata = data16_compare, type = "link", se.fit = TRUE)
# lp2 <- predict(model_mira[[1]], newdata = data16_compare, re.form = ~0, type = "link", se.fit = TRUE)

# marg_plot_age <- data.frame(
#   prob = plogis(lp2$fit + citl_adjust),
#   prob_uci = plogis((lp2$fit + citl_adjust) + 1.96 * lp2$se.fit),
#   prob_lci = plogis((lp2$fit + citl_adjust) - 1.96 * lp2$se.fit),
#   DM_AGEsO = DM_AGEsO
# )

plot_age1 <- marg_plot_age %>% ggplot() + 
  geom_ribbon(aes(x = DM_AGEsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.4, fill = "#ECF6E0") +
  geom_line(aes(x = DM_AGEsO, y = prob)) + 
  geom_line(aes(x = DM_AGEsO, y = prob_lci), linetype = "dashed") + 
  geom_line(aes(x = DM_AGEsO, y = prob_uci), linetype = "dashed") + 
  theme_minimal(base_size = 16) + 
  coord_cartesian(xlim = c(0, 70), ylim = c(0,0.10)) +
  scale_y_continuous(
    breaks = seq(0,0.10,0.01),
    labels = c("0","1","2","3","4", "5", "6", "7", "8", "9", "10")
  ) +
  scale_x_continuous(
    breaks = seq(0,70,10),
    minor_breaks = seq(0,70, 5)
  ) +
  labs(x = "Age (years)", y = "% Relapse probability (95% CI)")
plot_age1

data16 <- data %>% mutate(DM_AGEsO =  10 * DM_AGEs + mean(ads_model$DM_AGE, na.rm = TRUE))
plot_age2 <- data16 %>% ggplot() + 
  geom_histogram(aes(x = DM_AGEsO)) + 
  coord_cartesian(xlim = c(0, 70)) +
  labs(y = "", x = "Age (years)") +
  theme_minimal() + 
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

plot_age1
plot_age1 / plot_age2 + plot_layout(heights = c(6, 1))


#################
## AGE AND SEX ##
#################

DM_AGEsO <- seq(0, 70, 0.1)

DM_AGEs <- (DM_AGEsO - mean(ads_model$DM_AGE, na.rm = TRUE)) / 10

data16_mean_matrix <- data16_means[rep(1, length(DM_AGEsO)), ]
data16_mean_matrix[, "DM_AGEs"] <- DM_AGEs
data16_mean_matrix[, "ZZ_AGEs2"] <- DM_AGEs^2
#data16_mean_matrix[, "VL_DURATIONsO"] <- VL_DURATIONsO
#data16_mean_matrix[, c("LP_EST", "LP_SE")] <- NA
data16_mean_matrix_m <- data16_mean_matrix %>% mutate(DM_SEX = 1)
data16_mean_matrix_f <- data16_mean_matrix %>% mutate(DM_SEX = 0)

data16_mean_matrix <- rbind(data16_mean_matrix_m, data16_mean_matrix_f)

dm <- as.matrix(data16_mean_matrix) 
coef <- model_mipo$pool$estimate

est <- (dm %*% coef) + citl_adjust

se <- numeric(length(DM_AGEsO))
for (i in 1:length(DM_AGEsO)) {
  se[i] <- sqrt(t(dm[i, ]) %*% vt %*% dm[i, ])
}

uci <- plogis(est + 1.96 * se)
lci <- plogis(est - 1.96 * se)
prob <- plogis(est)

marg_plot_age <- data.frame(
  prob = prob,
  prob_uci = uci,
  prob_lci = lci,
  DM_AGEsO = rep(DM_AGEsO,2),
  DM_SEX = dm[,"DM_SEX"]
)

# compare with automatic prediction methods
# data16_compare <- data16_mean_matrix %>% 
#   rename("(Intercept)" = "INT") %>% 
#   mutate(
#     TREAT_GRP4 = case_when(
#       TREAT_GRP4SDA == 1 ~ "SDA",
#       TREAT_GRP4OTHER == 1 ~ "OTHER",
#       .default = "MF"),
#     TREAT_GRP4 = factor(TREAT_GRP4, levels = c("MF", "SDA", "OTHER"))
#     )

# lp2 <- predictSE(mod = model_mira[[1]], newdata = data16_compare, type = "link", se.fit = TRUE)
# lp2 <- predict(model_mira[[1]], newdata = data16_compare, re.form = ~0, type = "link", se.fit = TRUE)

# marg_plot_age <- data.frame(
#   prob = plogis(lp2$fit + citl_adjust),
#   prob_uci = plogis((lp2$fit + citl_adjust) + 1.96 * lp2$se.fit),
#   prob_lci = plogis((lp2$fit + citl_adjust) - 1.96 * lp2$se.fit),
#   DM_AGEsO = DM_AGEsO
# )

plot_age1 <- marg_plot_age %>% ggplot() + 
  geom_line(data = marg_plot_age %>% filter(DM_SEX == 1), aes(x = DM_AGEsO, y = prob, group = DM_SEX)) + 
  geom_line(data = marg_plot_age %>% filter(DM_SEX == 1), aes(x = DM_AGEsO, y = prob_lci, group = DM_SEX), linetype = "dashed") + 
  geom_line(data = marg_plot_age %>% filter(DM_SEX == 1), aes(x = DM_AGEsO, y = prob_uci, group = DM_SEX), linetype = "dashed") + 
  #geom_ribbon(data = marg_plot_age %>% filter(DM_SEX == 0), aes(x = DM_AGEsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.2, fill = "darkblue") +
  geom_ribbon(data = marg_plot_age %>% filter(DM_SEX == 1), aes(x = DM_AGEsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.2, fill = "darkred") +
  geom_line(data = marg_plot_age %>% filter(DM_SEX == 0), aes(x = DM_AGEsO, y = prob, group = DM_SEX)) + 
  geom_line(data = marg_plot_age %>% filter(DM_SEX == 0), aes(x = DM_AGEsO, y = prob_lci, group = DM_SEX), linetype = "dashed") + 
  geom_line(data = marg_plot_age %>% filter(DM_SEX == 0), aes(x = DM_AGEsO, y = prob_uci, group = DM_SEX), linetype = "dashed") + 
  #geom_ribbon(data = marg_plot_age %>% filter(DM_SEX == 0), aes(x = DM_AGEsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.2, fill = "darkblue") +
  geom_ribbon(data = marg_plot_age %>% filter(DM_SEX == 0), aes(x = DM_AGEsO, ymin = prob_lci, ymax = prob_uci), alpha = 0.2, fill = "darkblue") +
  theme_minimal() + 
  coord_cartesian(xlim = c(0, 70), ylim = c(0, 0.16)) +
    scale_y_continuous(
    breaks = seq(0,0.16,0.01),
    labels = c("0","","0.02","","0.04", "", "0.06", "", "0.08", "", "0.10", "", "0.12", "", "0.14", "", "0.16")
  ) +
  theme(axis.text.x =  element_blank()) + 
  labs(x = "", y = "Relapse probability (95% CI)")
plot_age1

data16 <- data %>% mutate(DM_AGEsO =  10 * DM_AGEs + mean(ads_model$DM_AGE, na.rm = TRUE))
plot_age2 <- data16 %>% ggplot() + 
  geom_histogram(aes(x = DM_AGEsO)) + 
  coord_cartesian(xlim = c(0, 70)) +
  labs(y = "", x = "Age (years)") +
  theme_minimal() + 
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

plot_age1 / plot_age2 + plot_layout(heights = c(6, 1))


###################
## SPLEEN LENGTH ##
###################

MP_BL_SPLEEN_LENGTHO <- seq(0, 20, 0.1)
MP_BL_SPLEEN_LENGTHs2 <- log(MP_BL_SPLEEN_LENGTHO + 1)

data16_mean_matrix <- data16_means[rep(1, length(MP_BL_SPLEEN_LENGTHO)), ]
data16_mean_matrix[, "MP_BL_SPLEEN_LENGTHs2"] <- MP_BL_SPLEEN_LENGTHs2
#data16_mean_matrix[, "VL_DURATIONsO"] <- VL_DURATIONsO
#data16_mean_matrix[, c("LP_EST", "LP_SE")] <- NA

dm <- as.matrix(data16_mean_matrix) 
coef <- model_mipo$pool$estimate

est <- (dm %*% coef) + citl_adjust

se <- numeric(length(MP_BL_SPLEEN_LENGTHO))
for (i in 1:length(MP_BL_SPLEEN_LENGTHO)) {
  se[i] <- sqrt(t(dm[i, ]) %*% vt %*% dm[i, ])
}

uci <- plogis(est + 1.96 * se)
lci <- plogis(est - 1.96 * se)
prob <- plogis(est)

marg_plot_spleen <- data.frame(
  prob = prob,
  prob_uci = uci,
  prob_lci = lci,
  MP_BL_SPLEEN_LENGTHO = MP_BL_SPLEEN_LENGTHO
)

plot_spleen1 <- marg_plot_spleen %>% ggplot() + 
  geom_line(aes(x = MP_BL_SPLEEN_LENGTHO, y = prob)) + 
  geom_line(aes(x = MP_BL_SPLEEN_LENGTHO, y = prob_lci), linetype = "dashed") + 
  geom_line(aes(x = MP_BL_SPLEEN_LENGTHO, y = prob_uci), linetype = "dashed") + 
  geom_ribbon(aes(x = MP_BL_SPLEEN_LENGTHO, ymin = prob_lci, ymax = prob_uci), alpha = 0.3, fill = "#b0b0ff") +
  theme_minimal() + 
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 0.15)) +
  theme(axis.text.x =  element_blank()) + 
  labs(x = "", y = "Relapse probability (95% CI)")

data16 <- data16 %>% mutate(MP_BL_SPLEEN_LENGTHO = exp(MP_BL_SPLEEN_LENGTHs2) - 1)
plot_spleen2 <- data16 %>% ggplot() + 
  geom_histogram(aes(x = MP_BL_SPLEEN_LENGTHO)) + 
  coord_cartesian(xlim = c(0, 20)) +
  labs(y = "", x = "Spleen size (cm)") +
  theme_minimal() + 
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

plot_spleen1 / plot_spleen2 + plot_layout(heights = c(6, 1))

######################
## PLOT ODDS RATIOS ##
######################

df <- prog$fit$result$model_sum
df_merge <- tibble(
  term = c("Treatment group: Miltefosine", "Non-severe anaemia", "Sex: Female"),
  estimate = c(0,0,0),
  std.error = c(0,0,0)
)
df <- bind_rows(df, df_merge)

levels <- c(
  "Sex: Female", "DM_SEX", "DM_AGEs", "ZZ_AGEs2", "Treatment group: Miltefosine", "TREAT_GRP4SDA", "TREAT_GRP4OTHER", "MB_COMBINEDs", "Non-severe anaemia", "LB_BL_HGB_GRP3",
  "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2")
df <- df %>% 
  mutate(
    term = factor(term, levels = rev(levels))
  )

df %>% filter(term!="INT") %>% ggplot() + 
  geom_pointrange(aes(x = term, y = exp(estimate), ymin = exp(estimate-1.96*std.error), ymax = exp(estimate + 1.96*std.error))) + 
  coord_flip() + 
  theme_minimal() + 
  labs(y = "Odds ratio of relapse") + 
  scale_y_log10(
    breaks = c(0.5, 1, 1.5, 2),
    minor_breaks = seq(0.2,2,0.1)
  )

