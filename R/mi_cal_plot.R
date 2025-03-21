
# look at random intercept and random slope models
model <- as.data.frame(prog$fit$result$model_sum)
imputations <- prog$mice$result$m
data <- complete(prog$mice$result_grp, "long", include = FALSE)
data <- data %>% filter(STUDYID == 16)
lp <- predict_lp(data = data, model = model)

df_cal <- data.frame(data[, c("OUT_DC_RELAPSE", "STUDYID")], lp)
colnames(df_cal) <- c("response", "dataset", "predictor")

# adjust calibration intercept to match STUDYID = 16 (i.e. we are only interesting in calibration slope)
model_adjust <- glm(
  formula = response ~ offset(predictor),
  data = df_cal[df_cal$dataset == 16, ],
  family = binomial()
)
citl_adjust <- coef(model_adjust)[[1]]

df_cal[, "predictor"] <- df_cal[, "predictor"] + citl_adjust
df_cal[, "prob"] <- 1 / (1 + exp(-df_cal[, "predictor"]))

#hist(df_cal[, "predictor"])
#hist(df_cal[, "prob"])
#plot(df_cal[, "predictor"], df_cal[, "prob"] )

df_cal[, "groups"] <- cut(
  df_cal[, "prob"],
  breaks = quantile(df_cal[, "prob"], prob = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)),
  labels = c(1:10),
  include.lowest = TRUE
)

df_sum <- df_cal %>% group_by(groups) %>% 
  summarise(
    n = n(),
    obs = mean(response),
    exp = mean(prob)
  ) %>% 
  mutate(
    lci = pmax(0, obs - (1.96 * (((obs * (1 - obs)) / (n / imputations))^.5))),
    uci = pmin(1, obs + (1.96 * (((obs * (1 - obs)) / (n / imputations))^.5)))
  )

loess_fit <- loess(
  formula = as.integer(response) ~ prob,
  data = df_cal,
  weights = rep(1 / prog$mice$result$m, nrow(df_cal))
)

pred_loess <- predict(loess_fit, se = TRUE)
pred_loess_y <- pred_loess$fit
pred_loess_se <- pred_loess$se.fit
upper_ci <- pred_loess_y + 1.96 * pred_loess_se * sqrt(imputations)
lower_ci <- pred_loess_y - 1.96 * pred_loess_se * sqrt(imputations)

loess_df <- data.frame(
  prob = df_cal$prob,
  fit = pred_loess_y,
  lower_ci = lower_ci,
  upper_ci = upper_ci
)

df_plot1 <- df_sum %>% ggplot() + 
  geom_pointrange(
    aes(x = exp, y = obs, ymin = lci, ymax = uci),
    size = 0) +
  geom_point(
    aes(x = exp, y = obs),
    shape = 1,
    colour = "red",
    size = 0.9
  ) + 
  geom_line(
    data = loess_df,
    aes(x = prob, y = fit)
  ) + 
  geom_ribbon(
    data = loess_df,
    aes(ymin = lower_ci, ymax = upper_ci, x = prob, y = fit),
    alpha = 0.2,
    fill = "#8bbce8"
  ) + 
  annotate("segment", y = 0, x = 0, yend = 1, xend =  1, linetype = "dashed") +
  coord_fixed(
    ratio = 1,
    xlim = c(-0.01, 0.3),
    ylim = c(-0.01, 0.3)
  ) +
  theme_classic() + 
  labs(
    x = "Expected probability",
    y = "Observed probability"
  )

hist_data1 <- hist(df_cal$prob[df_cal$response], plot = FALSE, breaks = 40)  # You can adjust 'breaks' to change bin count
hist_data2 <- hist(df_cal$prob[!df_cal$response], plot = FALSE, breaks = 60)  # You can adjust 'breaks' to change bin count

hist_df1 <- data.frame(mid = hist_data1$mids, count = hist_data1$counts, response = 1)
hist_df2 <- data.frame(mid = hist_data2$mids, count = hist_data2$counts, response = 2)

hist_df <- bind_rows(hist_df1, hist_df2)
hist_df <- hist_df %>% mutate(mid = ifelse(response == 1, mid + 0.0015, mid))

df_plot2 <- ggplot(data = hist_df) + 
  geom_segment(
    aes(x = mid, xend = mid, y = count, yend = 0, colour = factor(response))) + 
  theme_void() 

library(patchwork)
df_plot1 / df_plot2 + plot_layout(ncol = 1, heights = c(7, 1), axes = "collect")

hist_df
