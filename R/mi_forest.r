#########################
## create forest plots ##
#########################

cwcl <- prog$cstat$result$cw_ind
cscl <- prog$cal_boot$result$cal_slope_cluster
cicl <- prog$cal_boot$result$cal_intercept_cluster

cscl <- cscl %>% as_tibble() %>% 
  rename(
    estimate_cscl = estimate,
    ci_l_cscl = ci_l,
    ci_u_cscl = ci_u) %>% 
  mutate(
    cluster = row_number()
  ) %>% 
  dplyr::select(cluster, estimate_cscl, ci_l_cscl, ci_u_cscl)

cicl <- cicl %>% as_tibble() %>% 
  rename(
    estimate_cicl = estimate,
    ci_l_cicl = ci_l,
    ci_u_cicl = ci_u) %>% 
  mutate(
    cluster = row_number()
  ) %>% 
  dplyr::select(estimate_cicl, ci_l_cicl, ci_u_cicl)

cwcl <- cwcl %>% as_tibble() %>% 
  rename(
    estimate_cwcl = med,
    ci_l_cwcl = lq,
    ci_u_cwcl = uq) %>% 
  mutate(
    cluster = row_number()
  ) %>% 
  dplyr::select(estimate_cwcl, ci_l_cwcl, ci_u_cwcl)

cal_combined <- tibble(cwcl, cicl, cscl) %>% relocate(cluster)

plot1 <- cal_combined %>% 
  ggplot() + 
    geom_pointrange(
      aes( 
        x = reorder(cluster, estimate_cicl),
        ymin = ci_l_cicl,
        ymax = ci_u_cicl,
        y = estimate_cicl
      ),
      colour = "darkblue",
      fill = "darkblue"
    ) + 
  coord_flip() +
  theme_minimal() +
  labs(x = "Dataset", y = "Calibration-in-the-large (95% CI)")

plot2 <- cal_combined %>% 
  ggplot() + 
    geom_pointrange(
      aes( 
        x = reorder(cluster, estimate_cicl),
        ymin = ci_l_cscl,
        ymax = ci_u_cscl,
        y = estimate_cscl
      ),
      colour = "black"
    ) + 
  coord_flip() +
  theme_minimal() + 
  labs(x = "Dataset", y = "Calibration slope (95% CI)")

plot3 <- cal_combined %>% 
  ggplot() + 
    geom_pointrange(
      aes( 
        x = reorder(cluster, estimate_cwcl),
        ymin = ci_l_cwcl,
        ymax = ci_u_cwcl,
        y = estimate_cwcl
      ),
      colour = "black"
    ) + 
  coord_flip() +
  theme_minimal() + 
  labs(x = "Dataset", y = "Within-cluster concordance probability (95% CI)")

plot1
plot2
plot3

complete(prog$mice$result_grp, 1) %>% group_by(STUDYID) %>% summarise(n = n(), e = sum(OUT_DC_RELAPSE), prop = e / n) %>% arrange(prop)
prog$cstat$result$cw_all
