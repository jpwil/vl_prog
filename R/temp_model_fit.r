model_test %>% names()
model_test$mice %>% names()

# saturated model
select_rr_157_sex$var_final
m0 <- model_test
m0$var$result <- select_rr_157_sex
m0$fit$result <- model_fit(m0)
m0$fit$result$model_sum
m0$fit$result$icc %>% mean()

# saturated model no sex

select_rr_157_sex$var_final <- select_rr_157_sex$var_final[-1, ]
m01 <- model_test
m01$var$result <- select_rr_157_sex
m01$fit$result <- model_fit(m01)
m01$fit$result$model_sum
m01$fit$result$icc %>% mean()


# select_rr_5_sex
m1 <- model_test
m1$var$result <- select_rr_5_sex
m1$fit$result <- model_fit(m1)

m1$fit$result
m1$fit$result$icc %>% mean()


m3 <- model_test
m3$var$result$var_final <- tibble(term = c("DM_AGEs", "ZZ_AGEs2", "LB_BL_HGB_GRP3", "VL_DURATIONs", "MP_BL_SPLEEN_LENGTHs2", "MB_COMBINEDs", "TREAT_GRP4", "VL_HISTORY"))
m3$fit$result <- model_fit(m3)


m2$fit$result
mean(m2$fit$result$icc)

m3$fit$result
mean(m3$fit$result$icc)



select_rr_5_sex %>% names()
select_rr_10_sex
select_rr_157_sex

select_rr_5_nk
select_rr_10_nk
select_rr_157_nk