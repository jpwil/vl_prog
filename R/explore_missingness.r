library(tidyverse)

df <- readRDS("data/ads_impute.rds")

sum(apply(df, 1, anyNA))/nrow(df)

var_miss <- apply(df, 2, function(x) sum(is.na(x))) / nrow(df) %>% sort()
barplot(sort(var_miss), las = 2, cex.names = 0.6, ylim = c(0, 1))

binary_df <- as.data.frame(ifelse(is.na(df), 1, 0))
binary_df <- cbind(binary_df, df$STUDYID) 
binary_df <- binary_df %>% select(-STUDYID) %>% rename(STUDYID = "df$STUDYID") %>% arrange(STUDYID) 

binary_df2 <- binary_df %>% group_by(STUDYID) %>% summarise(across(everything(), function(x) sum(x)), n = n())
binary_df3 <- binary_df2 %>% group_by(STUDYID) %>% summarise(across(everything(), function(x) x / n))

binary_df4 <- binary_df3 %>% select(-n) %>% 
  pivot_longer(
    cols = DM_SEX:ZZ_AGEs3,
    names_to = "Variable",
    values_to = "Missingness"
  )

binary_df4 %>% ggplot() + 
  geom_tile(
    aes(x = STUDYID, y = as_factor(Variable), fill = Missingness)) + 
    scale_x_continuous(breaks = 1:19) + 
    labs(x = "STUDYID", y = "Variable") + 
    theme_minimal()
