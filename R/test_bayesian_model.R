######################
## LOAD PROG OBJECT ##
######################

rm(list = ls())
packages <- c("lme4", "micemd", "anthro", "anthroplus", "tidyverse", "broom.mixed", "pROC", "meta")
for (pkg in packages) {
  library(pkg, character.only = TRUE)
}

files <- list.files(path = "Analysis/MI/", pattern = "^[0-9]{10}_MICE[0-9]{1}.rdata$", full.names = TRUE) 
files
load(file = files[[4]])

source("Analysis/MI/mi_prepare.R")        ## PREPARE DATASET
source("Analysis/MI/mi_initialise.R")     ## INITIALISE MI PARAMETERS
source("Analysis/MI/mi_summarise.R")      ## INITIALISE MICE SUMMARY
source("Analysis/MI/mi_post_grouping.R")  ## GROUP MALNUTRITION & ANAEMIA
source("Analysis/MI/select_rr.r")         ## VARIABLE SELECTION (RUBIN'S RULES)
source("Analysis/MI/model_fit.R")         ## FIT MODEL 

# explore models
mi_summarise(prog)
prog$var$result$var_track
prog$var$result$var_final %>% as_tibble()

prog$fit$result$model_sum 
pool(prog$fit$result$model_mira) %>% summary(conf.int = TRUE) %>% as_tibble()
write.csv(pool(prog$fit$result$model_mira) %>% summary(conf.int = TRUE), "model_1.csv")

-3.351 + qt(0.975, 178.7072) * 0.3074 
-3.351 - qt(0.975, 178.7072) * 0.3074 

prog$cstat$result$cw_all[, 2] %>% mean()

# look at random intercept and random slope models



dataset <- complete(prog$mice$result_grp, 1) %>% mutate(OUT_DC_RELAPSE = as.integer(OUT_DC_RELAPSE))
dataset %>% count(OUT_DC_RELAPSE)
model <- prog$fit$result$model_mira[[1]]
intercept <- fixef(model)[[1]]
lp <- predict(model, newdata = dataset, re.form = ~0, type = "link")
lp_noint <- lp - intercept
lp_ran <- predict(model, newdata = dataset, re.form = ~0, type = "link")

dataset <- cbind(dataset, lp, lp_ran, lp_noint, intercept)

model_glmer <- glmer(
  formula = OUT_DC_RELAPSE ~  offset(lp) + (1 | STUDYID),
  data = dataset,
  family = binomial()
)

fitted(model_glmer)
coef(model_glmer)

model_bayes1 <- brms::brm( 
  formula = OUT_DC_RELAPSE ~  offset(lp) + (1 | STUDYID),
  data = dataset,
  family = bernoulli(),
  control = list(adapt_delta = 0.99)
)

model_bayes2 <- brms::brm( 
  formula = OUT_DC_RELAPSE ~  1 + lp + (1 + lp | STUDYID),
  data = dataset,
  family = bernoulli(),
  control = list(adapt_delta = 0.99)
)

coef(model_bayes2)$STUDYID[,,"lp"]
ranef(model_bayes2)$STUDYID[,,"lp"]

plot <- coef(model_bayes2)$STUDYID[,,"lp"]
plot <- plot  %>% as_tibble() %>% mutate(cluster = row_number())

plot %>% 
ggplot() + 
  geom_pointrange(
    aes(
      x = cluster,
      y = Estimate,
      ymin = Q2.5,
      ymax = Q97.5 
    )
  )

model_bayes2 %>% summary()

summary(model_bayes)
ranef(model_bayes, summary = FALSE)$STUDYID  %>% dimnames()

model_freq <- lme4::glmer( 
  formula = OUT_DC_RELAPSE ~ 1 + (1 | STUDYID),
  data = dataset,
  family = binomial()
)


model_freq %>% summary()
model_bayes %>% summary()

dataset %>% count(as.integer(OUT_DC_RELAPSE))
model <- prog$fit$result$model_mira[[1]]


###################
# worked examples #
###################

sleepstudy  %>% names()

lmerfit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

fixef(lmerfit)
vcov(lmerfit)

# Data frame to evaluate average effects predictions on
newavg <- data.frame(Days = 0:9)
newavg$Reaction <- predict(lmerfit, re.form = NA, newavg)
# Predictors for the varying effect's predictions
newvary <- expand.grid(Days = 0:9, Subject = unique(sleepstudy$Subject))
newvary$Reaction <- predict(lmerfit, newvary)

newvary$Reaction
library(arm)

fitted()