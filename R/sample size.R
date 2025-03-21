library(pmsampsize, tidyverse)

# For binary or survival (time-to-event) outcomes, there are three criteria:
#   i) small overfitting defined by an expected shrinkage of predictor effects by 10% or less,
#   ii) small absolute difference of 0.05 in the model's apparent and adjusted Nagelkerke's R-squared value, and
#   iii) precise estimation (within +/- 0.05) of the average outcome risk in the population for a key timepoint of interest for prediction.

# run the interim analysis to get this variable
demo_out3 %>% ungroup() %>% group_by(COUNTRY) %>% summarise(total = sum(n),
                                      death = sum(death),
                                      relapse = sum(relapse),
                                      studies = n())

# TRYPANOSOMIASIS 1 
E <- 32
n <- 907

# TRYPANOSOMIASIS 2 
E <- 2030-1067
n <- 2030

# ISC RELAPSE 1
E <- 263 + 30
n <- 5422 + 502

# ISC RELAPSE 2
E <- 225
n <- 4421

# EAST AFRICA RELAPSE
E <- 61 + 6 + 20 + 1
n <- 1182 + 288 + 1246 + 213

# ISC MORTALITY
E <- 37
n <- 5422 + 502


# EAST AFRICA MORTALITY
E <- 146
n <- 1182 + 288 + 1246 + 213

# ISC RELAPSE 2
E <- 225
n <- 4421

ll <- E * log(E/n) + (n - E)* log(1 - (E/n))
Rsqr = 0.15 * (1 - exp(2 * ll/n))

pmsampsize(type = "b", nagrsquared = 0.15, parameters = 24, shrinkage = 0.9, 
           prevalence = 225/4421, seed = 123456)

pmsampsize(
  "b", 
  parameters = 2,
  shrinkage = 0.9,
  prevalence = E/n,
  cstatistic = 0.7,
  seed = 123456
)

DD %>% 
  count(EPOCH, DDTEST, DDSTRESC) %>% 
  arrange(EPOCH, DDTEST, DDSTRESC, desc(n))

RS %>% count(RSSEQ, RSSCAT)
