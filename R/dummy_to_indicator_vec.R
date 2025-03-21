# cleans a character vector of model terms by removing the intercept term and 
# converting dummy variables to indicator variables where appropriate,
# and sorting alphabetically to aid quick comparison

dummy_to_indicator <- function(vec) {
  vec1 <- vec[vec != "(Intercept)"] 

  if (any(grepl("^TREAT_GRP4", vec1))) {
    vec1 <- c(vec1[!grepl("^TREAT_GRP4", vec1)], "TREAT_GRP4")
  }

  if (any(grepl("^ZZ_MAL", vec1))) {
    vec1 <- c(vec1[!grepl("^ZZ_MAL", vec1)], "ZZ_MAL")
  }

  return(sort(vec1))
}
