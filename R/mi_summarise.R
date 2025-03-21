#########################
## SUMMARISE MI OBJECT ##
#########################

mi_summarise <- function(data) {
  if (is.null(data$mice$result)) {
    cat("\nMultiple imputation produced an error") 
    return(NULL)
  } else {
    cat("\nMultiple imputation ran without errors") 
  }
  long <- complete(data$mice$result, action = "long", include = FALSE) 
  original <- complete(data$mice$result, 0)

  rows <- nrow(original)
  test <- sum(is.na(long)) # this should be 0! Beware - sometimes NAs are propogated without errors
  missingness <- original %>% summarise(across(everything(), ~100 * sum(is.na(.x)) / rows)) 
  missingness_print <- tibble(
    name = colnames(missingness),
    value = as_vector(missingness)
    ) %>% arrange(value)

temp <- original %>% dplyr::select(-STUDYID)
missing_prop <- sum(is.na(temp)) / (nrow(temp) * length(temp))

cat("\n** ORIGINAL DATASET **\n\n")
cat("Number of rows:", rows, "\n")

prop <- sprintf("%.1f", 100 * missing_prop)
cat("Total missingness % (excluding STUDYID): ", prop, "%\n\n", sep = "")

header <- sprintf("%-21s %9s", "Variable", "% missing")
cat(header, "\n")
cat(rep("-", 32), "\n", sep = "")  # Print a separator line

# Iterate through the vectors and print the rows
for (i in 1:nrow(missingness_print)) {
  line <- sprintf("%-21s %9.1f", missingness_print$name[i], missingness_print$value[i])
  cat(line, "\n")
}

cat("\n** IMPUTED DATASETS **\n\n")
cat("Imputations:", data$mice$result$m, "\n")
cat("Iterations:", data$mice$result$iter, "\n")
cat("Date/time started:", as.character(data$mice$time$time_start), "\n")
cat("Date/time ended:", as.character(data$mice$time$time_end), "\n")
cat("Elapsed time (hours)", data$mice$time$time_start - data$mice$time$time_end, "\n\n")
cat("Number of missing values imputed:", sum(is.na(long)), "\n")
cat("Number of warnings:", nrow(data$mice$warnings), "\n")
}
