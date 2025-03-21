
# this function loads the models as an R object
compile_models <- function(folder) {
  model <- list()
  files <- sort(list.files(folder, pattern = "^[0-9]{10}\\.rds$", full.names = TRUE))
  cat("\n")
for (i in seq_along(files)) {
  cat("Processing file:", i, " \n")
  temp_model <- readRDS(files[[i]])
  temp_model$mice$le <- temp_model$mice$result$loggedEvents
  temp_model$mice$result <- NULL  # memory saving
  model[[i]] <- temp_model
  }
cat("\n")
return(model)
}
