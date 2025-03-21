# this function takes the 'folder' argument and explores the meta data (number of files and meta.rds)

explore_meta <- function(folder) {
  stopifnot("folder must contain meta.rds" = "meta.rds" %in% list.files(paste0(folder), full.names = FALSE)) 

  temp <- readRDS(file = paste0(folder, "/meta.rds"))$arguments
  cat("\n", rep(c("=", ""), length.out = 100), "\n\n", sep = "")
  cat("Model summary: ")
  cat("\"", folder, ".rds\"", "\n\n", sep = "")

  cat(sprintf("%-12s %-5s\n", "Time started", temp$time_now), "\n")
  cat(sprintf("%-12s %-5s\n", "Imputations", temp$imputations))
  cat(sprintf("%-12s %-5s\n", "Iterations", temp$iterations))
  cat(sprintf("%-12s %-5s\n", "Bootstraps", temp$bootstraps))
  cat(sprintf("%-12s %-5s\n", "Files", length(list.files(folder, pattern = "^[0-9]{10}\\.rds$"))))
  cat(sprintf("%-12s %-5s\n", "p-value", temp$'p-value'), "\n")
  cat(sprintf("%-12s %-5s\n", "Cluster", temp$cluster), "\n")
  
  for (i in seq_along(temp$keep)) {
    cat(sprintf("%-12s %-5s\n", "Keep", temp$keep[i]))
  }
  for (i in seq_along(temp[[8]])) {
    cat(sprintf("%-12s %-5s\n", "Pred", temp[[8]][i]))
  }
}
