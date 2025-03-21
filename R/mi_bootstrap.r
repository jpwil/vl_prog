# define a function which takes the dataset and bootstraps by either individual patient or by study id
# the function outputs a list of bootstrapped datasets

mi_bootstrap <- function(df, bs_num, cluster) {
  # define helper functions
  mi_bootstrap_ind <- function(df, bs_num) {
  df_boot_ind <- vector("list", bs_num)
  for (i in 1:bs_num) {
    df_boot_ind[[i]] <- df[sample(nrow(df), replace = TRUE), ]
  }
  return(df_boot_ind)
}

  mi_bootstrap_cluster <- function(df, bs_num) {
    df_boot_cluster <- vector("list", bs_num)
    names(df)[names(df) == "STUDYID"] <- "ORIG_STUDYID"

    for (j in 1:bs_num) {
      resample <- sample(seq(1:length(unique(df$ORIG_STUDYID))), replace = TRUE)  # sample the study IDs with replacements
      df_list <- vector("list", length(resample))

      for (i in seq_along(resample)) {
        df_list[[i]] <- df[df$ORIG_STUDYID == resample[i], ]
        df_list[[i]]$STUDYID <- i
        df_list[[i]]$ORIG_STUDYID <- NULL
      }
      df_boot_cluster[[j]] <- do.call(rbind, df_list)
    }
    return(df_boot_cluster)
  }

  # main logic
  if (cluster) {
    out <- mi_bootstrap_cluster(df = df, bs_num = bs_num)
  } else {
    out <- mi_bootstrap_ind(df = df, bs_num = bs_num)
  }
  return(out)
}

