## LOAD VARIABLES ##
library(tidyverse)
library(UpSetR)

options(max.print = 999999) 

DATA_DIR <- if_else(
  Sys.info()["sysname"] == "Darwin",
  "/Users/jameswilson/Documents/SDTM June 2023",
  "C:/Users/jameswilson/Documents/R projects/SDTM Import"
)

WORKING_DIR <- if_else(
  Sys.info()["sysname"] == "Darwin",
  "/Users/jameswilson/OneDrive/Documents/R/IDDO SDTM analysis",
  "C:/Users/jameswilson/OneDrive/Documents/R/IDDO SDTM analysis"
)

DOMAINS <- c(
  "AU", "CC", "DD", "DM", "DS", "HO", "IN", "LB", "MB", "MP",
  "PE", "PT", "QS", "RP", "RS", "SA", "SC", "TI", "TS", "TV", "VS"
)

# studyid_list <- c(
#   "VAQMOU", "VBNRQO", "VBXLIU", "VCNXEB", "VDXALE", "VEZMZD", "VFEFCS", "VFETIZ",
#   "VFFFOP", "VFSPWD", "VGKSTG", "VHNNMS", "VHTYWO", "VIVXJN", "VIZGFA", "VKRFLS",
#   "VLAULV", "VLBXPMF", "VLEALTT", "VLJYNYC", "VLNAZSK", "VLNXMEA", "VLTTPJC", "VLZUKHR",
#   "VMHKNI", "VOAYGC", "VQBODC", "VQKRHN", "VRBQIF", "VSEVQC", "VSGPDL", "VUFCZW", "VVNGOE",
#   "VWPJRM", "VYDSGR", "VZUYLH"
# )

## REGULAR EXPRESSIONS ##
#ISO8601 <- "^(0[1-9]|1[0-9]|2[0-9]|3[0-1])/(0[1-9]|1[0-2])/(19[8-9][0-9]|200[0-9]|201[0-8])$"

# ## SUMMARISE FUNCTIONS ##
# var_sum <- function(data, var, group_var = NULL) {
#   stopifnot("please ensure variable is numeric format" = is.numeric(data %>% pull({{ var }})))
#   group_var_string <- as.character(substitute(group_var)) 
#   if (is.null(substitute(group_var))) {
#     temp <- data %>%                 # where group_var = NULL
#       summarise(
#         group = NA,
#         min = min({{ var }}, na.rm = TRUE),
#         max = max({{ var }}, na.rm = TRUE),
#         median = median({{ var }}, na.rm = TRUE),
#         mean = mean({{ var }}, na.rm = TRUE),
#         iqr_l = quantile({{ var }}, prob = 0.25, na.rm = TRUE),
#         iqr_u = quantile({{ var }}, prob = 0.75, na.rm = TRUE),
#         sd = sd({{ var }}, na.rm = TRUE),
#         n_total = n(),
#         n_miss = sum(is.na({{ var }}))
#     )
#   } else if (has_name(data, group_var_string)) { # where the grouping variable exists
#     temp <- data %>%
#       group_by({{ group_var }}) %>% 
#       summarise(
#         group = first({{ group_var }}),
#         min = min({{ var }}, na.rm = TRUE),
#         max = max({{ var }}, na.rm = TRUE),
#         median = median({{ var }}, na.rm = TRUE),
#         mean = mean({{ var }}, na.rm = TRUE),
#         iqr_l = quantile({{ var }}, prob = 0.25, na.rm = TRUE),
#         iqr_u = quantile({{ var }}, prob = 0.75, na.rm = TRUE),
#         sd = sd({{ var }}, na.rm = TRUE),
#         n_total = n(),
#         n_miss = sum(is.na({{ var }}))
#       )
#   } else {
#     print(paste("Grouping variable", group_var_string, "does not exist in the dataset."))
#     return(TRUE)
#   }
#   out <- tibble(
#     group = temp$group,
#     n = temp$n_total,
#     n_missing = temp$n_miss,
#     n_missing_pc = 100 * temp$n_miss / temp$n_total,
#     median = temp$median,
#     IQR_L = temp$iqr_l,
#     IQR_U = temp$iqr_u, 
#     min = temp$min,
#     max = temp$max,
#     mean = temp$mean,
#     sd = temp$sd
#   ) %>% arrange(n)
# return(out)
# }

## LOAD DOMAINS ##

load_domains <- function(studyid) {
  wd <- DATA_DIR
  datasets <- dir(wd) %>%
    str_subset(paste0(studyid, ".RData$"))
  var_names <- str_replace(datasets, "\\.RData$", "")

  for (i in seq_along(datasets)) {
    load(paste0(wd, "/", datasets[i]))
  }

  for (var_name in var_names) {
    domain <- str_extract(var_name, "^[A-Z]{2}")
    assign(domain, as_tibble(get(var_name)), envir = .GlobalEnv)
    print(var_name)
  }
  rm(list = var_names)
}

## LOAD DOMAIN MISSINGNESS DATASET ##

# # I'm aware this is poor R programming, but it works
# ld_missingness <- function(studyid) {
#   datasets <- dir(wd) %>%
#     str_subset(paste0(studyid, ".RData$"))
#   var_names <- str_replace(datasets, "\\.RData$", "")

#   for (i in seq_along(datasets)) {
#     load(paste0(wd, "/", datasets[i]))
#   }

#   tibble <- tibble(USUBJID = character()) # create an empty tibble for joining
#   for (var_name in var_names) {
#     domain <- str_extract(var_name, "^[A-Z]{2}")
#     if (domain %in% c("TV", "TS", "TI")) {    # These domains do not have USUBJID columns
#       next  
#     } 
#     assign(
#       domain, 
#       tibble(
#         USUBJID = unique(get(var_name)$USUBJID),
#         !!domain := TRUE
#       ),
#       )
#     tibble <- full_join(
#       tibble, 
#       get(domain), 
#       join_by(USUBJID == USUBJID))
#   }

#   rm(list = var_names)
#   tibble <- tibble %>%  
#     mutate(
#         across(where(is.logical),
#         ~ ifelse(is.na(.), FALSE, .)))
#   return(tibble)
# }


## tabulate and View all columns except USUBJID

# count_na <- function(df) {
#   df1 <- df %>% mutate(
#     across(everything(), ~ifelse(is.na(.x), FALSE, TRUE))
#   )
#   df2 <- df1 %>% count(across(everything()))
#   return(df2)
# }

# create a new variable which enumerates {count_col} within the group specified by [...]
# # The total number of rows in dataset is preserved (uses mutate and row_number())
# count_dup1 <- function(df, ..., count_col = USUBJID) {
#   out <- df %>% 
#     group_by({{ count_col }}, ...) %>% 
#     mutate(n_dup = row_number()) %>% 
#     ungroup() %>% 
#     count(..., n_dup, name = "n_by_row")
#   return(out)
# }

# # create a new variable which counts the number of duplicates of {count_col} within the group specified by [...]
# # The total number of rows in dataset is not preserved (uses summarise and n())
# # default value for count_col is USUBJID for CDISC work
# count_dup2 <- function(df, ..., count_col = USUBJID) {
#   out <- df %>% 
#     group_by({{ count_col }}, ...) %>% 
#     summarise(num = n()) %>% 
#     ungroup() %>% 
#     count(..., num, name = "n_by_cc")
#   return(out)
# }

# # combine the above two duplicate functions
# # You can get NAs for n_by_cc, as n_by_row does not always correspond to n_by_cc
# count_dup <- function(df, ..., count_col = USUBJID) {
#   out1 <- df %>% 
#     group_by({{ count_col }}, ...) %>% 
#     summarise(count_col = n()) %>% 
#     ungroup() %>% 
#     count(..., count_col, name = "n_by_cc")
#   out2 <- df %>% 
#     group_by({{ count_col }}, ...) %>% 
#     mutate(count_col = row_number()) %>% 
#     ungroup() %>% 
#     count(..., count_col, name = "n_by_row")
#   out <- out1 %>% full_join(out2)
#   return(out)
# }

# sum_na <- function(df) {
#   out <- df %>% summarise(
#     across(
#       everything(), 
#       ~sum(!is.na(.x)))
#   ) %>% unlist()
#   names(out) <- names(df)
#   return(out)
# }


# view_lt <- function(df) {
#   df %>% count(across(-USUBJID)) %>% View()
# }

# ## PREPARE LOGICAL DATA (WITH USUBJID) FOR UPSET 
# mod_lt <- function(df) {
#   out <- df %>% 
#     mutate(
#       across(
#         where(is.logical),
#         ~as.integer(.x))
#      ) %>% 
#     as.data.frame()
#   return(out)
# }

# ## Make all columns logical based on missing data

# make_log <- function(df) {
#   out <- df %>% 
#     mutate(across(everything(), ~ifelse(is.na(.x), FALSE, TRUE)))
#   return(out)
# }

# ## MISSINGNESS PLOT ##

# missingness <- function(domain, columns = "all") {
#   stopifnot(
#     "Please enter DOMAIN as character" = is.character(domain),
#     # "Please enter a valid DOMAIN"                = domain %in% domain_list,
#     "Please specify columns as character vector" = is.character(columns),
#     "Please specify columns correctly" = columns %in% colnames(get(domain)) | columns == "all"
#   )

#   if ("all" %in% columns) {
#     cols <- colnames(get(domain))[3:length(colnames(get(domain)))]
#   } else {
#     cols <- columns
#   }

#   # create dataset
#   ds <- get(domain) %>%
#     group_by(STUDYID) %>%
#     summarise(across(all_of(cols), ~ 1 - sum(is.na(.)) / n())) %>%
#     full_join(as_tibble(studyid_list), by = join_by(STUDYID == value)) %>%
#     mutate(STUDYID = factor(STUDYID, levels = rev(studyid_list))) %>%
#     pivot_longer(-STUDYID) %>%
#     mutate(name = as_factor(name))

#   # plot dataset
#   ds %>%
#     ggplot() +
#     geom_bin2d(aes(y = STUDYID, x = name, fill = value),
#       colour = "black"
#     ) +
#     scale_x_discrete(name = "Column") +
#     scale_y_discrete(name = "Study ID") +
#     scale_fill_gradient(
#       name = "% data present",
#       breaks = seq(0, 1, 0.1),
#       low = "#132B43",
#       high = "#56B1F7",
#       space = "Lab",
#       na.value = "grey50",
#       guide = "colourbar",
#       aesthetics = "fill"
#     ) +
#     theme(
#       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#       legend.key.width = unit(6, "lines"), # Adjust the size of the legend key
#       legend.position = "top"
#     ) +
#     labs(caption = "Diamonds = all data missing\nTriangles = all data present") +
#     geom_point(
#       data = ds %>% filter(value == 1),
#       aes(y = STUDYID, x = name),
#       shape = 17,
#       size = 1
#     ) +
#     geom_point(
#       data = ds %>% filter(value == 0),
#       aes(y = STUDYID, x = name),
#       shape = 18,
#       colour = "grey70",
#       size = 2
#     )
# }

# # extract logistic regression mode response estimates
# # better to use profile likelihood confidence intervals
# model_est <- function(model) {

#   vcov          <- sqrt(diag(vcov(model)))
#   conf          <- confint(model) # this is the 95% confidence interval from the profile likelihood 
#   coef          <- model$coefficients
#   z             <- coef / vcov
#   estimate_lci  <- conf[, 1]
#   estimate_uci  <- conf[, 2]
#   response      <- exp(coef)
#   response_uci  <- exp(estimate_uci)
#   response_lci  <- exp(estimate_lci)

#   out <- tibble(
#     name = names(coef),
#     e = coef,
#     e_lci = estimate_lci,
#     e_uci = estimate_uci,
#     r = response,
#     r_lci = response_lci,
#     r_uci = response_uci,
#     p = 2 * pnorm(-abs(z))
#   )
#   out <- out %>% 
#     mutate(
#       sig = case_when(
#         p >= 0.05 & p < 0.10 ~ ".",
#         p >= 0.01 & p < 0.05 ~ "*",
#         p >= 0.001 & p < 0.01 ~ "**",
#         p < 0.001 ~ "***",
#         .default = ""
#       )
#     )
#   return(out)
# }

# or <- function(a, b, c, d) {
#   or <- (a / b) / (c / d)
#   se <- sqrt((1 / a) + (1 / b) + (1 / c) + (1 / d))
#   h <- exp(log(or) + 1.96 * se)
#   l <- exp(log(or) - 1.96 * se)
#   p <- 2 * (1 - pnorm(abs(log(or) / se)))
#   out <- c(or, l, h, p)
#   names(out) <- c("or", "lower_ci", "upper_ci", "p-value")
#   return(out)
# }
