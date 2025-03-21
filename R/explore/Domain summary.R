# onedrive directory: C:\Users\jameswilson\OneDrive\Documents\R\IDDO SDTM analysis

library(tidyverse)
source("R/definitions.R")
load(paste0(wd, "/","domainsR.RData"))

# this script summarises domains/variables for each stuty using 2d heatmaps
# should only need to run this once for each data extraction

all_domains <- ls() %>% str_extract("^[:upper:]{2}$") %>% na.omit()

study_domain <- tibble(
  STUDYID = character(),
  DOMAIN = character()) 

# just extract domains and study id numbers
for (i in 1:length(all_domains)) {
  stopifnot("STUDYID" %in% colnames(get(all_domains[i])),
            "DOMAIN" %in% colnames(get(all_domains[i])))
  study_domain <- bind_rows(study_domain, get(all_domains[i]) %>% select(c("STUDYID","DOMAIN")))
}

# looks at unique studies and domains
study_domain %>% count(STUDYID) %>% print(n = Inf)
study_domain %>% count(DOMAIN) %>% print(n = Inf)

############################################
## DISPLAY DOMAINS AVAILABLE PER STUDY ID ##
############################################

# display graphically the domains present
levels_domain <- study_domain %>% count(DOMAIN) %>% pull(1)
sd_plot <- study_domain %>% count(STUDYID, DOMAIN) %>% 
  select(-n) %>% 
  arrange(desc(STUDYID)) %>% 
  mutate(STUDYID = as_factor(STUDYID),
         DOMAIN = factor(DOMAIN, levels = levels_domain),
         PRESENT = TRUE) %>% 
  complete(STUDYID, DOMAIN,
           fill = list(PRESENT = FALSE))

sd_plot %>% ggplot() + 
  geom_bin2d(aes(y = STUDYID, x = DOMAIN, fill = PRESENT),
             colour = "black") +
  scale_fill_manual(name = "Domain present?",
                    values = c("palegreen", "lightpink2"),
                    labels = c("True", "False"),
                    breaks = c(TRUE, FALSE)) + 
  scale_x_discrete(name = "Domain") + 
  scale_y_discrete(name = "IDDO Study ID") +
  ggtitle("IDDO implementation of CDISC domains available by IDDO study ID")

setwd(pd)
ggsave("Domains.png",
       plot = last_plot(),
       scale = 3,
       device = "png")

###################################################################
## DISPLAY COLUMN HEADINGS AVAILABLE BY DOMAIN FOR EACH STUDY ID ##
###################################################################

# try IN domain first

# AU	Audiometry Test Results	                       Findings Observation Class
# CC	Contributor-Coded Clinical and Adverse Events	 Events Observation Class
# DD	Death Details	                                 Events Observation Class
# *DM	Demographics	                                 Special Purpose
# DS	Disposition	                                   Events Observation Class
# HO	Healthcare Encounters	                         Events Observation Class
# *IN	Treatments and Interventions	                 Interventions Observation Class
# *LB	Laboratory Test Results	                       Findings Observation Class
# *MB	Microbiology Specimen	                         Findings Observation Class
# *MP	Morphology and Physiology	                     Findings Observation Class
# PE	Physical Examination	                         Findings Observation Class
# PT	Per-Protocol Treatments and Interventions	     Interventions Observation Class
# QS	Questionnaires	                               Findings Observation Class
# *RP	Reproductive System Findings	                 Findings Observation Class
# RS	Disease Response and Clinical Classification	 Findings Observation Class
# *SA	Clinical and Adverse Events	                   Events Observation Class
# SC	Subject Characteristics	                       Findings Observation Class
# TI	Trial Inclusion Exclusion Criteria	           Trial Design
# *TS	Trial Summary	                                 Trial Design
# *TV	Trial Visits	                                 Trial Design
# *VS	Vital Signs	                                   Findings Observation Class

class <- c(1,2,2,3,2,2,4,1,1,1,1,4,1,1,1,2,1,5,5,5,1)
class_labels <- c("Findings Observation Class",
                  "Events Observation Class",
                  "Special Purpose",
                  "Interventions Observation Class",
                  "Trial Design")
class_factor <- factor(class, levels = 1:length(class_labels), labels = class_labels)

domain_info <- tibble(
  domain = levels_domain,
  desc = c("Audiometry Test Results",
           "Contributor-Coded Clinical and Adverse Events",
           "Death Details",
           "*Demographics",
           "Disposition",
           "Healthcare Encounters",
           "*Treatments and Interventions",
           "*Laboratory Test Results",
           "*Microbiology Specimen",
           "*Morphology and Physiology",
           "Physical Examination",
           "Per-Protocol Treatments and Interventions",
           "Questionnaires",
           "*Reproductive System Findings",
           "Disease Response and Clinical Classification",
           "*Clinical and Adverse Events",
           "Subject Characteristics",
           "Trial Inclusion Exclusion Criteria",
           "*Trial Summary",
           "*Trial Visits",
           "*Vital Signs"),
  class = class_factor
)

# save the plots to display the available domains columns by study
# separate plot for each study

levels_study <- study_domain %>% count(STUDYID) %>% 
  select(-n)

for (i in 1:dim(domain_info)[1]) {
  print(i)
  col_order <- colnames(get(domain_info$domain[i]))
  domain <- get(domain_info$domain[i]) %>% group_by(STUDYID) %>% 
  summarise(across(everything(), ~all(is.na(.)))) %>% 
  mutate(across(-STUDYID, as.numeric)) %>% 
  right_join(levels_study) %>% 
  mutate(across(-STUDYID, ~ifelse(is.na(.), 2,.))) %>% 
  pivot_longer(cols = -c(STUDYID)) %>% 
  mutate(STUDYID = factor(STUDYID, levels = levels_study %>% pull() %>% rev()),
         name = factor(name, levels = col_order))

domain %>%   
  ggplot() + 
  geom_bin2d(aes(y = STUDYID, x = name, fill = factor(value)),
             colour = "black") +
  scale_fill_manual(name = "Column present?",
                    values = c("blue", "orange", "darkgrey"),
                    labels = c("Yes", "Column Missing", "Domain Missing")) + 
  scale_x_discrete(name = "Column") + 
  scale_y_discrete(name = "Study ID") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ggtitle(paste0(domain_info$domain[i], ", ", domain_info$desc[i],
          " (", domain_info$class[i], ")"))

ggsave(paste0(domain_info$domain[i], ".png"),
       plot = last_plot(),
       scale = 3,
       device = "png",
       path = "domains")
}
