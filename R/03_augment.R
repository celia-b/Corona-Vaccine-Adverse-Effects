# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients_clean <- read_csv(file = "data/02_patients_clean.csv")


# Wrangle data ------------------------------------------------------------
patients_clean_aug <- patients_clean %>%
  mutate(HAS_ALLERGIES = case_when(grepl("^no.?$|^no |^none|^not|^non", ALLERGIES, ignore.case = TRUE) ~ 'N',
                                 is.na(ALLERGIES) ~ 'N',
                                 TRUE ~ 'Y')) %>% # New column: patient has allergies
  mutate(HAS_ILLNESS = case_when(grepl("^non-serological|^Non-Hodgkin|^Non Hodgkin|^non-alcoholic|^non systemic", CUR_ILL, ignore.case = TRUE) ~ 'Y',
                                 grepl("^no.?$|^no |^none|^not|^non", CUR_ILL, ignore.case = TRUE) ~ 'N',
                                 is.na(CUR_ILL) ~ 'N',
                                 TRUE ~ 'Y')) %>% # New column: currently has illness 
  mutate(COVID_POSITIVE = case_when(grepl("covid", CUR_ILL, ignore.case = TRUE) ~ 'Y',
                                    grepl("covid", HISTORY, ignore.case = TRUE) ~ 'Y',
                                    TRUE ~ 'N')) # New column: has or had Covid-19

# Write data --------------------------------------------------------------
write_csv(x = patients_clean_aug,
          file = "data/03_patients_clean_aug.csv")
