# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
vaccines <- read_csv(file = "data/01_vaccines.csv")
patients <- read_csv(file = "data/01_patients.csv")


# Wrangle data ------------------------------------------------------------
#my_data_clean <- my_data # %>% ...

# Trying to see if there are repeated vaccine IDs, some are repeated in 
# vaccines, but in patients they are all unique which is weird bc patients
# has more rows
vaccines %>% count (VAERS_ID, sort = TRUE)
patients %>% count (VAERS_ID, sort = TRUE)


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")