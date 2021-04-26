# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# When you load 2021VAERSDATA.csv without specifying column types, 
# some problems arise. I found this is because R infers the column
# type from the first 1000 rows, which is bad in cases like the column
# BIRTH_DEFECT, where there is a "Y" in case of "yes" and an NA in case 
# of "no". Most patients don't have birth defects and so the 1000 first 
# rows are all NAs and R assigns the column type to be logical (1/0/T/F/TRUE/FALSE)
# and gets confused when it sees a "Y" and just puts an NA. This means 
# we lose data so we have to fix it by specifying column types in the loading.
# The correct column types can be found in the VAERS user guide:
# https://vaers.hhs.gov/docs/VAERSDataUseGuide_November2020.pdf

patients_raw <- read_csv(file = "data/_raw/2021VAERSDATA.csv", 
                         col_types = cols("BIRTH_DEFECT" = col_character(),
                                          "X_STAY" = col_character(),
                                          "RPT_DATE" = col_date(format="%m/%d/%Y"),
                                          "V_FUNDBY" = col_character(),
                                          "ER_VISIT" = col_character()),
                         na = c("", " ", 
                                "NA", "N/A", "na", "Na", "n/a", "N/a", 
                                "None", "none", "None.", "NONE",
                                "unknown", "Unknown", "UNKNOWN", "NO KNOWN", "None reported", "Not applicable", "None known")) # There is also "no", but that might interfere with certain columns where we do want yes/no --> change them to Y/N

symptoms_raw <- read_csv(file = "data/_raw/2021VAERSSYMPTOMS.csv")
vaccines_raw <- read_csv(file = "data/_raw/2021VAERSVAX.csv")

# Wrangle data ------------------------------------------------------------

# Looking better into the data, there are not that many columns with no data.
# Most N/As mean "no" or something similar. This we will have to deal with.

# Removed columns:
# CAGE_YR and CAGE_MO (calculated patient age in years and month) 
#   -> empty - we have AGE_YRS instead
# There are some columns that seem useless like RPT_DATE (date of 
# completion of the form), but they might be useful for solving
# row duplications, so let's not remove them yet.

patients <- patients_raw %>%
  select(-c("CAGE_YR", "CAGE_MO"))

symptoms <- symptoms_raw

vaccines <- vaccines_raw

data <- patients %>%
  full_join(., symptoms, by = "VAERS_ID") %>%
  full_join(., vaccines, by = "VAERS_ID")


# Write data --------------------------------------------------------------
write_csv(x = patients,
          file = "data/01_patients.csv")
