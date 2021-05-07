# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients_raw <- read_csv(file = "data/_raw/2021VAERSDATA.csv", 
                         col_types = cols("BIRTH_DEFECT" = col_character(),
                                          "X_STAY" = col_character(),
                                          "RPT_DATE" = col_date(format="%m/%d/%Y"),
                                          "V_FUNDBY" = col_character(),
                                          "ER_VISIT" = col_character(),
                                          "HOSPDAYS" = col_character()),
                         na = c("", " ", 
                                "NA", "N/A", "na", "Na", "n/a", "N/a", 
                                "None", "none", "None.", "NONE",
                                "unknown", "Unknown", "UNKNOWN", "U",
                                "NO KNOWN", "no known", "No known", "No Known", 
                                "None known", "none known", "NONE KNOWN", "None Known", 
                                "None reported", "Not applicable",
                                "No", "NO", "no")) # There is also "no", but that might interfere with certain columns where we do want yes/no --> change them to Y/N


symptoms_raw <- read_csv(file = "data/_raw/2021VAERSSYMPTOMS.csv")

vaccines_raw <- read_csv(file = "data/_raw/2021VAERSVAX.csv",
                         col_types = cols("VAX_DOSE_SERIES" = col_character()),
                         na = c("UNK", "N/A"))

vaccines_raw %>% view()

# Wrangle data ------------------------------------------------------------
 

# Write data --------------------------------------------------------------
write_csv(x = patients_raw,
          file = "data/01_patients.csv.gz")

write_csv(x = symptoms_raw,
          file = "data/01_symptoms.csv.gz")

write_csv(x = vaccines_raw,
           file = "data/01_vaccines.csv.gz")



