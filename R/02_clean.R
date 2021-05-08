# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients <- read_csv(file = gzfile("data/01_patients.csv.gz"), 
                     col_types = cols("BIRTH_DEFECT" = col_character(),
                                      "X_STAY" = col_character(),
                                      "RPT_DATE" = col_date(format = "%Y-%m-%d"),
                                      "V_FUNDBY" = col_character(),
                                      "ER_VISIT" = col_character()))

symptoms <- read_csv(file = gzfile("data/01_symptoms.csv.gz"))

vaccines <- read_csv(file = gzfile("data/01_vaccines.csv.gz"),
                     col_types = cols("VAX_DOSE_SERIES" = col_character()))


# Wrangle data ------------------------------------------------------------

################################## PATIENTS ##################################
patients_clean <- patients %>%
  select(-c(CAGE_YR, 
            CAGE_MO,
            RPT_DATE,
            SYMPTOM_TEXT,
            LAB_DATA,
            OFC_VISIT,
            ER_VISIT, 
            X_STAY, 
            V_FUNDBY, 
            BIRTH_DEFECT,
            SPLTTYPE,
            RECVDATE, 
            RECOVD,
            L_THREAT)) %>% # Removed columns
  replace_na(list(DIED = "N",
                  HOSPITAL = "N",
                  DISABLE = "N",
                  OFC_VISIT = "N",
                  ER_ED_VISIT = "N")) %>% # Handled NAs that are actually "No"
  mutate(AGE_YRS = as.integer(AGE_YRS)) # Age to integers



################################## SYMPTOMS ##################################
# Remove symptom versions
symptoms_clean <- symptoms %>%
  select(VAERS_ID, SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5)



################################## VACCINES ##################################
vaccines_clean <- vaccines %>%
  filter (VAX_TYPE == "COVID19") %>% # Keep only COVID vaccines
  distinct () %>% ### Remove duplicates (same values for all variables):
  add_count (VAERS_ID) %>% 
  filter (n==1) %>% # only keep non-repeated IDs (remove duplicated IDs that had different vaccine or lot)
  select (-n) %>% # remove count column ###
  filter(VAX_MANU != "UNKNOWN MANUFACTURER") %>% # Remove rows with unknown vaccine manufacturer
  mutate(VAX_MANU = recode(VAX_MANU, "PFIZER\\BIONTECH" = "PFIZER-BIONTECH")) %>% # Rename PFIZER\\BIONTECH for consistency
  select(-c(VAX_NAME, VAX_LOT)) # Redundant column
  


# Write data --------------------------------------------------------------
write_csv(x = patients_clean,
          file = "data/02_patients_clean.csv.gz")

write_csv(x = symptoms_clean,
          file = "data/02_symptoms_clean.csv.gz")

write_csv(x = vaccines_clean,
          file = "data/02_vaccines_clean.csv.gz")



