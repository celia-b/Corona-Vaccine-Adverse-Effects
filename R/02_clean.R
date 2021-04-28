# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients <- read_csv(file = "data/01_patients.csv", 
                     col_types = cols("BIRTH_DEFECT" = col_character(),
                                      "X_STAY" = col_character(),
                                      "RPT_DATE" = col_date(format="%Y-%m-%d"),
                                      "V_FUNDBY" = col_character(),
                                      "ER_VISIT" = col_character()))
symptoms <- read_csv(file = "data/01_symptoms.csv")
vaccines <- read_csv(file = "data/01_vaccines.csv")


# Wrangle data ------------------------------------------------------------

## PATIENTS
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
            SPLTTYPE)) %>% # Removed columns
  replace_na(list(DIED = "N",
                  L_THREAT = "N",
                  HOSPITAL = "N",
                  DISABLE = "N",
                  OFC_VISIT = "N",
                  ER_ED_VISIT = "N")) %>% # Handled NAs that are actually "No"
  mutate(AGE_YRS = as.integer(AGE_YRS)) # Age to integers



## VACCINES
vaccines <- vaccines_raw %>% 
  filter (VAX_TYPE == "COVID19") # Keep only COVID vaccines



# Trying to see if there are repeated vaccine IDs, some are repeated in 
# vaccines, but in patients they are all unique which is weird bc patients
# has more rows
vaccines %>% count (VAERS_ID, sort = TRUE)
patients %>% count (VAERS_ID, sort = TRUE)
id_groups <- vaccines %>% group_by(VAERS_ID) %>% summarise(n = n())
lot_groups <- vaccines %>% group_by(VAX_LOT) %>% summarise(n = n())


patients %>% filter (SEX == "U") %>% count()
# 898 patients have sex = "U" - should we delete?
# According to VAERS it should be blank:
# Sex (SEX):Sex of the vaccine recipient (M = Male, F = Female, Unknown = Blank).


# Write data --------------------------------------------------------------
write_csv(x = patients_clean,
          file = "data/02_patients_clean.csv")
