# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
# Misinterpreted column types are manually asigned

## 1. Patients ------------------------------------------------------------
patients <- read_csv(file = gzfile("data/01_patients.csv.gz"), 
                     col_types = cols("BIRTH_DEFECT" = col_character(),
                                      "X_STAY" = col_character(),
                                      "RPT_DATE" = col_date(format = "%Y-%m-%d"),
                                      "V_FUNDBY" = col_character(),
                                      "ER_VISIT" = col_character()))

## 2. Symptoms -----------------------------------------------------------
symptoms <- read_csv(file = gzfile("data/01_symptoms.csv.gz"))

## 3. Vaccines -----------------------------------------------------------
vaccines <- read_csv(file = gzfile("data/01_vaccines.csv.gz"),
                     col_types = cols("VAX_DOSE_SERIES" = col_character()))


# Wrangle data ------------------------------------------------------------

## 1. Patients ------------------------------------------------------------

# Remove dirty and unwanted columns. 
# Replace NAs that are actually 'no' with "N". 
# Replace no-like strings that are actually NA with NA. 

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
            L_THREAT)) %>% 
  replace_na(list(DIED = "N",
                  HOSPITAL = "N",
                  DISABLE = "N",
                  OFC_VISIT = "N",
                  ER_ED_VISIT = "N")) %>%
  mutate(ALLERGIES = case_when(grepl("^no\\.$|^no|^none|^not|^non|^-$|^\\?$", 
                                   ALLERGIES, 
                                   ignore.case = TRUE) ~ NA_character_,
                               is.na(ALLERGIES) ~ NA_character_,
                               TRUE ~ ALLERGIES)) %>%
  mutate(CUR_ILL = case_when(grepl("^non-serological|^Non-Hodgkin|^Non Hodgkin|
                                   ^non-alcoholic|^non systemic", 
                                   CUR_ILL, 
                                   ignore.case = TRUE) ~ CUR_ILL,
                             grepl("^no\\.$|^no|^none|^not|^non|none$|^zero$|
                                   ^0$|^-$|^\\?$", 
                                   CUR_ILL, 
                                   ignore.case = TRUE) ~ NA_character_,
                             is.na(CUR_ILL) ~ NA_character_,
                             TRUE ~ CUR_ILL))


## 2. Symptoms ---------------------------------------------------------------
# Remove columns containing symptom versions

symptoms_clean <- symptoms %>%
  select(-c(SYMPTOMVERSION1, 
            SYMPTOMVERSION2, 
            SYMPTOMVERSION3, 
            SYMPTOMVERSION4, 
            SYMPTOMVERSION5))


## 3. Vaccines ---------------------------------------------------------------
# Filter for only COVID19 vaccines.
# Remove duplicated rows.
# Remove duplicated IDs.
# Change column name for consistency.
# Remove dirty and redundant columns. 

vaccines_clean <- vaccines %>%
  filter(VAX_TYPE == "COVID19") %>%
  distinct() %>%
  add_count(VAERS_ID) %>% 
  filter(n == 1) %>% 
  select(-n) %>%
  filter(VAX_MANU != "UNKNOWN MANUFACTURER") %>% 
  mutate(VAX_MANU = recode(VAX_MANU, 
                           "PFIZER\\BIONTECH" = "PFIZER-BIONTECH")) %>% 
  select(-c(VAX_NAME, 
            VAX_LOT))
  


# Write data --------------------------------------------------------------
## 1. Patients ------------------------------------------------------------
write_csv(x = patients_clean,
          file = "data/02_patients_clean.csv.gz")

## 2. Symptoms ------------------------------------------------------------
write_csv(x = symptoms_clean,
          file = "data/02_symptoms_clean.csv.gz")

## 3. Vaccines ------------------------------------------------------------
write_csv(x = vaccines_clean,
          file = "data/02_vaccines_clean.csv.gz")



