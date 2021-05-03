# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

patients <- read_csv(file = "data/01_patients.csv", 
                     col_types = cols("BIRTH_DEFECT" = col_character(),
                                      "X_STAY" = col_character(),
                                      "RPT_DATE" = col_date(format="%Y-%m-%d"),
                                      "V_FUNDBY" = col_character(),
                                      "ER_VISIT" = col_character()))

symptoms <- read_csv(file = "data/01_symptoms.csv")

vaccines <- read_csv(file = "data/01_vaccines.csv",
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
            RECVDATE)) %>% # Removed columns
  replace_na(list(DIED = "N",
                  L_THREAT = "N",
                  HOSPITAL = "N",
                  DISABLE = "N",
                  OFC_VISIT = "N",
                  ER_ED_VISIT = "N")) %>% # Handled NAs that are actually "No"
  mutate(AGE_YRS = as.integer(AGE_YRS)) %>% # Age to integers
  filter(SEX != "U") # remove persons with unkwnon sex



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
  select(-c(VAX_NAME)) # Redundant column
  


## This is all exploration - I don't know if we should remove it
'''
sum(duplicated(vaccines)) # 30 duplicated rows in dataframe
# There should not be any rows with the duplicates of VAERS_ID and VAX_LOT...
vaccines %>% 
  distinct(VAERS_ID, VAX_LOT) 
  
vaccines %>% distinct(VAX_ROUTE) 
# All values OK

vaccines %>% distinct(VAX_SITE)
# All values OK

pattern <- "COVID19\\s\\(COVID19\\s\\((\\w++\\-*\\w*)\\)"# Regular expression matching the name of the vaccine manufacturer
vaccines <- vaccines %>% mutate(VAX_NAME_extracted = str_match(VAX_NAME, pattern)[,2]) #%>% select(VAX_NAME, VAX_NAME_extracted)
vaccines <- vaccines %>% mutate(comparison = if_else(VAX_MANU == VAX_NAME_extracted, TRUE, FALSE)) 
# Same number of rows:
vaccines %>% count(comparison)
nrow(vaccines)
# VAX_MANU and VAX_NAME are matching so we can delete the following columns:
# VAX_NAME, VAX_NAME_extracted, comparison
vaccines <- vaccines %>%
  select(-c("VAX_NAME", "VAX_NAME_extracted", "comparison"))
'''

## DEAL WITH THIS AT SOME POINT

# Clean VAX_LOT column



# Write data --------------------------------------------------------------
write_csv(x = patients_clean,
          file = "data/02_patients_clean.csv")

write_csv(x = symptoms_clean,
          file = "data/02_symptoms_clean.csv")

write_csv(x = vaccines_clean,
          file = "data/02_vaccines_clean.csv")



