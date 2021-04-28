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
vaccines <- read_csv(file = "data/01_vaccines.csv")
<<<<<<< HEAD
patients <- read_csv(file = "data/01_patients.csv")
symptoms <- read_csv(file = "data/01_symptoms.csv")
=======
>>>>>>> 7fc9c8c0bd31c9a28a0b3ebd0e42cd4b32f4060d


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



<<<<<<< HEAD
# Remove symptom versions
symptoms <- symptoms %>%
  select(VAERS_ID, SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5)

# Extract the 20 symptoms that most commonly occur
top_20_vec <- symptoms %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom num",
               values_to = "symptom",
               values_drop_na = TRUE) %>% #get all symptoms into one column
  select(VAERS_ID, symptom) %>%
  group_by(symptom) %>%
  count(sort = TRUE) %>% #count symptom occurrence, sort by highest occurrence
  head(20) %>%
  pull(symptom) #convert symptoms column from tibble into vector

# Filter out individuals that have a least one of the top 20 symptoms. 
# Make tibble with columns VAERS_ID for these individuals and each of the top 20 symptoms. 
# Fill tibble with TRUE/FALSE depending on whether the individual has symptom.  
top_20_symptoms <- symptoms %>%
  pivot_longer(cols = -VAERS_ID) %>% #get all symptoms into one column
  filter(value %in% top_20_vec) %>% # Filter out IDs with any of the top 20 symptoms  
  mutate(name = TRUE) %>% #create column with values TRUE
  drop_na(value) %>% 
  pivot_wider(id_cols = VAERS_ID,
              names_from = value,
              values_from = name,
              values_fill = FALSE) #convert symptoms into column names and TRUE into values
                                   #and give symptom value FALSE if empty

# Reintroduce individuals with none of the top 20 symptoms which were filtered out above
symptoms_all_IDs <- symptoms %>% 
  select(VAERS_ID) %>%
  full_join(., 
            top_20_symptoms) %>% #join tibble with all IDs
  replace(., 
          is.na(.), 
          FALSE) %>% #convert NAs to FALSE
  view()

# Make new column containing total number of symptoms each individual has
symptom_counts <- symptoms %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom num",
               values_to = "symptom",
               values_drop_na = TRUE) %>% #get all symptoms into one column
  select(VAERS_ID, 
         symptom) %>%
  group_by(VAERS_ID) %>%
  count(sort = FALSE) %>%
  rename(n_symptoms = n)

# Join tibble containing total number of symptoms with tibble containing patient symptoms
symptoms_clean <- symptom_counts %>% 
  select(VAERS_ID, 
         n_symptoms) %>%
  full_join(symptoms_all_IDs, 
            .) #join tibble with all IDs


# Write data --------------------------------------------------------------
write_tsv(x = symptoms_clean,
          file = "data/02_symptoms_clean.tsv")

=======
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
>>>>>>> 7fc9c8c0bd31c9a28a0b3ebd0e42cd4b32f4060d
