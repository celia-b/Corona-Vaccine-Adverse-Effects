# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients_clean <- read_csv(file = "data/02_patients_clean.csv", 
                           col_types = cols(VAX_DATE = col_date(format = "%m/%d/%Y"),
                                            DATEDIED = col_date(format = "%m/%d/%Y")))
symptoms_clean <- read_csv(file = "data/02_symptoms_clean.csv")

# Wrangle data ------------------------------------------------------------

################################### PATIENTS ###################################
patients_clean_aug <- patients_clean %>%
  mutate(HAS_ALLERGIES = case_when(grepl("^no.?$|^no |^none|^not|^non", 
                                         ALLERGIES, 
                                         ignore.case = TRUE) ~ 'N',
                                 is.na(ALLERGIES) ~ 'N',
                                 TRUE ~ 'Y')) %>% # New column: patient has allergies - more symptoms/death rate?
  select(-ALLERGIES) %>% # Remove old dirty column
  mutate(HAS_ILLNESS = case_when(grepl("^non-serological|^Non-Hodgkin|^Non Hodgkin|^non-alcoholic|^non systemic", 
                                       CUR_ILL, 
                                       ignore.case = TRUE) ~ 'Y',
                                 grepl("^no.?$|^no |^none|^not|^non", 
                                       CUR_ILL, 
                                       ignore.case = TRUE) ~ 'N',
                                 is.na(CUR_ILL) ~ 'N',
                                 TRUE ~ 'Y')) %>% # New column: currently has illness - more symptoms/death rate?
  mutate(HAS_COVID = case_when(grepl("covid",
                                     CUR_ILL,
                                     ignore.case = TRUE) ~ 'Y',
                               TRUE ~ 'N')) %>% # New column: has Covid-19 at time of vaccination - more symptoms/death rate?
  mutate(HAD_COVID = case_when(grepl("covid",
                                     HISTORY,
                                     ignore.case = TRUE) ~ 'Y',
                               TRUE ~ 'N')) %>% # New column: had Covid-19 in the past - less symptoms?
  select(-c(CUR_ILL, HISTORY)) %>% # Remove old dirty columns
  mutate(PRIOR_ADVERSE = case_when(is.na(PRIOR_VAX) ~ 'N',
                                   TRUE ~ 'Y')) %>% # New column: has had adverse reaction to other vaccines - more symptoms/death rate?
  select(-PRIOR_VAX) %>% #Remove old dirty column
  mutate(TAKES_ANTIINFLAMATORY = case_when(grepl("ibuprofen|aspirin|celecoxib|diclofenac|diflunisal|etodolac|indomethacin", 
                                                 OTHER_MEDS, 
                                                 ignore.case = TRUE) ~ 'Y',
                                           TRUE ~ 'N')) %>% # New column: takes anti-inflamatory meds - less symptoms?
  mutate(TAKES_STEROIDS = case_when(grepl("steroid|betamethasone|prednisolone|dexamethasone|hydrocortisone", 
                                          OTHER_MEDS, 
                                          ignore.case = TRUE) ~ 'Y',
                                    TRUE ~ 'N')) %>% # New column: takes steroid meds - something?
  select(-OTHER_MEDS) %>% # Remove old dirty column
  mutate(AGE_CLASS = case_when(AGE_YRS < 10 ~ '[0,10)',
                               AGE_YRS >= 10 & AGE_YRS < 20 ~ '[10,20)',
                               AGE_YRS >= 20 & AGE_YRS < 30 ~ '[20,30)',
                               AGE_YRS >= 30 & AGE_YRS < 40 ~ '[30,40)',
                               AGE_YRS >= 40 & AGE_YRS < 50 ~ '[40,50)',
                               AGE_YRS >= 50 & AGE_YRS < 60 ~ '[50,60)',
                               AGE_YRS >= 60 & AGE_YRS < 70 ~ '[60,70)',
                               AGE_YRS >= 70 & AGE_YRS < 80 ~ '[70,80)',
                               AGE_YRS >= 80 & AGE_YRS < 90 ~ '[80,90)',
                               AGE_YRS >= 90 ~ '[90,120)')) %>% # New column: age group - for plotting
  mutate(DIED_AFTER = DATEDIED - VAX_DATE) %>% # New column: how long after taking the vaccine the subject died --> DIRTY FORMAT: figure out how to change it
  rename(SYMPTOMS_AFTER = NUMDAYS) %>% # Renamed column: how long after taking the vaccine the symptoms appeared
  select(-c(VAX_DATE, DATEDIED, ONSET_DATE, TODAYS_DATE)) # Remove old dirty columns
  


################################## SYMPTOMS ##################################
# Extract the 20 symptoms that most commonly occur
top_20_vec <- symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom_n",
               values_to = "symptom",
               values_drop_na = TRUE) %>% # get all symptoms into one column
  select(VAERS_ID, symptom) %>%
  group_by(symptom) %>%
  count(sort = TRUE) %>% # count symptom occurrence, sort by highest occurrence
  head(20) %>%
  pull(symptom) # convert symptoms column from tibble into vector

# Filter for individuals that have a least one of the top 20 symptoms. 
# Make tibble with columns VAERS_ID and each of the top 20 symptoms. 
# Fill tibble with TRUE/FALSE depending on whether the individual has symptom.  
top_20_symptoms <- symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID) %>% # get all symptoms into one column
  filter(value %in% top_20_vec) %>% # Filter out IDs with any of the top 20 symptoms  
  mutate(name = TRUE) %>% # create column with values TRUE
  drop_na(value) %>% 
  pivot_wider(id_cols = VAERS_ID,
              names_from = value,
              values_from = name,
              values_fill = FALSE) # convert symptoms into column names and TRUE into values.
  # Give symptom value FALSE if empty

# Reintroduce individuals with none of the top 20 symptoms which were filtered out above. 
# The result is a tibble containing all IDs and symptom columns with TRUE/FALSE
symptoms_all_IDs <- symptoms_clean %>% 
  select(VAERS_ID) %>%
  distinct(VAERS_ID) %>% # remove repeated IDs
  full_join(., 
            top_20_symptoms) %>% # join tibble with all IDs with symptoms tibble
  replace(., 
          is.na(.), 
          FALSE) # convert NAs to FALSE

# Make new column containing total number of symptoms each individual has.
# Join this column with tibble containing symptom columns. 
# The final tibble contains IDs, total number of symptoms and top 20 symptoms. 
symptoms_clean_aug <- symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom num",
               values_to = "symptom",
               values_drop_na = TRUE) %>% # get all symptoms into one column
  select(VAERS_ID, 
         symptom) %>%
  group_by(VAERS_ID) %>%
  count(sort = FALSE) %>% # count number of symptoms per ID
  rename(n_symptoms = n) %>%
  full_join(., 
            symptoms_all_IDs) %>% # join tibble with all IDs 
  setNames(gsub(" ", "_", names(.))) %>% # replace spaces with _ in column names
  setNames(toupper(names(.))) %>%
  view()



# Write data --------------------------------------------------------------
write_csv(x = patients_clean_aug,
          file = "data/03_patients_clean_aug.csv")

write_csv(x = symptoms_clean_aug,
          file = "data/03_symptoms_clean_aug.csv")


