# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients_clean <- read_csv(file = gzfile("data/02_patients_clean.csv.gz"), 
                           col_types = cols(VAX_DATE = col_date(format = "%m/%d/%Y"),
                                            DATEDIED = col_date(format = "%m/%d/%Y")))

symptoms_clean <- read_csv(file = gzfile("data/02_symptoms_clean.csv.gz"))

vaccines_clean <- read_csv(file = gzfile("data/02_vaccines_clean.csv.gz"),
                           col_types = cols(VAX_DOSE_SERIES = col_character()))

# Wrangle data ------------------------------------------------------------

################################### PATIENTS ###################################
patients_clean_aug <- patients_clean %>%
  mutate(HAS_ALLERGIES = case_when(grepl("^no.?$|^no |^none|^not|^non", 
                                         ALLERGIES, 
                                         ignore.case = TRUE) ~ 'N',
                                 is.na(ALLERGIES) ~ 'N',
                                 TRUE ~ 'Y')) %>% 
  select(-ALLERGIES) %>% 
  mutate(HAS_ILLNESS = case_when(grepl("^non-serological|^Non-Hodgkin|^Non Hodgkin|^non-alcoholic|^non systemic", 
                                       CUR_ILL, 
                                       ignore.case = TRUE) ~ 'Y',
                                 grepl("^no.?$|^no |^none|^not|^non", 
                                       CUR_ILL, 
                                       ignore.case = TRUE) ~ 'N',
                                 is.na(CUR_ILL) ~ 'N',
                                 TRUE ~ 'Y')) %>% 
  mutate(HAS_COVID = case_when(grepl("covid",
                                     CUR_ILL,
                                     ignore.case = TRUE) ~ 'Y',
                               TRUE ~ 'N')) %>% 
  mutate(HAD_COVID = case_when(grepl("covid",
                                     HISTORY,
                                     ignore.case = TRUE) ~ 'Y',
                               TRUE ~ 'N')) %>% 
  select(-c(CUR_ILL, HISTORY)) %>% 
  mutate(PRIOR_ADVERSE = case_when(is.na(PRIOR_VAX) ~ 'N',
                                   TRUE ~ 'Y')) %>% 
  select(-PRIOR_VAX) %>% 
  mutate(TAKES_ANTIINFLAMATORY = case_when(grepl("ibuprofen|aspirin|celecoxib|diclofenac|diflunisal|etodolac|indomethacin", 
                                                 OTHER_MEDS, 
                                                 ignore.case = TRUE) ~ 'Y',
                                           TRUE ~ 'N')) %>% 
  mutate(TAKES_STEROIDS = case_when(grepl("steroid|betamethasone|prednisolone|dexamethasone|hydrocortisone", 
                                          OTHER_MEDS, 
                                          ignore.case = TRUE) ~ 'Y',
                                    TRUE ~ 'N')) %>% 
  select(-OTHER_MEDS) %>% 
  mutate(AGE_CLASS = case_when(AGE_YRS < 10 ~ '[0,10)',
                               AGE_YRS >= 10 & AGE_YRS < 20 ~ '[10,20)',
                               AGE_YRS >= 20 & AGE_YRS < 30 ~ '[20,30)',
                               AGE_YRS >= 30 & AGE_YRS < 40 ~ '[30,40)',
                               AGE_YRS >= 40 & AGE_YRS < 50 ~ '[40,50)',
                               AGE_YRS >= 50 & AGE_YRS < 60 ~ '[50,60)',
                               AGE_YRS >= 60 & AGE_YRS < 70 ~ '[60,70)',
                               AGE_YRS >= 70 & AGE_YRS < 80 ~ '[70,80)',
                               AGE_YRS >= 80 & AGE_YRS < 90 ~ '[80,90)',
                               AGE_YRS >= 90 ~ '[90,120)')) %>% 
  mutate(DIED_AFTER = DATEDIED - VAX_DATE) %>% # --> DIRTY FORMAT: figure out how to change it
  rename(SYMPTOMS_AFTER = NUMDAYS) %>% 
  select(-c(VAX_DATE, DATEDIED, ONSET_DATE, TODAYS_DATE)) 
  


################################## SYMPTOMS ##################################
# Extract the 20 symptoms that most commonly occur
top_20_vec <- symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom_n",
               values_to = "symptom",
               values_drop_na = TRUE) %>% # get all symptoms into one column
  count(symptom, sort = TRUE) %>% # count symptom occurrence, sort by highest occurrence
  head(20) %>%
  pluck("symptom") # convert symptoms column from tibble into vector 


# Filter for individuals that have a least one of the top 20 symptoms. 
# Make tibble with columns VAERS_ID and each of the top 20 symptoms. 
# Fill tibble with TRUE/FALSE depending on whether the individual has symptom.  
top_20_symptoms <- symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom_num", 
               values_to = "symptom") %>% # get all symptoms into one column
  filter(symptom %in% top_20_vec) %>% # Filter out IDs with any of the top 20 symptoms  
  mutate(true_col = TRUE) %>% # create column with values TRUE
  drop_na(symptom) %>% 
  pivot_wider(id_cols = VAERS_ID,
              names_from = symptom,
              values_from = true_col,
              values_fill = FALSE) # convert symptoms into column names and TRUE into values.
  # Give symptom value FALSE if empty

# Reintroduce individuals with none of the top 20 symptoms which were filtered out above. 
# The result is a tibble containing all IDs and symptom columns with TRUE/FALSE
symptoms_all_IDs <- symptoms_clean %>% 
  select(VAERS_ID) %>%
  distinct(VAERS_ID) %>% # remove repeated IDs
  full_join(., 
            top_20_symptoms,
            by = "VAERS_ID") %>% # join tibble with all IDs with symptoms tibble
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
            symptoms_all_IDs,
            by = "VAERS_ID") %>% # join tibble with all IDs 
  setNames(gsub(" ", "_", names(.))) %>% # replace spaces with _ in column names
  setNames(toupper(names(.))) %>%
  ungroup()



################################## VACCINES ##################################

vaccines_clean_aug <- vaccines_clean


################################ MERGED TABLE ################################

merged_data_wide <- patients_clean_aug %>%
  inner_join(symptoms_clean_aug, by = "VAERS_ID") %>%
  inner_join(vaccines_clean_aug, by = "VAERS_ID")


######################### LONG FORMAT SYMPTOMS TABLE #########################

# Make long format tibble containing VAERS_ID, SEX, and symptoms column with all top 20 symptoms
merged_data_long <- merged_data %>%
  pivot_longer(cols = (top_20_vec %>% toupper(.) %>% gsub(" ", "_", .)), 
               names_to = "SYMPTOM", 
               values_to = "SYMPTOM_VALUE")



# Write data --------------------------------------------------------------
write_csv(x = patients_clean_aug,
          file = "data/03_patients_clean_aug.csv.gz")

write_csv(x = symptoms_clean_aug,
          file = "data/03_symptoms_clean_aug.csv.gz")

write_csv(x = vaccines_clean_aug,
          file = "data/03_vaccines_clean_aug.csv.gz")

write_csv(x = merged_data_wide,
          file = "data/03_merged_data_wide.csv.gz")

write_csv(x = merged_data_long,
          file = "data/03_merged_data_long.csv.gz")

