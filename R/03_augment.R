# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
# Misinterpreted column types are manually asigned
## 1. Patients ------------------------------------------------------------
patients_clean <- read_csv(file = gzfile("data/02_patients_clean.csv.gz"), 
                           col_types = cols(VAX_DATE = col_date(format = "%m/%d/%Y"),
                                            DATEDIED = col_date(format = "%m/%d/%Y")))

## 2. Symptoms ------------------------------------------------------------
symptoms_clean <- read_csv(file = gzfile("data/02_symptoms_clean.csv.gz"))



## 3. Vaccines ------------------------------------------------------------
vaccines_clean <- read_csv(file = gzfile("data/02_vaccines_clean.csv.gz"),
                           col_types = cols(VAX_DOSE_SERIES = col_character()))


# Wrangle data ------------------------------------------------------------

## 1. Patients ---------------------------------------------------------------
# Convert variables containing long strings into categorical (Y/N) variables. 
# Remove dirty and uninformative columns. 

patients_clean_aug <- patients_clean %>%
  mutate(HAS_ALLERGIES = case_when(is.na(ALLERGIES) ~ "N",
                                   TRUE ~ "Y")) %>% 
  select(-ALLERGIES) %>% 
  mutate(HAS_ILLNESS = case_when(is.na(CUR_ILL) ~ "N",
                                 TRUE ~ "Y")) %>% 
  mutate(HAS_COVID = case_when(grepl("covid",
                                     CUR_ILL,
                                     ignore.case = TRUE) ~ "Y",
                               TRUE ~ "N")) %>%
  mutate(HAD_COVID = case_when(grepl("covid",
                                     HISTORY,
                                     ignore.case = TRUE) ~ "Y",
                               TRUE ~ "N")) %>% 
  select(-c(CUR_ILL, 
            HISTORY)) %>% 
  mutate(PRIOR_ADVERSE = case_when(is.na(PRIOR_VAX) ~ "N",
                                   TRUE ~ "Y")) %>% 
  select(-PRIOR_VAX) %>% 
  mutate(TAKES_ANTIINFLAMATORY = case_when(grepl("ibuprofen | aspirin | celecoxib |
                                                 diclofenac | diflunisal | etodolac |
                                                 indomethacin", 
                                                 OTHER_MEDS, 
                                                 ignore.case = TRUE) ~ "Y",
                                           TRUE ~ "N")) %>% 
  select(-OTHER_MEDS) %>% 
  mutate(AGE_CLASS = case_when(AGE_YRS < 15 ~ "[0, 15)",
                               AGE_YRS >= 15 & AGE_YRS < 25 ~ "[15, 25)",
                               AGE_YRS >= 25 & AGE_YRS < 40 ~ "[25, 40)",
                               AGE_YRS >= 40 & AGE_YRS < 60 ~ "[40, 60)",
                               AGE_YRS >= 60 & AGE_YRS < 80 ~ "[60, 80)",
                               AGE_YRS >= 80 ~ "80+")) %>%
  mutate(DIED_AFTER = (as.integer(DATEDIED) - as.integer(VAX_DATE))) %>%
  filter(DIED_AFTER >= 0 | is.na(DIED_AFTER)) %>%
  rename(SYMPTOMS_AFTER = NUMDAYS) %>% 
  select(-c(VAX_DATE, 
            DATEDIED, 
            ONSET_DATE, 
            TODAYS_DATE))


## 2. Symptoms -------------------------------------------------------------
# Convert symptoms dataset into long format

symptoms_clean_long <- symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom_num", 
               values_to = "symptom",
               values_drop_na = TRUE) %>%
  select(-symptom_num)


# Extract top 20 occurring symptoms using self-made top_n_symptoms_func() function.
# Filter for individuals that have a least one of these top 20 symptoms. 
# Fill tibble with TRUE/FALSE depending on whether the individual has symptom.
# Convert top 20 symptoms to wide format tibble. 

top_20_symptoms <- symptoms_clean_long %>%
  filter(symptom %in% top_n_symptoms_func(data = symptoms_clean, 
                                          n_symp = 20, 
                                          VAERS_ID = VAERS_ID)) %>% 
  mutate(true_col = TRUE) %>%
  pivot_wider(id_cols = VAERS_ID,
              names_from = symptom,
              values_from = true_col,
              values_fill = FALSE)


# Reintroduce individuals with none of the top 20 symptoms which were filtered out above. 
# The result is a tibble containing all IDs and TRUE/FALSE symptom columns.

symptoms_all_IDs <- symptoms_clean %>% 
  select(VAERS_ID) %>%
  distinct(VAERS_ID) %>% 
  full_join(top_20_symptoms,
            by = "VAERS_ID") %>% 
  replace(is.na(.), 
          FALSE)

# Make new column containing total number of symptoms each individual has.
# Join this column with top 20 symptom columns. 
# The final tibble contains IDs, total number of symptoms and top 20 symptoms.

symptoms_clean_aug <- symptoms_clean_long %>%
  group_by(VAERS_ID) %>%
  count(sort = FALSE) %>%
  rename(n_symptoms = n) %>%
  full_join(symptoms_all_IDs,
            by = "VAERS_ID") %>%
  setNames(gsub(" ", 
                "_", 
                colnames(.))) %>%
  setNames(str_to_upper(names(.))) %>%
  ungroup()

<<<<<<< HEAD


=======
>>>>>>> a9c5e8ac6a15b035edf1b337db94c58161320b9f
## 3. Vaccines --------------------------------------------------------------

vaccines_clean_aug <- vaccines_clean


## 4. Merged ------------------------------------------------------------------

### 4.1 Wide format -----------------------------------------------------------

# Merge patients, symptoms, and vaccine data into one tibble.
# Make columns DIED and DEATH (symptom) identical.
merged_data_wide <- patients_clean_aug %>%
  inner_join(symptoms_clean_aug, 
             by = "VAERS_ID") %>%
  inner_join(vaccines_clean_aug, 
             by = "VAERS_ID") %>%
  mutate(DIED = case_when(DIED == "Y" ~ "Y", 
                          DEATH == TRUE ~ "Y",
                          TRUE ~ "N"),
         DEATH = case_when(DEATH == TRUE ~ TRUE, 
                           DIED == "Y" ~ TRUE,
                           TRUE ~ FALSE))

### 4.2 Long format -----------------------------------------------------------

# Make long format tibble containing a symptoms column with all top 20 symptoms
# Use format_func() to capitalize and replace spaces with _ in symptom names
merged_data_long <- merged_data_wide %>%
  pivot_longer(cols = (top_n_symptoms_func(data = symptoms_clean, 
                                           n_symp = 20) %>% 
                         format_func()), 
               names_to = "SYMPTOM", 
               values_to = "SYMPTOM_VALUE")



# Write data --------------------------------------------------------------

## 1. Patients ------------------------------------------------------------
write_csv(x = patients_clean_aug,
          file = "data/03_patients_clean_aug.csv.gz")

## 2. Symptoms ------------------------------------------------------------
write_csv(x = symptoms_clean_aug,
          file = "data/03_symptoms_clean_aug.csv.gz")

## 3. Vaccines ------------------------------------------------------------
write_csv(x = vaccines_clean_aug,
          file = "data/03_vaccines_clean_aug.csv.gz")

## 4. Merged Tables  ------------------------------------------------------
write_csv(x = merged_data_wide,
          file = "data/03_merged_data_wide.csv.gz")

write_csv(x = merged_data_long,
          file = "data/03_merged_data_long.csv.gz")

