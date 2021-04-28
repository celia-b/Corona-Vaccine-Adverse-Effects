# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
vaccines <- read_csv(file = "data/01_vaccines.csv")
patients <- read_csv(file = "data/01_patients.csv")
symptoms <- read_csv(file = "data/01_symptoms.csv")


# Wrangle data ------------------------------------------------------------
#my_data_clean <- my_data # %>% ...

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

