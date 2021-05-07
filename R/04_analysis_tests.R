# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library ("cowplot")
library("patchwork")
library("scales")
library("broom")
library("purrr")
library("infer")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Wide format
merged_data_wide <- read_csv(file = gzfile("data/03_merged_data_wide.csv.gz"),
                             col_types = cols(VAX_DOSE_SERIES = col_character()))

# Long format symptom column
merged_data_long <- read_csv(file = gzfile("data/03_merged_data_long.csv.gz"), 
                             col_types = cols(HOSPDAYS = col_integer(),
                                              DIED_AFTER = col_integer(), 
                                              VAX_DOSE_SERIES = col_character()))

# Wrangle data ------------------------------------------------------------

# Convert variables to factors
merged_data_wide <- merged_data_wide %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)

# Convert variables to factors
merged_data_long <- merged_data_long %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)

# Define symptoms
symptoms <- merged_data_wide %>% 
  select(DYSPNOEA, PAIN_IN_EXTREMITY, DIZZINESS, FATIGUE, 
         INJECTION_SITE_ERYTHEMA, INJECTION_SITE_PRURITUS, INJECTION_SITE_SWELLING, 
         CHILLS, RASH, HEADACHE, INJECTION_SITE_PAIN, NAUSEA,PAIN, PYREXIA, MYALGIA,
         ARTHRALGIA, PRURITUS, ASTHENIA, VOMITING, DEATH) %>%
  names()



# Model data --------------------------------------------------------------


############################# STATISTICS ###############################

## Proportion tests for DIED vs. different variables

# Null hypothesis: the proportions are the same
# Assumptions:

#Data in contingency table is presented in counts (not in percent)
#All cells contain more than 5 observations
#Each observation contributes to one group only
#Groups are independent
#The variables under study are categorical
#The sample is, supposedly, reasonably random


# Function that performs a Pearson's Chi-squared contingency table test between two variables
chisq_func <- function(variable1, variable2) {
  variable1 <- enquo(variable1) 
  variable2 <- enquo(variable2) 
  merged_data_wide %>%
    group_by(!!variable1, !!variable2) %>%
    summarise(n = n()) %>%
    spread(!!variable2, n) %>%  
    tibble() %>% 
    select(-!!variable1) %>% 
    chisq.test() %>% 
    tidy()
}  

# Run different Chi-squared tests
chisq_func(DIED, HAS_ILLNESS)           # p-value = 2.79e-187
chisq_func(DIED, SEX)                   # p-value = 1.40e-183
chisq_func(DIED, HAS_COVID)             # p-value = 1.23e-9
chisq_func(DIED, HAD_COVID)             # p-value = 0.139
chisq_func(DIED, HAS_ALLERGIES)         # p-value = 0.316
chisq_func(DIED, HOSPITAL)              # p-value = 0.0000000277
chisq_func(DIED, L_THREAT)              # p-value = 0.0138 
chisq_func(DIED, PRIOR_ADVERSE)         # p-value = 1.66e-15
chisq_func(DIED, TAKES_ANTIINFLAMATORY) # p-value = 3.95e-20
chisq_func(DIED, TAKES_STEROIDS)        # p-value = 0.00205
chisq_func(DIED, VAX_MANU)              # p-value = 1.83e-10

# Using the infer library, it can be done like this:
chisq_test(merged_data_wide, DIED ~ HAS_ILLNESS)


# How to make a contingency table:
merged_data_wide %>%
  group_by(DIED, VAX_MANU) %>%
  summarise(n = n()) %>%
  spread(DIED, n) %>%  
  tibble() 


# Visualizations
merged_data_wide %>%
  ggplot(aes(x = DIED, fill = HAS_ILLNESS)) +
  geom_bar(position = "fill") +
  labs(title = "Visualization of Contingency Table",
       x = "DIED",
       y = "Proportion")

merged_data_wide %>%
  ggplot(aes(x = DIED, fill = SEX)) +
  geom_bar(position = "fill") +
  labs(title = "Visualization of Contingency Table",
       x = "DIED",
       y = "Proportion")


merged_data_wide %>%
  ggplot(aes(x = DIED, fill = HAS_COVID)) +
  geom_bar(position = "fill") +
  labs(title = "Visualization of Contingency Table",
       x = "DIED",
       y = "Proportion")

merged_data_wide %>%
  ggplot(aes(x = DIED, fill = VAX_MANU)) +
  geom_bar(position = "fill") +
  labs(title = "Visualization of Contingency Table",
       x = "DIED",
       y = "Proportion")

# Make some mosaic plots?



# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)

