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


# Wrangle data ------------------------------------------------------------

# Convert variables to factors
merged_data_wide <- merged_data_wide %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)



# Model data --------------------------------------------------------------

## Proportion tests for DIED vs. different variables

# Null hypothesis: the proportions are the same. The two variables are independent.
# Assumptions:
# Data in contingency table is presented in counts (not in percent)
# All cells contain more than 5 observations
# Each observation contributes to one group only
# Groups are independent
# The variables under study are categorical
# The sample is, supposedly, reasonably random


# Run Chi-squared tests
chisq_func(DIED, VAX_MANU)              # p-value = 2.24e-10
chisq_func(DIED, SEX)                   # p-value = 1.21e-183


# Using the infer library, it can be done like this:
chisq_test(merged_data_wide, DIED ~ VAX_MANU) 
chisq_test(merged_data_wide, DIED ~ SEX) 


# Contingency tables:
merged_data_wide %>%
  group_by(DIED, VAX_MANU) %>%
  summarise(n = n()) %>%
  spread(VAX_MANU, n) %>% 
  tibble() 

merged_data_wide %>%
  filter(!is.na(SEX)) %>% 
  group_by(DIED, SEX) %>%
  summarise(n = n()) %>%
  spread(SEX, n) %>% 
  tibble() 


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
