# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
merged_data <- read_csv(file = "data/03_merged_data.csv",
                        col_types = cols(VAX_DOSE_SERIES = col_character()))


# Wrangle data ------------------------------------------------------------
#my_data_clean_aug %>% ...


# Model data
#my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
######################## SYMPTOMS AFTER N DAYS ############################
## Distribution of the number of days after vaccine injection
## when symptoms appear.

merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  ggplot(aes(SYMPTOMS_AFTER)) +
  geom_bar() # would be nice to have x-axis as proportion instead of count

# It's interesting that there is a bump at around 7 days, which is when 
# the adaptive immune system kick in. Maybe symptoms that appear immediately
# after the vaccine is recieved are due to the innate immune response
# and the ones after a few days are the adaptive.

# By age class 
# --> there is a problem here with unequal representation of age classes
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(SYMPTOMS_AFTER, fill = AGE_CLASS)) +
  geom_bar()

# By manufacturer 
# --> again, problem with manufacturer representation
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  ggplot(aes(SYMPTOMS_AFTER, fill = VAX_MANU)) +
  geom_bar()


######################## NUMBER OF SYMPTOMS ###############################
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, N_SYMPTOMS, VAX_MANU) %>%
  arrange(N_SYMPTOMS) %>%
  filter(N_SYMPTOMS < 20) %>%
  ggplot(aes(N_SYMPTOMS)) + 
  geom_bar()

  


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)