# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
merged_data <- read_csv(file = "data/03_merged_data.csv",
                        col_types = cols(VAX_DOSE_SERIES = col_character()))
  


# Wrangle data ------------------------------------------------------------

################################## SYMPTOMS ##################################

# Get vector with names of symptom columns in order to refer to refer to 
# all symptom columns later on
symptom_cols <- merged_data %>%
  select(DYSPNOEA, PAIN_IN_EXTREMITY, DIZZINESS, FATIGUE, INJECTION_SITE_ERYTHEMA, 
         INJECTION_SITE_PRURITUS, INJECTION_SITE_SWELLING, CHILLS, RASH, HEADACHE, 
         INJECTION_SITE_PAIN, NAUSEA, PAIN, PYREXIA, MYALGIA, ARTHRALGIA, PRURITUS, 
         ASTHENIA, VOMITING, DEATH) %>%
  names()

# Make long format tibble containing VAERS_ID, SEX, and symptoms column with all top 20 symptoms
merged_long <- merged_data %>%
  select(VAERS_ID, SEX, N_SYMPTOMS, all_of(symptom_cols)) %>%
  pivot_longer(cols = all_of(symptom_cols), 
               names_to = "symptom", 
               values_to = "value")


# Model data ----------------------------------------------------------
#my_data_clean_aug %>% ...



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

  


#################### GENDER VS NUMBER/TYPES OF SYMPTOMS ####################

# Get list of relative symptom counts for men and women (symptom counts for each
# gender relative to number of each gender that participated in study). 
# Make bar plot showing relative occurence of top 20 symptoms by gender
symp_types_bar <- merged_long %>%
  count(SEX, symptom, value) %>%
  group_by(symptom, SEX) %>%
  mutate(total = sum(n)) %>%
  filter(value == TRUE) %>%
  summarise(prop = n/total*100, .groups = "rowwise") %>%
  ggplot(.,
         aes(x = reorder(symptom, desc(prop)),
             y = prop,
             fill = SEX)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Relative occurence of top 20 symptoms by gender") +
  xlab("Symptoms") + 
  ylab("Relative occurence (%)")

# Plot number of symptoms experienced by males and females as bar plot
n_symp_bar <- merged_data %>%
  group_by(SEX) %>%
  ggplot(.,
         aes(x = N_SYMPTOMS,
             fill = SEX,
             stat(prop))) +
  geom_bar(position = "dodge") + 
  scale_x_continuous(limits = c(0, 25)) + 
  ggtitle("Total number of symptoms by gender") +
  xlab("Number of symptoms") + 
  ylab("Relative occurence")






# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)


