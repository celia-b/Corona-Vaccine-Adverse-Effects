# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
merged_clean_aug <- read_csv(file = "data/03_merged_data.csv")
  


# Wrangle data ------------------------------------------------------------

################################## SYMPTOMS ##################################

# Get vector with names of symptom columns in order to refer to refer to 
# all symptom columns later on
symptom_cols <- merged_clean_aug %>%
  select(DYSPNOEA, PAIN_IN_EXTREMITY, DIZZINESS, FATIGUE, INJECTION_SITE_ERYTHEMA, 
         INJECTION_SITE_PRURITUS, INJECTION_SITE_SWELLING, CHILLS, RASH, HEADACHE, 
         INJECTION_SITE_PAIN, NAUSEA, PAIN, PYREXIA, MYALGIA, ARTHRALGIA, PRURITUS, 
         ASTHENIA, VOMITING, DEATH) %>%
  names()

# Make long format tibble containing VAERS_ID, SEX, and symptoms column with all top 20 symptoms
merged_clean_aug_long <- merged_clean_aug %>%
  select(VAERS_ID, SEX, N_SYMPTOMS, all_of(symptom_cols)) %>%
  pivot_longer(cols = all_of(symptom_cols), 
               names_to = "symptom", 
               values_to = "value")


# Model data ----------------------------------------------------------
my_data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ...


#################### GENDER VS NUMBER/TYPES OF SYMPTOMS ####################

# Get list of relative symptom counts for men and women (symptom counts for each
# gender relative to number of each gender that participated in study). 
# Make bar plot showing relative occurence of top 20 symptoms by gender
symp_types_bar <- merged_clean_aug_long %>%
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
n_symp_bar <- merged_clean_aug %>%
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


