# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Wide format
merged_data <- read_csv(file = "data/03_merged_data.csv",
                        col_types = cols(VAX_DOSE_SERIES = col_character()))

# Long format symptom column
merged_data_long <- read_csv(file = gzfile("data/03_merged_data_long.csv.gz"))


# Wrangle data ------------------------------------------------------------




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


#################### SEX VS NUMBER/TYPES OF SYMPTOMS ####################

# Bar plot showing the relative occurrence of the top 20 symptoms by sex. 
# As there is not an equal number of males and females in the study, the
# counts are relative to the respective sex. 
merged_data_long %>%
  count(SEX, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, SEX) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(aes(x = fct_reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = SEX)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Sex", 
       labels = c("Females", "Males")) +
  ggtitle("Relative occurence of top 20 symptoms by sex") +
  xlab("Symptoms") + 
  ylab("Relative occurence")

# Bar plot showing the number of symptoms experienced by males and females. 
# The counts are relative to the respective genders. 
merged_data_long %>%
  ggplot(aes(x = N_SYMPTOMS,
             fill = SEX,
             stat(prop))) +
  geom_bar(position = "dodge") + 
  scale_x_continuous(limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Females", "Males")) +
  ggtitle("Relative occurrence of number of symptoms by gender") +
  xlab("Number of symptoms") + 
  ylab("Relative occurence") +
  theme_classic()



############### VACCINE MANUFACTURER VS NUMBER/TYPES OF SYMPTOMS ###############

# Total number of symptoms by vaccine manufacturer
# Stacked bar chart
merged_data_long %>%
  ggplot(aes(x = N_SYMPTOMS,
             fill = VAX_MANU,
             stat(prop))) +
  geom_bar(position = "stack") + 
  scale_x_continuous(limits = c(0, 20)) + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Total number of symptoms by vaccine manufacturer") +
  labs(fill = "Vaccine manufacturer") +
  xlab("Number of symptoms") + 
  ylab("Relative occurence") +
  theme_classic()


# Relative occurence of top 20 symptoms by manufacturer
# Non-stacked bar chart
merged_data_long %>%
  count(VAX_MANU, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(VAX_MANU, SYMPTOM) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(aes(x = reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = VAX_MANU)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Vaccine manufacturer") +
  ggtitle("Relative occurence of top 20 symptoms by manufacturer") +
  xlab("Symptoms") + 
  ylab("Relative occurence")



####################### VACCINE MANUFACTURER VS DEATH ########################

merged_data_long %>%
  count(VAX_MANU, DIED) %>%
  group_by(VAX_MANU) %>%
  mutate(total = sum(n)) %>%
  filter(DIED == "Y") %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(.,
         aes(x = fct_reorder(VAX_MANU, desc(prop)),
             y = prop,
             fill = VAX_MANU)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Relative occurence of death by vaccine manufacturer") +
  xlab("Vaccine manufacturer") +
  ylab("Relative occurence of death") 


merged_data_long %>%
  count(VAX_MANU, L_THREAT) %>%
  group_by(VAX_MANU) %>%
  mutate(total = sum(n)) %>%
  filter(L_THREAT == "Y") %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(.,
         aes(x = fct_reorder(VAX_MANU, desc(prop)),
             y = prop,
             fill = VAX_MANU)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Relative occurence of life threatening symptoms by vaccine manufacturer") +
  xlab("Vaccine manufacturer") +
  ylab("Relative occurence of life threatening symptoms") 


###################### AGE VS NUMBER/TYPES OF SYMPTOMS ######################

# Boxplot of number of symptoms vs age
# Works - nothing important?
merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(x = AGE_CLASS,
             y = N_SYMPTOMS)) +
  geom_boxplot(outlier.shape = NA, 
               fill = "sky blue", 
               alpha = 0.8) +
  scale_x_discrete(name = "Age groups (years)",
                   labels = c("0-10", "10-20", "20-30", "30-40", "40-50", 
                              "50-60", "60-70", "70-80", "80-90", "90-120")) +
  scale_y_continuous(limits = c(0, 15), 
                     name = "Number of symptoms") +
  coord_flip() +
  theme_classic()

# Bar plot showing the number of symptoms by age
# Stacked bar plot
# Consider grouping number of symptoms? 
# Change colors to blend together less
merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(x = N_SYMPTOMS,
             fill = AGE_CLASS,
             stat(prop))) +
  geom_bar(position = "stack") + 
  scale_x_continuous(limits = c(0, 30)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Age groups (years)", 
                      labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                                 "50-60", "60-70", "70-80", "80-90", "90-120")) +
  ggtitle("Relative occurrence of number of symptoms by age") +
  xlab("Number of symptoms") + 
  ylab("Relative occurence") +
  theme_classic()

# Relative occurence of top 20 symptoms by age
# Stacked bar plot
# Consider making bigger groups?
merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  count(AGE_CLASS, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, AGE_CLASS) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  arrange(SYMPTOM, desc(prop)) %>%
  #head(58) %>% 
  ggplot(aes(x = reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = AGE_CLASS)) +
  geom_bar(position = "stack",
           stat = "identity") +
  #facet_wrap(~ AGE_CLASS) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Age groups (years)", 
                      labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                                 "50-60", "60-70", "70-80", "80-90", "90-120")) +
  ggtitle("Relative occurence of top 20 symptoms by age") +
  xlab("Symptoms") + 
  ylab("Relative occurence")


# Relative occurence of top 20 symptoms by age
# heat map
merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  count(AGE_CLASS, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, AGE_CLASS) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = AGE_CLASS, 
             y = reorder(SYMPTOM, desc(prop)),
             fill = prop)) +
  geom_tile() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(name = "Age (years)", 
                   labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                              "50-60", "60-70", "70-80", "80-90", "90-120")) +
  ylab("Top 20 symptoms") +
  labs(fill = "Relative occurence") +
  ggtitle("Relative occurrence of top 20 symptoms by age")






# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)


