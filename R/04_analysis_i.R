# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")
library("scales")


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

## Distribution of the number of days after receiving the vaccine
## when symptoms appear.
symptoms_after <- merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  ggplot(aes(SYMPTOMS_AFTER, stat(prop))) +
  geom_bar() +
  scale_y_continuous(labels = percent) +
  labs(x = "Days after vaccination",
       y = "Relative ocurrence")



## By age class --> I think we need less age classes
# Also I'm not convinced this plot looks nice --> maybe facet_wrap it?
symptoms_after_age <- merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(SYMPTOMS_AFTER, stat(prop), fill = AGE_CLASS)) +
  geom_bar(position = "dodge") +
  #scale_y_continuous(labels = percent) +
  labs(#subtitle = "Grouping by age class",
       x = "Days after vaccination",
       y = "Relative ocurrence") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(fill = "Age class")



# Putting them together
(symptoms_after + symptoms_after_age) + 
  plot_annotation(title = 'Days after vaccination when symptoms appear',
                  subtitle = NULL,
                  caption = "(Symptoms that appear after day 20 are infrequent and not shown)") +
  theme(plot.title = element_text(size=24))



## By manufacturer 
symptoms_after_manu <- merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  ggplot(aes(SYMPTOMS_AFTER, stat(prop), fill = VAX_MANU)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(title = "Days after vaccination when symptoms appear.",
       subtitle = "Grouping by manufacturer",
       caption = NULL,
       tag = NULL,
       x = "Days after vaccination",
       y = "Relative ocurrence") +
  labs(fill = "Manufacturer")

symptoms_after_manu



######################## DEATH AFTER N DAYS ############################

## Days after vaccination when symptoms appear.
death_after <- merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  ggplot(aes(DIED_AFTER, stat(prop))) +
  geom_bar() +
  scale_y_continuous(labels = percent) +
  labs(title = "Days after vaccination when death occurs",
       caption = "(Death after 30 days is infrequent and not shown)",
       x = "Days after vaccination",
       y = "Relative ocurrence")

death_after


## By age class --> Not informative (small sample size, < 5 in age classes)
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  drop_na(AGE_CLASS) %>%
  group_by(AGE_CLASS) %>%
  count()


## By manufacturer --> Again problematic because of small sample size in Janssen
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  group_by(VAX_MANU) %>%
  count() # --> 16 (Janssen) vs 852 vs 690 (Moderna and Pfizer)

n_days_manu <- merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  ggplot(aes(DIED_AFTER, stat(prop), fill = VAX_MANU)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(title = "Days after vaccination when death occurs.",
       subtitle = "Grouping by manufacturer",
       caption = NULL,
       tag = NULL,
       x = "Days after vaccination",
       y = "Relative ocurrence") +
  labs(fill = "Manufacturer")

n_days_manu



######################## DEATH RATE ###############################
# Out of the people who died, what proportion were already sick?
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  filter(DIED == 'Y') %>%
  ggplot(aes(HAS_ILLNESS)) + 
  geom_bar() +
  coord_flip()

# How much more likely is it to die when you were already sick when 
# taking the vaccine vs when you were healthy? --> DO STATISTICAL ANALYSIS
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  group_by(HAS_ILLNESS) %>%
  count()

merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  group_by(HAS_ILLNESS, DIED) %>%
  count()

prop_test(x = c(1191, 644), n = c(29127, 4306), 
          p = NULL, alternative = "two.sided", correct = TRUE)


# Out of the people who died, what proportion had covid at the time?
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_COVID, VAX_MANU) %>%
  filter(DIED == 'Y') %>%
  ggplot(aes(HAS_COVID)) + 
  geom_bar() +
  coord_flip()


# How much more likely is it to die when you had covid while 
# taking the vaccine vs when you were healthy? --> DO STATISTICAL ANALYSIS
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_COVID, VAX_MANU) %>%
  group_by(HAS_COVID) %>%
  count()

merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_COVID, VAX_MANU) %>%
  group_by(HAS_COVID, DIED) %>%
  count()

# How much less likely is it to die when you had covid in the past? 
# --> DO STATISTICAL ANALYSIS
merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAD_COVID, VAX_MANU) %>%
  group_by(HAD_COVID) %>%
  count()

merged_data %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAD_COVID, VAX_MANU) %>%
  group_by(HAD_COVID, DIED) %>%
  count()


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)