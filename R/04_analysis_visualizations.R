# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")
library("patchwork")
library("scales")
library("broom")
library("infer")
library("viridis")


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
                                              VAX_DOSE_SERIES = col_character(),
                                              DIED = col_character()))

# Wrangle data ------------------------------------------------------------

# Convert variables to factors
merged_data_wide <- merged_data_wide %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)

# Convert variables to factors
merged_data_long <- merged_data_long %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)

# Use top_n_symptoms() function to get vector of top 20 symptoms occurring in data set.
# Use capitalize() function to capitalize elements and replace spaces with _
symptoms <- top_n_symptoms(data = symptoms_clean, n_symp = 20) %>%
  capitalize()


# Visualise data ----------------------------------------------------------

## AGE DISTRIBUTION -------------------------------

# Age 
age_dist <- merged_data_wide %>%
  ggplot(aes(AGE_YRS, 
             stat(count))) +
  geom_density(color="black", fill="#00846b",alpha = 0.5) +
  labs(x = "Age (years)",
       y = "Count",
       title = "Age distribution of the subjects in the dataset") +
  theme_minimal(base_family = "Avenir", base_size = 12) +
  theme(plot.title = element_text(size=13))
               
## SYMPTOMS AFTER N DAYS -------------------------------------------------

## Distribution of the number of days after receiving the vaccine
## when symptoms appear.
symptoms_after <- merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  ggplot(aes(SYMPTOMS_AFTER, stat(prop))) +
  geom_bar() +
  scale_y_continuous(labels = percent) +
  labs(x = "Days after vaccination",
       y = "Relative ocurrence")


## By age class
# I'm not convinced this plot looks nice --> maybe facet_wrap it? make it into heatmap?
# Delete ?
symptoms_after_age <- merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(SYMPTOMS_AFTER, 
             stat(prop), 
             fill = AGE_CLASS)) +
  geom_bar(position = "dodge") +
  #scale_y_continuous(labels = percent) +
  labs(#subtitle = "Grouping by age class",
    x = "Days after vaccination",
    y = "Relative ocurrence") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(fill = "Age class")

# Patchwork
# delete ?
(symptoms_after + symptoms_after_age) + 
  plot_annotation(title = "Days after vaccination when symptoms appear",
                  subtitle = NULL,
                  caption = "(Symptoms that appear after day 20 are infrequent and not shown)") +
  theme(plot.title = element_text(size = 24))


## By manufacturer 
# delete ?
symptoms_after_manu <- merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, SYMPTOMS_AFTER, VAX_MANU) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 20) %>%
  ggplot(aes(SYMPTOMS_AFTER, 
             stat(prop), 
             fill = VAX_MANU)) +
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



## DEATH AFTER N DAYS ----------------------------------------------------

## Days after vaccination when symptoms appear.
# delete ?
death_after <- merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  ggplot(aes(DIED_AFTER, 
             stat(prop))) +
  geom_bar() +
  scale_y_continuous(labels = percent) +
  labs(title = "Days after vaccination when death occurs",
       caption = "(Death after 30 days is infrequent and not shown)",
       x = "Days after vaccination",
       y = "Relative ocurrence")

death_after


## By age class --> Not informative (small sample size, < 5 in age classes)
# delete ?
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  drop_na(AGE_CLASS) %>%
  group_by(AGE_CLASS) %>%
  count()

## By manufacturer --> Again problematic because of small sample size in Janssen
# delete?
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  group_by(VAX_MANU) %>%
  count() # --> 16 (Janssen) vs 852 vs 690 (Moderna and Pfizer)

# delete ?
n_days_manu <- merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  ggplot(aes(DIED_AFTER, 
             stat(prop), 
             fill = VAX_MANU)) +
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


## DEATH RATE ----------------------------------------------------------------
# Out of the people who died, what proportion were already sick? 
# delete ?
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  filter(DIED == "Y") %>%
  ggplot(aes(HAS_ILLNESS)) + 
  geom_bar() +
  coord_flip()


## NUMBER OF SYMPTOMS ----------------------------------------------------

# Boxplot showing number of symptoms vs age
nsymptoms_v_age <- merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(x = AGE_CLASS,
             y = N_SYMPTOMS, 
             fill = AGE_CLASS)) +
  geom_boxplot(outlier.shape = NA, 
               alpha = 0.6) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_x_discrete(labels = c("0-15", "15-25", "25-40", "40-60", "60-80",
                              "80+")) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Age vs. number of symptoms",
       x = "Age (years)",
       y = "Number of symptoms") +
  theme_minimal(base_family = "Avenir") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(10, 30, 10, 10))

# Boxplot showing total number of symptoms by sex
nsymptoms_v_sex <- merged_data_long %>%
  drop_na(SEX) %>%
  ggplot(aes(x = SEX,
             y = N_SYMPTOMS, 
             fill = SEX)) +
  geom_boxplot(outlier.shape = NA, 
               alpha = 0.6) +
  scale_y_continuous(limits = c(0, 15)) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Sex vs. number of symptoms",
       x = "Sex ",
       y = "Number of symptoms", 
       fill = "Sex") +
  theme_minimal(base_family = "Avenir") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 30, 10, 10))

# Boxplot showing total number of symptoms by vaccine manufacturer
nsymptoms_v_manu <- merged_data_long %>%
  ggplot(aes(x = VAX_MANU,
             y = N_SYMPTOMS, 
             fill = VAX_MANU)) +
  geom_boxplot(outlier.shape = NA, 
               alpha = 0.6) +
  scale_y_continuous(limits = c(0, 15)) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Manufacturer vs. number of symptoms",
       subtitle = "Outliers not included",
       x = "Vaccine manufacturer",
       y = "Number of symptoms") +
  theme_minimal(base_family = "Avenir") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(10, 30, 10, 10))

# Combine plots showing age/sex vs. number of symptoms into one
# figure using patchwork
nsymptoms_age_sex <- (nsymptoms_v_age + nsymptoms_v_sex) +
  plot_annotation(caption = "Outliers not included")


## TYPES OF SYMPTOMS ----------------------------------------------------------

# Bar chart showing relative occurrence of the top 20 symptoms by sex. 
# Symptom counts are relative to the number of individuals from the respective sex, 
# as there are more females than males in the dataset.
symptom_types_v_sex <- merged_data_long %>%
  count(SEX, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, SEX) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE, !is.na(SEX)) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = fct_reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = SEX)) +
  geom_bar(position = position_dodge2(width = 1.5),
           stat = "identity",
           width = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Sex vs. types of symptoms",
       x = "Symptoms",
       y = "Relative occurence",
       fill = "Sex") +
  theme_minimal(base_family = "Avenir") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 9), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))


# Bar chart showing relative occurrence of the top 20 symptoms by manufacturer. 
# Symptom counts are relative to the number of individuals vaccinated with the 
# respective vaccine. 
symptom_types_v_manu <- merged_data_long %>%
  count(VAX_MANU, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(VAX_MANU, SYMPTOM) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = VAX_MANU)) +
  geom_bar(position = position_dodge2(width = 1.5),
           stat = "identity", 
           width = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Manufacturer vs. types of symptoms",
       x = "Symptoms",
       y = "Relative occurence",
       fill = "Vaccine manufacturer") +
  theme_minimal(base_family = "Avenir") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 8), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))


# Heatmap showing the relative occurrence of top 20 symptoms by age. 
# Counts are relative to the number of people in the respective age groups.
symptom_types_v_age <- merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  count(AGE_CLASS, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, AGE_CLASS) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = AGE_CLASS, 
             y = fct_reorder(SYMPTOM, desc(prop)),
             fill = prop)) +
  geom_tile() +
  scale_x_discrete(labels = c("0-15", "15-25", "25-40", "40-60", "60-80",
                              "80+")) +
  scale_fill_viridis_c() +
  labs(title = "Age vs. types of symptoms",
       x = "Age (years)",
       y = "Top 20 symptoms",
       fill = "Relative occurence") +
  theme_minimal(base_family = "Avenir") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 9), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))


## AGE DISTRIBUTION OF PEOPLE WHO ... --------------------------

# Total people
merged_data_wide %>%
  ggplot(aes(x = AGE_YRS, y = ..density..)) +
  geom_histogram(bins = 116) +
  geom_density(color = "blue") +
  labs(title = "Age distribution of subjects in the dataset", 
       x = "Age (years)",
       y = "Proportion")

# People who died -- > Not accounting for different age group sizes
merged_data_wide %>%
  filter(DIED == "Y") %>%
  drop_na() %>%
  ggplot(aes(AGE_YRS)) +
  geom_density()

# People who had to go to the hospital after vaccination 
# -- > Not accounting for different age group sizes
merged_data_wide %>%
  filter(HOSPITAL == "Y") %>%
  drop_na() %>%
  ggplot(aes(AGE_YRS)) +
  geom_density()



# Write data --------------------------------------------------------------

# Save age distribution plot
ggsave(age_dist, file = "results/age_dist.png")


# Save number of symptoms plots
ggsave(nsymptoms_age_sex, 
       file = "results/nsymptoms_age_sex.png", 
       height = 4, 
       width = 10)

ggsave(nsymptoms_v_manu, 
       file = "results/nsymptoms_v_manu.png",
       height = 5,
       width = 8)

ggsave(symptom_types_v_age, 
       file = "results/symptom_types_v_age.png",
       height = 8,
       width = 9)


# Save types of symptoms plots
ggsave(symptom_types_v_sex, 
       file = "results/symptom_types_v_sex.png",
       height = 5,
       width = 10)

ggsave(symptom_types_v_manu, 
       file = "results/symptom_types_v_manu.png",
       height = 5,
       width = 10)


