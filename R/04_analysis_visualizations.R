# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")
library("patchwork")
library("scales")
library("viridis")
library("ggridges")


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
  mutate_if(is.character,
            as.factor) %>%
  mutate_if(is.logical,
            as.factor)

# Convert variables to factors
merged_data_long <- merged_data_long %>%
  mutate_if(is.character,
            as.factor) %>%
  mutate_if(is.logical,
            as.factor)

# Use top_n_symptoms_func() function to get vector of top 20 symptoms occurring in data set.
# Use format_func() function to capitalize vector elements and replace spaces with _
symptoms <- top_n_symptoms_func(data = symptoms_clean, 
                                n_symp = 20) %>%
  format_func()


# Visualise data ----------------------------------------------------------

## 1. Age distribution -------------------------------------------------------

# Make density plot showing the age distribution within the data set
age_dist <- merged_data_wide %>%
  ggplot(aes(x = AGE_YRS, 
             y = stat(count))) +
  geom_density(color = "black", 
               fill = "#00846b",
               alpha = 0.5) +
  labs(x = "Age (years)",
       y = "Count",
       title = "Age distribution of the subjects in the dataset") +
  theme_minimal(base_family = "Avenir",
                base_size = 12) +
  theme(plot.title = element_text(size = 13))
               

## 2. Symptoms after n days -------------------------------------------------

### 2.1 Bar plot ------------------------------------------------------------

# Make bar plot showing the distribution of number of days after receiving a
# vaccine that symptoms appear
symptoms_after <- merged_data_wide %>%
  select(VAERS_ID,
         SYMPTOMS_AFTER) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 15) %>%
  ggplot(aes(x = SYMPTOMS_AFTER,
             y = stat(count))) +
  geom_bar(fill = "#1E9B8AFF" ,
           alpha = 0.7) +
  labs(x = "Days after vaccination",
       y = "Count") +
  theme_minimal(base_family = "Avenir",
                base_size = 12) 


### 2.2 Density plot grouped by age class  --------------------------------------

# Make density plots showing the distribution of number of days after receiving 
# a vaccine that symptoms appear, grouped by age class
symptoms_after_age <- merged_data_wide %>%
  select(AGE_CLASS,
         SYMPTOMS_AFTER) %>%
  arrange(SYMPTOMS_AFTER) %>%
  filter(SYMPTOMS_AFTER < 15) %>%
  drop_na(AGE_CLASS) %>% 
  ggplot(aes(x = SYMPTOMS_AFTER,
             y = AGE_CLASS,
             fill = AGE_CLASS)) +
  geom_density_ridges(alpha = 0.5) +
  scale_fill_viridis_d() +
  labs(fill = "Age class",
       x = "Days after vaccination",
       y = "Age class (years)") +
  theme_minimal(base_family = "Avenir", 
                base_size = 12) +
  theme(legend.position = "right")


### 2.3 Patchwork -------------------------------------------------------------

# Patchwork the two above plots together
symptoms_after_dist_age <- symptoms_after + 
  symptoms_after_age + 
  plot_annotation(title = "Distribution of the number of days after vaccination
                  until symptom onset",
                  subtitle = NULL,
                  caption = "Symptoms that appear after day 15 are infrequent 
                  and not shown")



## 3. Number of symptoms --------------------------------------------------------

### 3.1 Grouped by age --------------------------------------------------------

# Box plot showing age vs number of symptoms
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


### 3.2 Grouped by sex --------------------------------------------------------

# Box plot showing sex vs total number of symptoms
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


### 3.3 Grouped by vaccine manufacturer ---------------------------------------

# Box plot showing vaccine manufacturer vs. total number of symptoms
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


### 3.4 Patchwork -------------------------------------------------------------

# Combine plots showing age/sex vs. number of symptoms into one
# figure using patchwork
nsymptoms_age_sex <- (nsymptoms_v_age + nsymptoms_v_sex) +
  plot_annotation(caption = "Outliers not included")


## 4. Types of symptoms --------------------------------------------------------

### 4.1 Grouped by age

# Heat map showing the relative occurrence of top 20 symptoms by age. 
# Counts are relative to the number of people in the respective age groups.
symptom_types_v_age <- merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  count(AGE_CLASS, 
        SYMPTOM, 
        SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, 
           AGE_CLASS) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = AGE_CLASS, 
             y = fct_reorder(SYMPTOM, 
                             desc(prop)),
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


### 4.2 Grouped by sex --------------------------------------------------------

# Bar chart showing relative occurrence of the top 20 symptoms by sex. 
# Symptom counts are relative to the number of individuals from the respective sex, 
# as there are more females than males in the dataset.
symptom_types_v_sex <- merged_data_long %>%
  count(SEX, 
        SYMPTOM, 
        SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, 
           SEX) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE, 
         !is.na(SEX)) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = fct_reorder(SYMPTOM, 
                             desc(prop)),
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


### 4.2 Grouped by vaccine manufacturer ----------------------------------------

# Bar chart showing relative occurrence of the top 20 symptoms by manufacturer. 
# Symptom counts are relative to the number of individuals vaccinated with the 
# respective vaccine. 
symptom_types_v_manu <- merged_data_long %>%
  count(VAX_MANU, 
        SYMPTOM, 
        SYMPTOM_VALUE) %>%
  group_by(VAX_MANU, 
           SYMPTOM) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, 
            .groups = "rowwise") %>%
  ggplot(aes(x = reorder(SYMPTOM, 
                         desc(prop)),
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



# Write data --------------------------------------------------------------

# Save age distribution plot
ggsave(age_dist,
       file = "results/age_dist.png",
       height = 4,
       width = 7)

# Save days after vaccination plot
ggsave(symptoms_after_dist_age,
        file = "results/symptoms_after_dist_age.png",
        height = 5,
        width = 10)

# Save number of symptoms plots
ggsave(nsymptoms_age_sex, 
       file = "results/nsymptoms_age_sex.png", 
       height = 4, 
       width = 10)

ggsave(nsymptoms_v_manu, 
       file = "results/nsymptoms_v_manu.png",
       height = 5,
       width = 8)

# Save types of symptoms plots
ggsave(symptom_types_v_age, 
       file = "results/symptom_types_v_age.png",
       height = 8,
       width = 9)

ggsave(symptom_types_v_sex, 
       file = "results/symptom_types_v_sex.png",
       height = 5,
       width = 10)

ggsave(symptom_types_v_manu, 
       file = "results/symptom_types_v_manu.png",
       height = 5,
       width = 10)


