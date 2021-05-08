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

# Define symptoms
symptoms <- merged_data_wide %>% 
  select(DYSPNOEA, PAIN_IN_EXTREMITY, DIZZINESS, FATIGUE, 
         INJECTION_SITE_ERYTHEMA, INJECTION_SITE_PRURITUS, INJECTION_SITE_SWELLING, 
         CHILLS, RASH, HEADACHE, INJECTION_SITE_PAIN, NAUSEA,PAIN, PYREXIA, MYALGIA,
         ARTHRALGIA, PRURITUS, ASTHENIA, VOMITING, DEATH) %>%
  names()


# Visualise data ----------------------------------------------------------

################# AGE, SEX AND MANUFACTURER DISTRIBUTIONS #####################

# Age 
age_dist <- merged_data_wide %>%
  ggplot(aes(AGE_YRS, stat (count))) +
  geom_density(color="black", fill="lightblue",alpha = 0.5) +
  labs(x = "Age (years)",
       y = "Count",
       title = "Age distribution of the subjects in the dataset") +
  theme_minimal(base_family = "Avenir", base_size = 12) +
  theme ( plot.title = element_text(size=13))
               
age_dist

# Sex
sex_dist <- merged_data_wide %>%
  ggplot (aes (SEX, stat (count), fill = SEX)) +
  geom_bar () +
  labs (x = "Sex",
        y = "Count",
        fill = "Sex",
        title = "Sex distribution of the subjects in the dataset") +
  theme_minimal(base_family = "Avenir", base_size = 12) +
  theme (plot.title = element_text(size=13))

sex_dist

# Vaccine
vac_dist <- merged_data_wide %>%
  ggplot (aes (VAX_MANU, stat (count), fill = VAX_MANU)) +
  geom_bar() +
  labs (x = "Vaccine manufacturer",
        y = "Count",
        fill = "Vaccine manufacturer",
        title = "Vaccine manufacturer distribution") +
  theme_minimal(base_family = "Avenir", base_size = 12) +
  theme (plot.title = element_text(size=13))

vac_dist

########################## SYMPTOMS AFTER N DAYS ##############################

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
symptoms_after_age <- merged_data_wide %>%
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



# Patchwork
(symptoms_after + symptoms_after_age) + 
  plot_annotation(title = 'Days after vaccination when symptoms appear',
                  subtitle = NULL,
                  caption = "(Symptoms that appear after day 20 are infrequent and not shown)") +
  theme(plot.title = element_text(size=24))


## By manufacturer 
symptoms_after_manu <- merged_data_wide %>%
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
death_after <- merged_data_wide %>%
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
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  drop_na(AGE_CLASS) %>%
  group_by(AGE_CLASS) %>%
  count()

## By manufacturer --> Again problematic because of small sample size in Janssen
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED_AFTER, VAX_MANU) %>%
  arrange(DIED_AFTER) %>%
  filter(DIED_AFTER < 30 & DIED_AFTER >= 0) %>%
  group_by(VAX_MANU) %>%
  count() # --> 16 (Janssen) vs 852 vs 690 (Moderna and Pfizer)

n_days_manu <- merged_data_wide %>%
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


############################## DEATH RATE #####################################
# Out of the people who died, what proportion were already sick?
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  filter(DIED == 'Y') %>%
  ggplot(aes(HAS_ILLNESS)) + 
  geom_bar() +
  coord_flip()


############################# NUMBER OF SYMPTOMS #############################

# Boxplot showing number of symptoms vs age
nsymptoms_v_age <- merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  ggplot(aes(x = AGE_CLASS,
             y = N_SYMPTOMS, 
             fill = AGE_CLASS)) +
  geom_boxplot(outlier.shape = NA, 
               alpha = 0.8) +
  coord_flip() +
  scale_x_discrete(name = "Age groups (years)",
                   labels = c("0-10", "10-20", "20-30", "30-40", "40-50", 
                              "50-60", "60-70", "70-80", "80-90", "90-120")) +
  scale_y_continuous(limits = c(0, 15), 
                     name = "Number of symptoms") +
  ggtitle("Age vs. number of symptoms") +
  scale_fill_viridis_d() +
  theme_half_open(font_size = 9, 
                  font_family = "serif") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 20, 10, 10))


# Boxplot showing total number of symptoms by sex
nsymptoms_v_sex <- merged_data_long %>%
  drop_na(SEX) %>%
  ggplot(aes(x = SEX,
             y = N_SYMPTOMS, 
             fill = SEX)) +
  geom_boxplot(outlier.shape = NA, 
               alpha = 0.8) +
  scale_x_discrete(name = "Sex",
                   labels = c("Females", "Males")) +
  scale_y_continuous(limits = c(0, 15), 
                     name = "Number of symptoms") +
  coord_flip() +
  ggtitle("Sex vs. number of symptoms") +
  scale_fill_viridis_d() +
  theme_half_open(font_size = 9, 
                  font_family = "serif") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 40, 10, 10))

# Boxplot showing total number of symptoms by vaccine manufacturer
nsymptoms_v_manu <- merged_data_long %>%
  ggplot(aes(x = VAX_MANU,
             y = N_SYMPTOMS, 
             fill = VAX_MANU)) +
  geom_boxplot(outlier.shape = NA, 
               alpha = 0.8) +
  scale_x_discrete(name = "Vaccine manufacturer") +
  scale_y_continuous(limits = c(0, 15), 
                     name = "Number of symptoms") +
  coord_flip() +
  ggtitle("Manufacturer vs. number of symptoms") +
  scale_fill_viridis_d() +
  theme_half_open(font_size = 9, 
                  font_family = "serif, Times") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))

# Combine all number of symptom plots into one figure using patchwork
nsymptoms_age_sex <- nsymptoms_v_age + nsymptoms_v_sex


############################## TYPES OF SYMPTOMS ##############################

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
  labs(fill = "Sex") +
  ggtitle("Sex vs. types of symptoms") +
  xlab("Symptoms") + 
  ylab("Relative occurence") +
  scale_fill_viridis_d() +
  theme_half_open(font_size = 9, 
                  font_family = "serif, Times") +
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
  labs(fill = "Vaccine manufacturer") +
  ggtitle("Manufacturer vs. types of symptoms") +
  xlab("Symptoms") + 
  ylab("Relative occurence") +
  scale_fill_viridis_d() +
  theme_half_open(font_size = 9, 
                  font_family = "serif, Times") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 9), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))


# Heatmap showing the relative occurence of top 20 symptoms by age. 
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
             y = reorder(SYMPTOM, desc(prop)),
             fill = prop)) +
  geom_tile() +
  scale_x_discrete(name = "Age (years)", 
                   labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                              "50-60", "60-70", "70-80", "80-90", "90-120")) +
  ylab("Top 20 symptoms") +
  labs(fill = "Relative occurence") +
  ggtitle("Age vs. types of symptoms") +
  scale_fill_viridis_c() +
  theme_half_open(font_size = 9, 
                  font_family = "serif, Times") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 10), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))


####################### VACCINE MANUFACTURER VS DEATH ########################

# Plot vaccine manufacturer vs death as bar plot. 
# Counts of deaths per group (per vaccine) are relative to the number of 
# subjects in the group. 
manu_v_death <- merged_data_long %>%
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
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Relative occurence of death by vaccine manufacturer") +
  xlab("Vaccine manufacturer") +
  ylab("Relative occurence of death") +
  coord_flip() +
  scale_fill_viridis_d() +
  theme_half_open(font_size = 9, 
                  font_family = "serif, Times") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), # center title
        plot.margin = margin(10, 10, 10, 10))



###################### AGE DISTRIBUTION OF PEOPLE WHO ... ######################

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
write_tsv(...)
ggsave(...)

ggsave (age_dist, file = "results/age_dist.png")
ggsave (sex_dist, file = "results/sex_dist.png")
ggsave (vac_dist, file = "results/vac_dist.png")
ggsave(nsymptoms_age_sex, file = "results/nsymptoms_age_sex.png")
ggsave(nsymptoms_v_manu, file = "results/nsymptoms_v_manu.png")
ggsave(symptom_types_v_age, file = "results/symptom_types_v_age.png")
ggsave(symptom_types_v_sex, file = "results/symptom_types_v_sex.png")
ggsave(symptom_types_v_manu, file = "results/symptom_types_v_manu.png")
ggsave(manu_v_death, file = "results/manu_v_death.png")



