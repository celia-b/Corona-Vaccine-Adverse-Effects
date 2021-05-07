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


<<<<<<< HEAD:R/04_analysis_i.R
# Model data --------------------------------------------------------------

######### Modeling death outcome vs sex, age, n hospital days, n days before 
# symptoms presence of allergies, presence of illness, presence of COVID ######
logistic_regression <- merged_data_wide %>%
  glm(formula = DIED ~ 
        SEX + AGE_YRS + HOSPDAYS + SYMPTOMS_AFTER + HAS_ALLERGIES + HAS_ILLNESS + HAS_COVID, 
      family = binomial, 
      data = .)

summary(logistic_regression)


logistic_regression_interactions <- merged_data_wide %>%
  glm(formula = DIED ~ 
        (SEX + AGE_YRS + HOSPDAYS + SYMPTOMS_AFTER + HAS_ALLERGIES + HAS_ILLNESS + HAS_COVID)^2, 
      family = binomial, 
      data = .)

summary(logistic_regression_interactions)
# Some of the main effects are not significant anymore


################# Modeling death vs presence/absence of symptoms ###############
# Can be done like this:
death_v_symptoms <- merged_data_wide %>%
  glm(data = ., 
      formula = DIED ~ 
        DYSPNOEA + PAIN_IN_EXTREMITY + DIZZINESS + FATIGUE + 
        INJECTION_SITE_ERYTHEMA + INJECTION_SITE_PRURITUS + 
        INJECTION_SITE_SWELLING + CHILLS + RASH + HEADACHE + INJECTION_SITE_PAIN +
        NAUSEA + PAIN + PYREXIA + MYALGIA + ARTHRALGIA + PRURITUS + ASTHENIA + 
        VOMITING, 
      family = binomial)


# Or like this (symptoms defined in wrangle section):
death_v_symptoms <- merged_data_wide %>%
  glm(data = ., 
      formula = str_c("DEATH ~ ", str_c(symptoms, collapse = "+")), 
      family = binomial)

summary(death_v_symptoms)


# Visualize significant symptoms
tidy(death_v_symptoms) %>%
  filter(term != "(Intercept)") %>%
  #filter(p.value < 0.05) %>%
  ggplot(aes(x = fct_reorder(term, p.value),
             y = -log(p.value))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = -log(0.05), 
             col = "red", 
             linetype="dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Symptoms significantly associated with death") +
  xlab("Symptoms") + 
  ylab("-log(p-value)")


# The "estimate" column is the log-odds ratio, so we must interpret them as follows:
# 1. If the variable is categorical, like HAS_ILLNESS, an estimate of 9.877e-01 for the "Y" group
#    means that this group is exp(9.877e-01) = 2.685052 times more likely to die after taking 
#    the vaccine than the reference 'N' group.
# 2. If the variable is continuous, like HOSPDAYS, an estimate of 6.024e-02
#    means that, holding all else constant, one unit change in HOSPDAYS will have 
#    exp(6.024e-02) = 1.062091 units change in the odds ratio.


=======
>>>>>>> 7c4c77d4862a0f8b3ac11d660d29ca9cbf9fd4d1:R/04_analysis_visualizations.R

# Visualise data ----------------------------------------------------------

######################## SYMPTOMS AFTER N DAYS ############################

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


#################### SEX VS NUMBER/TYPES OF SYMPTOMS ####################

# Bar plot showing the number of symptoms experienced by males and females. 
# The counts are relative to the respective genders. 
nsymptoms_v_sex <- merged_data_long %>%
  filter(!is.na(SEX)) %>%
  ggplot(aes(x = N_SYMPTOMS,
             fill = SEX,
             stat(prop))) +
  geom_bar(position = "dodge") + 
  scale_x_continuous(limits = c(0, 30), 
                     breaks = seq(0, 30, by = 5)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Females", "Males")) +
  ggtitle("Relative occurrence of number of symptoms by sex") +
  xlab("Number of symptoms") + 
  ylab("Relative occurence") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # center title 


# Bar plot showing the relative occurrence of the top 20 symptoms by sex. 
# As there is not an equal number of males and females in the study, the
# counts are relative to the respective sex. 
symptom_types_v_sex <- merged_data_long %>%
  count(SEX, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, SEX) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE, !is.na(SEX)) %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(aes(x = fct_reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = SEX)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Females", "Males")) +
  ggtitle("Relative occurence of top 20 symptoms by sex") +
  xlab("Symptoms") + 
  ylab("Relative occurence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5)) # center title 


############### VACCINE MANUFACTURER VS NUMBER/TYPES OF SYMPTOMS ###############

# Total number of symptoms by vaccine manufacturer
# Stacked bar chart
nsymptoms_v_manu <- merged_data_long %>%
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
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # center title 


# Relative occurence of top 20 symptoms by manufacturer
# Non-stacked bar chart
symptom_types_v_manu <- merged_data_long %>%
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
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Vaccine manufacturer") +
  ggtitle("Relative occurence of top 20 symptoms by manufacturer") +
  xlab("Symptoms") + 
  ylab("Relative occurence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5)) # center title 



####################### VACCINE MANUFACTURER VS DEATH ########################

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
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

manu_v_lthreat <- merged_data_long %>%
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
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Relative occurence of life threatening symptoms by vaccine manufacturer") +
  xlab("Vaccine manufacturer") +
  ylab("Relative occurence of life threatening symptoms") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) # center title


###################### AGE VS NUMBER/TYPES OF SYMPTOMS ######################

# Boxplot of number of symptoms vs age
# Works - nothing important?
age_v_nsymptoms_boxplot <- merged_data_long %>%
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
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # center title

# Bar plot showing the number of symptoms by age
# Stacked bar plot
# Consider grouping number of symptoms? 
# Change colors to blend together less
age_v_nsymptoms_stacked <- merged_data_long %>%
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
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # center title

# Relative occurence of top 20 symptoms by age
# Stacked bar plot
# Consider making bigger groups?
age_vs_symptom_types_stacked <- merged_data_long %>%
  drop_na(AGE_CLASS) %>%
  count(AGE_CLASS, SYMPTOM, SYMPTOM_VALUE) %>%
  group_by(SYMPTOM, AGE_CLASS) %>%
  mutate(total = sum(n)) %>%
  filter(SYMPTOM_VALUE == TRUE) %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(aes(x = reorder(SYMPTOM, desc(prop)),
             y = prop,
             fill = AGE_CLASS)) +
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Age groups (years)", 
                      labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                                 "50-60", "60-70", "70-80", "80-90", "90-120")) +
  ggtitle("Relative occurence of top 20 symptoms by age") +
  xlab("Symptoms") + 
  ylab("Relative occurence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
        plot.title = element_text(hjust = 0.5)) # center title



# Relative occurence of top 20 symptoms by age
# heat map
age_vs_symptom_types_heatmap <- merged_data_long %>%
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
  ggtitle("Relative occurrence of top 20 symptoms by age") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))



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

ggsave(nsymptoms_v_sex, file = "results/nsymptoms_v_sex.png")
ggsave(symptom_types_v_sex, file = "results/symptom_types_v_sex.png")
ggsave(nsymptoms_v_manu, file = "results/nsymptoms_v_manu.png")
ggsave(symptom_types_v_manu, file = "results/symptom_types_v_manu.png")
ggsave(manu_v_lthreat, file = "results/manu_v_lthreat.png")
ggsave(age_v_nsymptoms_boxplot, file = "results/age_v_nsymptoms_boxplot.png")
ggsave(age_v_nsymptoms_stacked, file = "results/age_v_nsymptoms_stacked.png")
ggsave(age_vs_symptom_types_stacked, file = "results/age_vs_symptom_types_stacked.png")
ggsave(age_vs_symptom_types_heatmap, file = "results/age_vs_symptom_types_heatmap.png")




