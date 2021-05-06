# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library ("cowplot")
library("patchwork")
library("scales")
library("broom")
library("purrr")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


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


# Model data --------------------------------------------------------------

######### Modeling death outcome vs sex, age, n hospital days, n days before 
# symptoms presence of allergies, presence of illness, presence of COVID ######
logistic_regression <- merged_data_wide %>%
  glm(formula = DIED ~ 
        SEX + AGE_YRS + HOSPDAYS + SYMPTOMS_AFTER + HAS_ALLERGIES + HAS_ILLNESS + HAS_COVID, 
      family = binomial, 
      data = .)

summary(logistic_regression)


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
      formula = as.formula(paste("DEATH ~ ", paste(symptoms, collapse = "+"))), 
      family = binomial)


# Visualize significant symptoms
tidy(death_v_symptoms) %>%
  filter(term != "(Intercept)") %>%
  filter(p.value < 0.05) %>%
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


## By age class --> I think we need less age classes
# Also I'm not convinced this plot looks nice --> maybe facet_wrap it?
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


# Putting them together
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



######################## DEATH RATE ###############################
# Out of the people who died, what proportion were already sick?
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  filter(DIED == 'Y') %>%
  ggplot(aes(HAS_ILLNESS)) + 
  geom_bar() +
  coord_flip()

# How much more likely is it to die when you were already sick when 
# taking the vaccine vs when you were healthy? --> DO STATISTICAL ANALYSIS
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  group_by(HAS_ILLNESS) %>%
  count()

merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_ILLNESS, VAX_MANU) %>%
  group_by(HAS_ILLNESS, DIED) %>%
  count()

prop_test(x = c(1191, 644), n = c(29127, 4306), 
          p = NULL, alternative = "two.sided", correct = TRUE)


# Out of the people who died, what proportion had covid at the time?
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_COVID, VAX_MANU) %>%
  filter(DIED == 'Y') %>%
  ggplot(aes(HAS_COVID)) + 
  geom_bar() +
  coord_flip()


# How much more likely is it to die when you had covid while 
# taking the vaccine vs when you were healthy? --> DO STATISTICAL ANALYSIS
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_COVID, VAX_MANU) %>%
  group_by(HAS_COVID) %>%
  count()

merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAS_COVID, VAX_MANU) %>%
  group_by(HAS_COVID, DIED) %>%
  count()

# How much less likely is it to die when you had covid in the past? 
# --> DO STATISTICAL ANALYSIS
merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAD_COVID, VAX_MANU) %>%
  group_by(HAD_COVID) %>%
  count()

merged_data_wide %>%
  select(VAERS_ID, AGE_CLASS, SEX, DIED, HAD_COVID, VAX_MANU) %>%
  group_by(HAD_COVID, DIED) %>%
  count()


#################### SEX VS NUMBER/TYPES OF SYMPTOMS ####################

# Bar plot showing the relative occurrence of the top 20 symptoms by sex. 
# As there is not an equal number of males and females in the study, the
# counts are relative to the respective sex. 
merged_data_long %>%
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


##################### Principal component analysis #############################

# Change all variables I need to numeric and do PCA
pca_fit <- merged_data_wide %>% 
  mutate (DIED = case_when (DIED == 'N'~ 0,
                            DIED == 'Y'~ 1)) %>%
  mutate (L_THREAT = case_when (L_THREAT == 'N'~ 0,
                                L_THREAT == 'Y'~ 1)) %>%
  mutate (HOSPITAL = case_when (HOSPITAL == 'N'~ 0,
                                HOSPITAL == 'Y'~ 1)) %>%
  mutate (DISABLE = case_when (DISABLE == 'N'~ 0,
                                DISABLE == 'Y'~ 1)) %>%
  mutate (ER_ED_VISIT = case_when (ER_ED_VISIT == 'N'~ 0,
                                ER_ED_VISIT == 'Y'~ 1)) %>%
  mutate (DYSPNOEA = case_when (DYSPNOEA == FALSE ~ 0,
                                DYSPNOEA == TRUE ~ 1)) %>%
  mutate (PAIN_IN_EXTREMITY = case_when (PAIN_IN_EXTREMITY == FALSE ~ 0,
                                PAIN_IN_EXTREMITY == TRUE ~ 1)) %>%
  mutate (DIZZINESS = case_when (DIZZINESS == FALSE ~ 0,
                                DIZZINESS == TRUE ~ 1)) %>%
  mutate (FATIGUE = case_when (FATIGUE == FALSE ~ 0,
                                FATIGUE == TRUE ~ 1)) %>%
  mutate (INJECTION_SITE_ERYTHEMA = case_when (INJECTION_SITE_ERYTHEMA == FALSE ~ 0,
                                INJECTION_SITE_ERYTHEMA == TRUE ~ 1)) %>%
  mutate (INJECTION_SITE_PRURITUS = case_when (INJECTION_SITE_PRURITUS == FALSE ~ 0,
                                               INJECTION_SITE_PRURITUS == TRUE ~ 1)) %>%
  mutate (INJECTION_SITE_SWELLING = case_when (INJECTION_SITE_SWELLING == FALSE ~ 0,
                                               INJECTION_SITE_SWELLING == TRUE ~ 1)) %>%
  mutate (CHILLS = case_when (CHILLS == FALSE ~ 0,
                               CHILLS == TRUE ~ 1)) %>%
  mutate (RASH = case_when (RASH == FALSE ~ 0,
                               RASH == TRUE ~ 1)) %>%
  mutate (HEADACHE = case_when (HEADACHE == FALSE ~ 0,
                               HEADACHE == TRUE ~ 1)) %>%
  mutate (INJECTION_SITE_PAIN = case_when (INJECTION_SITE_PAIN == FALSE ~ 0,
                               INJECTION_SITE_PAIN == TRUE ~ 1)) %>%
  mutate (NAUSEA = case_when (NAUSEA == FALSE ~ 0,
                               NAUSEA == TRUE ~ 1)) %>%
  mutate (PAIN = case_when (PAIN == FALSE ~ 0,
                               PAIN == TRUE ~ 1)) %>%
  mutate (PYREXIA = case_when (PYREXIA == FALSE ~ 0,
                               PYREXIA == TRUE ~ 1)) %>%
  mutate (MYALGIA = case_when (MYALGIA == FALSE ~ 0,
                               MYALGIA == TRUE ~ 1)) %>%
  mutate (ARTHRALGIA = case_when (ARTHRALGIA == FALSE ~ 0,
                               ARTHRALGIA == TRUE ~ 1)) %>%
  mutate (PRURITUS = case_when (PRURITUS == FALSE ~ 0,
                               PRURITUS == TRUE ~ 1)) %>%
  mutate (ASTHENIA = case_when (ASTHENIA == FALSE ~ 0,
                               ASTHENIA == TRUE ~ 1)) %>%
  mutate (VOMITING = case_when (VOMITING == FALSE ~ 0,
                               VOMITING == TRUE ~ 1)) %>% # didn't add death bc it's already in DIED
  select (DIED, L_THREAT, HOSPITAL, DISABLE, ER_ED_VISIT, SYMPTOMS_AFTER,
          N_SYMPTOMS, DYSPNOEA, PAIN_IN_EXTREMITY, DIZZINESS, FATIGUE, 
          INJECTION_SITE_ERYTHEMA, INJECTION_SITE_PRURITUS, INJECTION_SITE_SWELLING,
          CHILLS, RASH, HEADACHE, INJECTION_SITE_PAIN, NAUSEA, PAIN, PYREXIA, 
          MYALGIA, ARTHRALGIA, PRURITUS, ASTHENIA, VOMITING) %>%
  drop_na(SYMPTOMS_AFTER) %>% # there are NAs in this column
  select(where(is.numeric)) %>% # retain only numeric columns
  scale() %>% # scale data
  prcomp(center = TRUE) # do PCA

# instead of doing this the NAs in SYMPTOMS_AFTER should be removed in 
# 02_clean??
classes <- merged_data_wide %>% 
  drop_na(SYMPTOMS_AFTER) %>%
  select (VAX_MANU)  

# PC1 vs PC2 biplot
pca_fit %>%
  augment(classes) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, color = VAX_MANU)) + 
  geom_point(size = 0.5) +
  labs(x = 'PC1', y = 'PC2') +
  scale_color_viridis_d(name = "MANUFACTURER", option = "D") +
  theme_half_open(font_size = 9, font_family = "serif,Times") +
  background_grid() 
  
# define arrow style for plotting
arrow_style <- arrow(
  angle = 10, ends = "first", type = "open", length = grid::unit(5, "pt")
)

# plot rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 0, nudge_x = 0.05, 
    color = "#904C2F",
    size = 2.5) +
  xlim(-.75, .75) + ylim(-.75, .75) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(10)

# scree plot
pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  labs(y = "explained variance") +
  scale_x_continuous(breaks = 1:26) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(10)

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








>>>>>>> 9ab903ad1d4b6728017eccee3466907908f1ba51

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)



