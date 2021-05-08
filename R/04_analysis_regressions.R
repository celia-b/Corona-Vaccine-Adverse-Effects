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

summary(logistic_regression_interactions) # Some of the main effects are not significant anymore


################# Modeling death vs presence/absence of symptoms ###############

# Make logistic model of death vs all symptoms
death_v_symptoms <- merged_data_wide %>%
  glm(data = ., 
      formula = str_c("DEATH ~ ", str_c(symptoms, collapse = "+")), 
      family = binomial)

# Visualize significant symptoms
death_v_symptoms_model_fig <- tidy(death_v_symptoms) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = fct_reorder(term, p.value),
             y = -log(p.value),
             fill = term)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = -log(0.05), 
             linetype = "dashed", 
             color = "black") +
  scale_fill_viridis_d() +
  labs(title = "P-values for death ~ symptoms association",
       subtitle = "Dashed line indicates a p-value of 0.05",
       x = "Symptoms",
       y = "-log(p-value)",
       fill = "Sex") +
  theme_minimal(base_family = "Avenir") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   size = 9), 
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10))


# Takes some time to run!
death_v_symptoms_interactions <- merged_data_wide %>%
  glm(data = ., 
      formula = DIED ~ 
        (DYSPNOEA + PAIN_IN_EXTREMITY + DIZZINESS + FATIGUE + 
           INJECTION_SITE_ERYTHEMA + INJECTION_SITE_PRURITUS + 
           INJECTION_SITE_SWELLING + CHILLS + RASH + HEADACHE + INJECTION_SITE_PAIN +
           NAUSEA + PAIN + PYREXIA + MYALGIA + ARTHRALGIA + PRURITUS + ASTHENIA + 
           VOMITING)^2, 
      family = binomial)

summary(death_v_symptoms_interactions)

# The "estimate" column is the log-odds ratio, so we must interpret them as follows:
# 1. If the variable is categorical, like HAS_ILLNESS, an estimate of 9.877e-01 for the "Y" group
#    means that this group is exp(9.877e-01) = 2.685052 times more likely to die after taking 
#    the vaccine than the reference 'N' group.
# 2. If the variable is continuous, like HOSPDAYS, an estimate of 6.024e-02
#    means that, holding all else constant, one unit change in HOSPDAYS will have 
#    exp(6.024e-02) = 1.062091 units change in the odds ratio.




# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)

ggsave(death_v_symptoms_model_fig, file = "results/death_v_symptoms_model_fig.png")




