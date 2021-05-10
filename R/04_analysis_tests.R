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


# Wrangle data ------------------------------------------------------------

# Convert variables to factors
merged_data_wide <- merged_data_wide %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.logical, as.factor)



# Model data --------------------------------------------------------------

## Proportion tests for DIED vs. different variables

# Null hypothesis: the proportions are the same. The two variables are independent.
# Assumptions:
# Data in contingency table is presented in counts (not in percent)
# All cells contain more than 5 observations
# Each observation contributes to one group only
# Groups are independent
# The variables under study are categorical
# The sample is, supposedly, reasonably random


# Run Chi-squared tests and extract p-values
manu_v_died_test <- chisq_func(DIED, VAX_MANU)
manu_v_died_pval <- manu_v_died_test %>%
  pluck("p.value") %>%
  format.pval(digits = 2)

sex_v_died_test <- chisq_func(DIED, SEX)
sex_v_died_pval <- sex_v_died_test %>%
  pluck("p.value") %>% 
  format.pval(digits = 2)


# Using the infer library, it can be done like this:
chisq_test(merged_data_wide, DIED ~ VAX_MANU) 
chisq_test(merged_data_wide, DIED ~ SEX) 


# Contingency tables:
merged_data_wide %>%
  group_by(DIED, VAX_MANU) %>%
  summarise(n = n()) %>%
  spread(VAX_MANU, n) %>% 
  tibble() 

merged_data_wide %>%
  filter(!is.na(SEX)) %>% 
  group_by(DIED, SEX) %>%
  summarise(n = n()) %>%
  spread(SEX, n) %>% 
  tibble() 



## Visualizations ----------------------------------------------

# Vaccine manufacturer vs. death bar plot
# Counts of deaths per group (per vaccine) are relative to the number of 
# subjects in the group. 
manu_v_death <- merged_data_wide %>%
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
           stat = "identity",
           alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Relative occurence of death by vaccine manufacturer",
       subtitle = str_c("p-value:", manu_v_died_pval), 
       x = "Vaccine manufacturer",
       y = "Relative occurence of death") +
  theme_minimal(base_family = "Avenir") +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 10),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))
  


# Sex vs. death bar plot
# Counts of deaths per group (per vaccine) are relative to the number of 
# subjects in the group. 
sex_v_death <- merged_data_wide %>%
  filter(!is.na(SEX)) %>%
  count(SEX, DIED) %>%
  group_by(SEX) %>%
  mutate(total = sum(n)) %>%
  filter(DIED == "Y") %>%
  summarise(prop = n/total, .groups = "rowwise") %>%
  ggplot(.,
         aes(x = fct_reorder(SEX, desc(prop)),
             y = prop,
             fill = SEX, 
             drop_na = TRUE)) +
  geom_bar(position = "dodge",
           stat = "identity",
           alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  labs(title = "Relative occurence of death by vaccine manufacturer",
       subtitle = str_c("p-value:", sex_v_died_pval),
       x = "Sex",
       y = "Relative occurence of death") +
  theme_minimal(base_family = "Avenir") +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))




# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)

# Save manufacturer/sex vs. death plots
ggsave(manu_v_death, 
       file = "results/manu_v_death.png",
       height = 5,
       width = 8)

ggsave(sex_v_death,
       file = "results/sex_v_death.png",
       height = 5,
       width = 8)


