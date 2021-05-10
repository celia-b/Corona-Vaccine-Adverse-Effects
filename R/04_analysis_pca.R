# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Wide format
merged_data_wide <- read_csv(file = gzfile("data/03_merged_data_wide.csv.gz"),
                             col_types = cols(VAX_DOSE_SERIES = col_character()))

# Wrangle data ------------------------------------------------------------

# Use top_n_symptoms_func() to get vector of top 20 symptoms occurring in data set.
# Use format_func() to capitalize vector elements and replace spaces with _
symptoms <- top_n_symptoms_func(data = symptoms_clean, 
                                n_symp = 20,
                                VAERS_ID = VAERS_ID) %>%
  format_func()


# Convert symptom-related variables to numeric values (FALSE/N = 0, TRUE/Y = 1)
numeric_symptoms <- merged_data_wide %>% 
  mutate(HOSPITAL = case_when(HOSPITAL == "N" ~ 0,
                              HOSPITAL == "Y" ~ 1)) %>%
  mutate(DISABLE = case_when(DISABLE == "N" ~ 0,
                             DISABLE == "Y" ~ 1)) %>%
  mutate(ER_ED_VISIT = case_when(ER_ED_VISIT == "N" ~ 0,
                                 ER_ED_VISIT == "Y" ~ 1)) %>%
  mutate_if(is.logical, 
            as.numeric) %>%
  select(all_of(symptoms), 
         HOSPITAL, 
         DISABLE, 
         ER_ED_VISIT, 
         SYMPTOMS_AFTER, 
         N_SYMPTOMS) %>%
  drop_na()


# Get tibble containing the vaccine manufacturer of each patient. 
# To be used as classes (labels) for the PCA.
classes <- merged_data_wide %>% 
  drop_na(all_of(symptoms), HOSPITAL, DISABLE, ER_ED_VISIT, 
          SYMPTOMS_AFTER, N_SYMPTOMS) %>%
  select(VAX_MANU)


# 1. Principal component analysis -------------------------------------------------

# Do PCA on symptoms with numeric format
pca_fit <- numeric_symptoms %>% 
  prcomp(scale = TRUE, 
         center = TRUE)


## 1.1 PCA plot  -----------------------------------------------------------

# Plot PC1 vs. PC2 and color by vaccine manufacturer
pca_plot <- pca_fit %>%
  augment(classes) %>% 
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             color = VAX_MANU)) + 
  geom_point(size = 0.5) +
  labs(title = "PCA biplot",
       x = "PC1", 
       y = "PC2") +
  scale_color_viridis_d(name = "Vaccine manufacturer", 
                        option = "D") +
  theme_minimal(base_family = "Avenir") +
  theme(plot.title = element_text(hjust = 0.5))


## 1.2 Rotation matrix -------------------------------------------------------------

# Define arrow style for plotting rotation matrix
arrow_style <- arrow(angle = 10, 
                     ends = "first", 
                     type = "open", 
                     length = grid::unit(5, "pt"))

# Extract the rotation matrix using tidy() from broom and then plot it
rotation_matrix <- pca_fit %>%
  tidy(matrix = "rotation") %>% # extract rotation matrix using tidy() from broom
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value") %>%
  ggplot(aes(x = PC1, 
             y = PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style) +
  geom_text(aes(label = column),
            position = position_jitter(),
            color = "#904C2F",
            size = 2.5) +
  xlim(-0.5, 0.5) + 
  ylim(-0.5, 0.5) +
  coord_fixed() + # fix aspect ratio to 1:1
  labs(title = "Rotation matrix") +
  theme_minimal(base_family = "Avenir",
                base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))



## 1.3 Scree plot ------------------------------------------------------------------

# Use tidy() to get eigenvalues and use these to make scree plot
scree_plot <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>% #
  ggplot(aes(x = PC, 
             y = percent)) +
  geom_col(alpha = 0.7, 
           fill = "#00846b") +
  labs(title = "Scree plot",
       subtitle = "Percentage of variance explained by each principal component",
       y = "Explained variance") +
  scale_x_continuous(breaks = 1:25,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_minimal(base_family = "Avenir",
                base_size = 10) +
  theme(plot.title = element_text(hjust = 0))


  
# Write data -------------------------------------------------------------------

# Save PCA plot, rotation matrix plot and scree plot
ggsave(pca_plot, 
        file = "results/pca_plot.png", 
        height = 6,
        width = 10)

ggsave(rotation_matrix, 
       file = "results/rotation_matrix.png", 
       height = 7,
       width = 8)

ggsave(scree_plot, 
       file = "results/scree_plot.png",
       height = 5,
       width = 10)


