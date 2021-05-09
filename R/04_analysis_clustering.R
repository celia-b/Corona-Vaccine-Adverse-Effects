# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("cowplot")
library("patchwork")
library("scales")
library("broom")
library("purrr")
library("infer")
library("tidymodels")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Wide format
merged_data_wide <- read_csv(file = gzfile("data/03_merged_data_wide.csv.gz"),
                             col_types = cols(VAX_DOSE_SERIES = col_character()))

# Wrangle data ------------------------------------------------------------

# Use top_n_symptoms() function to get vector of top 20 symptoms occurring in data set.
# Use capitalize() function to capitalize elements and replace spaces with _
symptoms <- top_n_symptoms(data = symptoms_clean, n_symp = 20) %>%
  capitalize()

# Convert symptom-related variables to numeric values (FALSE/N = 0, TRUE/Y = 1)
numeric_symptoms <- merged_data_wide %>% 
  mutate(HOSPITAL = case_when(HOSPITAL == "N"~ 0,
                              HOSPITAL == "Y"~ 1)) %>%
  mutate(DISABLE = case_when(DISABLE == "N"~ 0,
                             DISABLE == "Y"~ 1)) %>%
  mutate(ER_ED_VISIT = case_when(ER_ED_VISIT == "N"~ 0,
                                 ER_ED_VISIT == "Y"~ 1)) %>%
  mutate_if(is.logical, as.numeric) %>%
  select(all_of(symptoms), HOSPITAL, DISABLE, ER_ED_VISIT, 
         SYMPTOMS_AFTER, N_SYMPTOMS) %>%
  drop_na()

# Get classes (vaccine manufactures)
classes <- merged_data_wide %>% 
  drop_na(all_of(symptoms), HOSPITAL, DISABLE, ER_ED_VISIT, 
          SYMPTOMS_AFTER, N_SYMPTOMS) %>%
  select(VAX_MANU)


# Principal component analysis -------------------------------------------------

# Do PCA on symptoms with numeric format
pca_fit <- numeric_symptoms %>% 
  prcomp(scale = TRUE, 
         center = TRUE)

# PC1 vs PC2 biplot
biplot <- pca_fit %>%
  augment(classes) %>% 
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             color = VAX_MANU)) + 
  geom_point(size = 0.5) +
  labs(title = "PCA...",
       x = "PC1", 
       y = "PC2") +
  scale_color_viridis_d(name = "Vaccine manufacturer", 
                        option = "D") +
  theme_minimal(base_family = "Avenir") +
  theme(plot.title = element_text(hjust = 0.5))
  
biplot

# define arrow style for plotting rotation matrix
arrow_style <- arrow(angle = 10, 
                     ends = "first", 
                     type = "open", 
                     length = grid::unit(5, "pt"))

# Extract the rotation matrix using tidy() from broom
# Plot rotation matrix
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

rotation_matrix

# Use tidy() from broom to get eigenvalues and use these to make scree plot
scree_plot <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>% #
  ggplot(aes(x = PC, 
             y = percent)) +
  geom_col(alpha = 0.8, 
           fill = "#56B4E9") +
  labs(title = "Scree plot",
       y = "Explained variance") +
  scale_x_continuous(breaks = 1:25,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = expansion(mult = c(0, 0.01))) +
  theme_minimal(base_family = "Avenir",
                base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5))


scree_plot

# K-means clustering  -----------------------------------------------------

# only number of symptoms and symptoms_after because it doesn't make sense
# with binary variables 

kclust <- numeric_symptoms %>%
  select (SYMPTOMS_AFTER, N_SYMPTOMS) %>% 
  kmeans (centers = 3) %>% 
  augment (numeric_symptoms) %>%
  ggplot (aes (N_SYMPTOMS, SYMPTOMS_AFTER, color = .cluster)) +
  geom_point()
  
by_vac_manu <- merged_data_wide %>% 
  select (VAX_MANU, N_SYMPTOMS, SYMPTOMS_AFTER) %>%
  drop_na() %>%
  ggplot (aes (N_SYMPTOMS, SYMPTOMS_AFTER, color = VAX_MANU)) +
  geom_point()

kmeans_comparison <- kclust + by_vac_manu

kmeans_comparison
  
# Write data --------------------------------------------------------------

# Save biplot, rotation matrix plot and scree plot
ggsave(biplot, 
        file = "results/biplot.png", 
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


