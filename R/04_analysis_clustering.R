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
library("tidymodels")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

# Wide format
merged_data_wide <- read_csv(file = gzfile("data/03_merged_data_wide.csv.gz"),
                             col_types = cols(VAX_DOSE_SERIES = col_character()))

# Wrangle data ------------------------------------------------------------

# Get symptom names, so symptom columns can be referred to as all_of(symptoms)
symptoms <- merged_data_wide %>% 
  select(DYSPNOEA, PAIN_IN_EXTREMITY, DIZZINESS, FATIGUE, 
         INJECTION_SITE_ERYTHEMA, INJECTION_SITE_PRURITUS, INJECTION_SITE_SWELLING, 
         CHILLS, RASH, HEADACHE, INJECTION_SITE_PAIN, NAUSEA,PAIN, PYREXIA, MYALGIA,
         ARTHRALGIA, PRURITUS, ASTHENIA, VOMITING, DEATH) %>%
  names()

# Convert symptom related variables to numeric
numeric_symptoms <- merged_data_wide %>% 
  mutate(DIED = case_when (DIED == "N"~ 0,
                           DIED == "Y"~ 1)) %>%
  mutate(HOSPITAL = case_when (HOSPITAL == "N"~ 0,
                               HOSPITAL == "Y"~ 1)) %>%
  mutate(DISABLE = case_when (DISABLE == "N"~ 0,
                              DISABLE == "Y"~ 1)) %>%
  mutate_if(is.logical, as.numeric) %>%
  select(all_of(symptoms)) %>%
  drop_na()

# Get classes (vaccine manufacturer)
classes <- merged_data_wide %>% 
  drop_na(all_of(symptoms)) %>%
  select(VAX_MANU)


# Principal component analysis -------------------------------------------------

pca_fit <- numeric_symptoms %>% 
  scale() %>% # scale data
  prcomp(center = TRUE) # do PCA

# PC1 vs PC2 biplot
biplot <- pca_fit %>%
  augment(classes) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2, color = VAX_MANU)) + 
  geom_point(size = 0.5) +
  labs(x = 'PC1', y = 'PC2') +
  scale_color_viridis_d(name = "MANUFACTURER", option = "D") +
  theme_half_open(font_size = 9, font_family = "Avenir") +
  background_grid() 
  
biplot

# define arrow style for plotting
arrow_style <- arrow(angle = 10, ends = "first", type = "open", 
                     length = grid::unit(5, "pt"))

# plot rotation matrix
rotation_matrix <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 0, nudge_x = 0.05, 
    color = "#904C2F",
    size = 2.5) +
  xlim(-.5, .5) + ylim(-.5, .5) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(10)

rotation_matrix

# scree plot
scree_plot <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  labs(y = "explained variance") +
  scale_x_continuous(breaks = 1:25) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(10)

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
  
# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)

ggsave (biplot, file = "results/biplot.png")
ggsave (rotation_matrix, file = "results/rotation_matrix.png")
ggsave (scree_plot, file = "results/scree_plot.png")


