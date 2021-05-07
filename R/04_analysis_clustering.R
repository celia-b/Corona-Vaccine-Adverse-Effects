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
  theme_half_open(font_size = 9, font_family = "serif, Times") +
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



# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)



