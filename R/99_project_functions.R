# Load data ------------------------------------------------
symptoms_clean <- read_csv(file = gzfile("data/02_symptoms_clean.csv.gz"))


# Define project functions ------------------------------------------------

## top_n_symptoms() ---------------------------------------------------------
top_n_symptoms <- function(data = symptoms_clean, n_symp = 20) {
  # returns a vector of the top n most occurring symptoms from a data set. 
  # :param data: dataset with symptoms
  # :param n_symp: number of symptoms to extract
symptoms_clean %>%
  pivot_longer(cols = -VAERS_ID, 
               names_to = "symptom_num", 
               values_to = "symptom", 
               values_drop_na = TRUE) %>%
  select(-symptom_num) %>%
  count(symptom, sort = TRUE) %>%
  head(n_symp) %>%
  pluck("symptom")
}


## capitalize() ---------------------------------------------------------
capitalize <- function(x) {
  # Capitalizes letters and replaces spaces with _ for elements of a vector
  # :param x: vector
  toupper(x) %>% 
  gsub(" ", "_", .)
}


## chisq_func() ---------------------------------------------------------
chisq_func <- function(variable1, variable2) {
  # performs a Pearson's Chi-squared contingency table test between two variables,
  # variable1 and variable2
  variable1 <- enquo(variable1) 
  variable2 <- enquo(variable2) 
  merged_data_wide %>%
    group_by(!!variable1, !!variable2) %>%
    summarise(n = n()) %>%
    spread(!!variable2, n) %>%  
    tibble() %>% 
    select(-!!variable1) %>% 
    chisq.test() %>% 
    tidy()
}  
  
  