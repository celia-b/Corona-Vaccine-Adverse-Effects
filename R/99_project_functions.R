# Load data ------------------------------------------------
symptoms_clean <- read_csv(file = gzfile("data/02_symptoms_clean.csv.gz"))


# Define project functions ------------------------------------------------

## 1. top_n_symptoms_func() ---------------------------------------------------------

top_n_symptoms_func <- function(data = symptoms_clean, n_symp = 20) {
  # Returns a vector of the top n most occurring symptoms from a data set. 
  # :param data: data set containing identifier "VAERS_ID" and variables "SYMPTOMX" 
  #  with symptoms as character values. Default is "02_symptoms_clean.csv.gz"
  # :param n_symp: number of top occurring symptoms to extract. Default is 20. 
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


## 2. format_func() ---------------------------------------------------------

format_func <- function(x) {
  # Capitalizes letters and replaces spaces with _ for elements of a vector.
  # :param x: vector containing string elements.  
  str_to_upper(x) %>% 
  gsub(" ", 
       "_", 
       .)
}


## chisq_func() ---------------------------------------------------------

chisq_func <- function(variable1, variable2) {
  # Performs a Pearson's Chi-squared contingency table test between two variables,
  # variable1 and variable2
  variable1 <- enquo(variable1) 
  variable2 <- enquo(variable2) 
  merged_data_wide %>%
    group_by(!!variable1, 
             !!variable2) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = !!variable2,
                values_from = n) %>%
    tibble() %>% 
    select(-!!variable1) %>% 
    chisq.test() %>% 
    tidy()
}  


