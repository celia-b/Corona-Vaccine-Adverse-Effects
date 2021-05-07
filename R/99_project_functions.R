# Define project functions ------------------------------------------------


# Function that performs a Pearson's Chi-squared contingency table test between two variables
chisq_func <- function(variable1, variable2) {
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