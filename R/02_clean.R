# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
vaccines <- read_csv(file = "data/01_vaccines.csv")
patients <- read_csv(file = "data/01_patients.csv")


# Wrangle data ------------------------------------------------------------
#my_data_clean <- my_data # %>% ...


############################### VACCINES Dataset ###############################
View(vaccines)

sum(duplicated(vaccines)) # 30 duplicated rows in dataframe
vaccines <- vaccines %>% distinct() # Remove duplicate rows based on all columns


### Check each column one by one and clean up
vaccines %>% count(VAX_MANU)
# There are 18 rows with "UNKNOWN MANUFACTURER" of vaccine; these are deleted:
vaccines <- vaccines %>% subset(VAX_MANU != "UNKNOWN MANUFACTURER")
# Rename PFIZER\\BIONTECH so that we can check for consistency
vaccines <- vaccines %>% mutate(VAX_MANU = recode(VAX_MANU, "PFIZER\\BIONTECH" = "PFIZER-BIONTECH")) 
# We have 3 vaccine manufacturers in the dataset:
vaccines %>% distinct(VAX_MANU) 
#1 "MODERNA"         
#2 "PFIZER\\BIONTECH"
#3 "JANSSEN"

vaccines %>% distinct(VAX_DOSE_SERIES)
# Replace the strings "N/A" and "UNK" with NA values
vaccines <- vaccines %>% mutate(VAX_DOSE_SERIES = na_if(VAX_DOSE_SERIES, "UNK"))
vaccines <- vaccines %>% mutate(VAX_DOSE_SERIES = na_if(VAX_DOSE_SERIES, "N/A"))

vaccines %>% distinct(VAX_ROUTE) 
# All values OK

vaccines %>% distinct(VAX_SITE)
# All values OK

pattern <- "COVID19\\s\\(COVID19\\s\\((\\w++\\-*\\w*)\\)"# Regular expression matching the name of the vaccine manufacturer
vaccines <- vaccines %>% mutate(VAX_NAME_extracted = str_match(VAX_NAME, pattern)[,2]) #%>% select(VAX_NAME, VAX_NAME_extracted)
vaccines <- vaccines %>% mutate(comparison = if_else(VAX_MANU == VAX_NAME_extracted, TRUE, FALSE)) 
# Same number of rows:
vaccines %>% count(comparison)
nrow(vaccines)
# VAX_MANU and VAX_NAME are matching so we can delete the following columns:
# VAX_NAME, VAX_NAME_extracted, comparison
vaccines <- vaccines %>%
  select(-c("VAX_NAME", "VAX_NAME_extracted", "comparison"))

# Clean VAX_LOT column


# There should not be any rows with the duplicates of VAERS_ID and VAX_LOT...
vaccines %>% distinct(VAERS_ID, VAX_LOT) 

################################################################################



patients %>% count (VAERS_ID, sort = TRUE)

patients %>% filter (SEX == "U") %>% count()
# 898 patients have sex = "U" - should we delete?
# According to VAERS it should be blank:
# Sex (SEX):Sex of the vaccine recipient (M = Male, F = Female, Unknown = Blank).


# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")