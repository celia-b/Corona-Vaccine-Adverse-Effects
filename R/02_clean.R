# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
patients <- read_csv(file = "data/01_patients.csv", 
                     col_types = cols("BIRTH_DEFECT" = col_character(),
                                      "X_STAY" = col_character(),
                                      "RPT_DATE" = col_date(format="%Y-%m-%d"),
                                      "V_FUNDBY" = col_character(),
                                      "ER_VISIT" = col_character()))
symptoms <- read_csv(file = "data/01_symptoms.csv")
vaccines <- read_csv(file = "data/01_vaccines.csv")



# Wrangle data ------------------------------------------------------------

################################## PATIENTS ##################################
patients_clean <- patients %>%
  select(-c(CAGE_YR, 
            CAGE_MO,
            RPT_DATE,
            SYMPTOM_TEXT,
            LAB_DATA,
            OFC_VISIT,
            ER_VISIT, 
            X_STAY, 
            V_FUNDBY, 
            BIRTH_DEFECT,
            SPLTTYPE)) %>% # Removed columns
  replace_na(list(DIED = "N",
                  L_THREAT = "N",
                  HOSPITAL = "N",
                  DISABLE = "N",
                  OFC_VISIT = "N",
                  ER_ED_VISIT = "N")) %>% # Handled NAs that are actually "No"
  mutate(AGE_YRS = as.integer(AGE_YRS)) # Age to integers



################################## VACCINES ##################################
sum(duplicated(vaccines)) # 30 duplicated rows in dataframe
# There should not be any rows with the duplicates of VAERS_ID and VAX_LOT...
vaccines %>% distinct(VAERS_ID, VAX_LOT) 

vaccines <- vaccines %>%  # Vaccines, vaccines_clean?? we need naming convention
  filter (VAX_TYPE == "COVID19") %>% # Keep only COVID vaccines
  distinct () %>% # removes duplicates (same values for all variables)
  add_count (VAERS_ID) %>% 
  filter (n==1) %>% # only keep non-repeated IDs (remove duplicated IDs that had different vaccine or lot)
  select (-n) # remove count column

View(vaccines)

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

# ?
patients %>% filter (SEX == "U") %>% count()
# 898 patients have sex = "U" - should we delete?
# According to VAERS it should be blank:
# Sex (SEX):Sex of the vaccine recipient (M = Male, F = Female, Unknown = Blank).

################################## SYMPTOMS ##################################

# Remove symptom versions
symptoms_clean <- symptoms %>%
  select(VAERS_ID, SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5)



# Write data --------------------------------------------------------------
write_csv(x = symptoms_clean,
          file = "data/02_symptoms_clean.csv")

write_csv(x = patients_clean,
          file = "data/02_patients_clean.csv")

