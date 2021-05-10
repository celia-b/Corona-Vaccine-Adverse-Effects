
# Final Project in Course 22100 R For Bio Data Science, Spring 2021
![DTU](https://github.com/rforbiodatascience21/2021_group14_final_project/blob/main/DTU_Logo.jpg)

The goal of this collaborative bio data science project is to demonstrate our ability to use Tidyverse R to perform data cleansing, transformation, visualization, and communication. The focus is on reproducibility and following the tidyverse style guide.



## The Dataset
In this project, we are working with the **COVID-19 World Vaccine Adverse Reactions** Dataset, which is available on Kaggle: https://www.kaggle.com/ayushggarg/covid19-vaccine-adverse-reactions. The dataset comes from The Vaccine Adverse Event Reporting System (VAERS), established by The U.S. Department of Health and Human Services, and co-managed by the Centers for Disease Control and Prevention (CDC) and the U.S. Food and Drug Administration (FDA). It should be noted that VAERS data come from a passive surveillance system and represent unverified reports of health events that occur after vaccination with U.S.-licensed vaccines. Therefore, reports may include incomplete, inaccurate and coincidental information.  

The raw data consists of 3 CSV files:

- 2021VAERSDATA.csv (VAERS DATA) 
- 2021VAERSSYMPTOMS.csv (VAERS Symptoms) 
- 2021VAERSVAX.csv (VAERS Vaccine)
 


## Project Description

![Flowchart of the workflow](https://github.com/rforbiodatascience21/2021_group14_final_project/blob/main/doc/Flowchart.jpg)

The flowchart illustrates the data flow from the raw data to the final results. A big part of the project consists of data cleansing for which the VAERS Data Use Guide (https://vaers.hhs.gov/docs/VAERSDataUseGuide_November2020.pdf) was useful. All visualizations were made with ggplot, and the modeling includes PCA, logistic regression, and chi-squared contingency table tests.


**00_doit.R** runs all scripts at once and produces an ioslides presentation from R Markdown in HTML format. 




