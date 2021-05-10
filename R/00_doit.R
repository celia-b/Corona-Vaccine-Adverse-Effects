# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_analysis_visualizations.R")
source(file = "R/04_analysis_pca.R")
source(file = "R/04_analysis_regressions.R")
source(file = "R/04_analysis_tests.R")
rmarkdown::render("doc/presentation.Rmd", output_format = NULL)
