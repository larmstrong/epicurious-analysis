
# This script is responsible for preparing the raw Epicurious data for
# further statistical analysis. The raw data file is assumed to be located in
# the current working directory.

library(dplyr)

recipe.filename <- "epi_r.csv"

# On read, specifying "stringsAsFactors = FALSE" will prevent the title from
# being coerced into factors. 
recipe.data <- read.csv(recipe.filename,
                        header = TRUE, 
                        stringsAsFactors = FALSE)

# Remove rows of "bad" data, herein defined to be rows that have no data in
# one or more of the five quantitative categories.
recipe.data <- recipe.data %>%
  filter(!is.na(recipe.data$rating) &
           !is.na(recipe.data$calories) &
           !is.na(recipe.data$protein) &
           !is.na(recipe.data$fat) &
           !is.na(recipe.data$sodium))

## Next steps: Remove outlier data
