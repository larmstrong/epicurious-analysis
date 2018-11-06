
# This script is responsible for preparing the raw Epicurious data for
# further statistical analysis. The raw data file is assumed to be located in
# the current working directory.

library("dplyr")
library("stringi")

##---------------------------------------------------------------------------
## READ RECIPE DATA

# Local location of recipe CSV file from Kaggle
recipe.csv.filename <- "epi_r.csv"

# On read, specifying "stringsAsFactors = FALSE" will prevent the title from
# being coerced into factors. 
recipe.data <- read.csv(recipe.csv.filename,
                        header = TRUE, 
                        stringsAsFactors = FALSE)

# Record original data volume.
volume.data = c(original = nrow(recipe.data))

##---------------------------------------------------------------------------
## REMOVE RECORDS WITH "BAD" DATA

# Remove rows of "bad" data, herein defined to be rows that have no data in
# one or more of the five quantitative categories.
recipe.data <- recipe.data %>%
  filter(!is.na(recipe.data$rating) &
           !is.na(recipe.data$calories) &
           !is.na(recipe.data$protein) &
           !is.na(recipe.data$fat) &
           !is.na(recipe.data$sodium))

# Capture volume after removal of NAs
volume.data <- c(volume.data, na.removal = nrow(recipe.data))

# Remove recipes with values of 0 where such values should be impossible: 
# rating and calories.

# Since ratings start with 1 "fork" we remove any recipe with a 0 rating
recipe.data <- recipe.data %>%
  filter(recipe.data$rating > 0) 

# And remove any 0 calorie recipes
recipe.data <- recipe.data %>%
  filter(recipe.data$calories > 0) 

# Capture volume after removal of NAs
volume.data <- c(volume.data, na.removal = nrow(recipe.data))

## Next steps: Remove outlier data
categoriy.names = c("Rating", "Calories", "Protien", "Fat", "Sodium")
recipe.bp <- boxplot(recipe.data$rating, 
                     recipe.data$calories, 
                     recipe.data$protein, 
                     recipe.data$fat, 
                     recipe.data$sodium,
                     names = categoriy.names, 
                     plot = FALSE)
min.index <- 1
max.index <- 5
calories.index <- 2
protein.index  <- 3
fat.index      <- 4
sodium.index   <- 5
calories.low <- recipe.bp$stats[min.index, calories.index]
calories.hi  <- recipe.bp$stats[max.index, calories.index]
protein.low  <- recipe.bp$stats[min.index, protein.index]
protein.hi   <- recipe.bp$stats[max.index, protein.index]
fat.low      <- recipe.bp$stats[min.index, fat.index]
fat.hi       <- recipe.bp$stats[max.index, fat.index]
sodium.low   <- recipe.bp$stats[min.index, sodium.index]
sodium.hi    <- recipe.bp$stats[max.index, sodium.index]

recipe.data <- filter(.data = recipe.data, 
                      between(recipe.data$calories, calories.low,calories.hi))
recipe.data <- filter(.data = recipe.data, 
                      between(recipe.data$protein, protein.low, protein.hi))
recipe.data <- filter(.data = recipe.data, 
                      between(recipe.data$fat, fat.low, fat.hi))
recipe.data <- filter(.data = recipe.data, 
                      between(recipe.data$sodium, sodium.low, sodium.hi))

# Capture volume after removal of outliers
volume.data <- c(volume.data, outliers = nrow(recipe.data))

recipe.data <- 
  recipe.data[
    !duplicated(
      stri_join(
        recipe.data$title,
        recipe.data$calories, 
        recipe.data$sodium, 
        recipe.data$fat, 
        recipe.data$protein)), ]
volume.data <- c(volume.data, duplicates = nrow(recipe.data))

#############################################################################
# This section will run some tests to create a new data frame from the JSON
# data.

library("rjson")

jsonField.to.vector <- function (jsonData, fieldName) {
  unlist(
    lapply(
      jsonData,
      function(x) {
        if (is.null(x[[fieldName]])) { NA }
        else { x[[fieldName]] } # if
      } # anonymous function
    ) # lapply
  ) # unlist
} # jsonField.to.vector

recipe.json.filename <- "full_format_recipes.json"
recipe.extendedData <- fromJSON(file = recipe.json.filename)

title <- trimws(jsonField.to.vector(recipe.extendedData, "title"), 
                which = "both")
rating <- jsonField.to.vector(recipe.extendedData, "rating")
fat <- trimws(jsonField.to.vector(recipe.extendedData, "fat"), which = "both")
calories <- trimws(jsonField.to.vector(recipe.extendedData, "calories"),
                   which = "both")
