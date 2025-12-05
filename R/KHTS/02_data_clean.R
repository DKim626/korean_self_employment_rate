#### ==== 0. Initial set up ====
library(dplyr)
library(stringr)
library(rlang)
library(purrr)
library(stringi)
library(data.table)
library(readr)
library(here)
library(assertthat)

# Loads clean_one_year(year)
# 1) Loads raw data in .rds format
# 2) Selects and renames variables
# 3) Recodes variables
# 4) Constructs self-employment rate
source(here('R/KHTS/module/cleaning/clean_one_year.R'), local = environment())

#### ==== 1. Year 1981 - 2024, monthly data ====
# Loops through survey data from 1981 to 2024
# survey_number: two digit identifier for year of survey
# survey_year: year of survey
# - Load .rdata for each year
# - Select and rename variables
# - Recode variables
# - Construct self-employment variable
#### run_data_cleaning ####
#' Executes data cleaning process of given time span.
#' 
#' @param start_year Numeric value for the start of the survey year (eg. 1981)
#' @param end_year Numeric value for the end of the survey year (eg. 2024)
run_data_cleaning <- function(start_year, end_year) {
  if(!file.exists(here('data/KHTS/clean'))) {
    dir.create(here('data/KHTS/clean'))
    message("[INFO] File directory data/KHTS/clean/ created.")
  }
  
  start_time <- proc.time()
  
  safe_clean <- safely(clean_one_year)
  
  results <- walk(start_year:end_year,
                 ~ safe_clean(.x))
  
  failed_years <- map(results, "error") |> keep( ~ !is.null(.x))
  
  if (length(failed_years)) {
    message("[WARNING] Cleaning failed for the following years:")
    walk2(names(failed_years), failed_years, ~ 
            message("  - ", .x, ": ", .y$message))
  }
  
  elapsed_time <- (proc.time() - start_time)[3]
  elapsed_min <- floor(elapsed_time / 60)
  elapsed_sec <- round(elapsed_time %% 60, 2)
  
  message("[INFO] Finished cleaning all years ", start_year, '-', end_year)
  message(sprintf(
    "[INFO] Data cleaning execution time: %02d minutes %02.2f seconds.",
    elapsed_min, elapsed_sec))
  
}

