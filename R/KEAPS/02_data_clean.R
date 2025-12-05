############################################################################## #
# Data Cleaning for Korean Economically Active Population Survey (KEAPS)
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Pipeline:
# 1. 01_preprocess.R: Automized KOSIS data processing
#   - Runs each .R files provided by KOSIS
#   - Outputs raw data with variable names & codes
# 2. 02_data_cleaning.R: Standardizes variables, merges data
#   - Uses pipeline_clean.R for modularized cleaning
#   - Recodes key variables (econ_act, emp_type, industry code)
# 3. 03_rate_calculation.R
#   - Computes monthly self-employment rates (overall, by industry & groups)
# 4. 04_data_visualization.R:
#   - Loads yearly trends & generates plots
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Load processed KEAPS .rds file
# - Standardize variable names across years (1981 - 2024)
# - Extract employment, industry, demographic variables
# - Recode categorical variables Korean -> English
# - Save cleaned data at data/clean/
#
#----------------------------------------------------------------------------- -
#
# Notes:
# - KEAPS spans 1981-current
# - Pre-2000 raw data: inconsistent variable names/values
# - KSIC industry codes revised multiple times:
#   - 5th KSIC: 1986 - 1992
#   - 6th/7th KSIC: 1993 - 2000
#   - 8th KSIC: 2000 - 2008
#   - 9th KSIC: 2004 - 2017
#   - 10th, 11th KSIC: 2013 - 2024
#
#----------------------------------------------------------------------------- -
#
# Known issues:
# - There are coding outside of KSIC; consider as NA
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, stringr, rlang, purrr, stringi, data.table, readr, assertthat
# - .rds files must be stored in "data/processed" folder
#   - data_(yyyy).rds: survey data in year (yyyy)
# - clean_one_year.R: data cleaning for each year
#   - variable_select_rename.R: selects and renames variables
#   - variable_recode.R: recodes variables
#   - ksic_mapping.R: mapping for KSIC industry codes
#
#----------------------------------------------------------------------------- -
#
# Output Flow:
# - data_(yyyy) -> cleaned_data_(yyyy)
#
# - Next Step:
#   - Compute self-employment rates
#     - Overall, by industry and groups
#   - Compute supplementary statistics
#     - Labor force participation rate 
#
#----------------------------------------------------------------------------- -
#
# Final Outputs (saved as .rds in data/processed)
# - cleaned_data_(yyyy).rds
#
############################################################################## #

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
source(here('R/KEAPS/module/cleaning/clean_one_year.R'), local = environment())

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
  if(!file.exists(here('data/KEAPS/clean'))) {
    dir.create(here('data/KEAPS/clean'))
    message("[INFO] File directory data/KEAPS/clean/ created.")
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

