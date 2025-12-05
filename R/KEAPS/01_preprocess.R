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
# - Runs each .R files for raw data provided by KOSIS
# - Save raw data for data cleaning
#
#----------------------------------------------------------------------------- -
#
# Notes:
# - Preprocessing code files provided by KOSIS
#   - External scripts provided by KOSIS (in Korean)
#   - If encoding issue arises when opening code manually,
#   - RStudio -> File -> Reopen with Encoding -> CP949
# - There exist multiple columns with same name
#   - Handled with .name_repair = 'unique' in as_tibble()
# - All processed files saved at 'data/processed'
#
#----------------------------------------------------------------------------- -
#
# Known issues:
# - 1986 recode_factor mismatch:
#   - Original R code used wrong year's coding scheme
#   - Fixed manually in code/raw/fix_1986.R
# - Some years have redundant naming of variables
#   - Handled with as_tibble(..., .name_repair = 'unique')
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, stringi, purrr, assertthat
# - All raw .csv data files must be stored at 'data/raw'
# - All KOSIS .R code files must be stored at 'code/raw'
# - preprocess_functions.R: functions to preprocess raw .csv files
#
#----------------------------------------------------------------------------- -
#
# Output Flow:
# - Raw .csv files -> data_(yyyy).rds
# - Next Step: Data Cleaning (data_clean.R)
#
#----------------------------------------------------------------------------- -
#
# Final Outputs (saved as .rds)
# - data_(yyyy): KEAPS raw data for each year (yyyy)
#
############################################################################## #

#### ==== 0. Initial set up ====
# Loading necessary libraries
library(dplyr)
library(stringi)
library(purrr)
library(assertthat)

# Loading modules for preprocessing
tryCatch(
  {
    source(here('R/KEAPS/module/preprocess/preprocess_functions.R'),
           local = environment())
  }, error = function(e) {
    message("[ERROR] Check file directory of preprocess_functions.R")
  }
)

kosis_env <- new.env(parent = environment())

#### run_preprocess ####
#' Executes preprocess of KEAPS data from 1981 to 2024. Note that this command does not require any input and does not return any output.
#'
run_preprocess <- function() {
  start_time <- proc.time()
  # Directories
  raw_code_dir <- 'R/KEAPS/raw'
  raw_data_dir <- 'data/KEAPS/raw'
  data_dir <- 'data/KEAPS/processed'
  
  if(!file.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
  
  #### ==== 1. Extracts list of .R code file directories ====
  code_list <- extract_code_list(raw_code_dir)
  
  #### ==== 2. KEAPS data processing ====
  # Loops through survey data from 1981 to 2024
  # - Run the codes specified with file_path
  #   - Each KOSIS .R file includes Korean name and coding of variables
  # - Transform data.frame into tibble
  # - Save tibble objects in .rds form
  
  walk2(
    code_list,
    seq_along(code_list),
    \(file_path, list_num) {
      tryCatch({
        # isolated env for each KOSIS run
        kosis_env <- new.env(parent = environment())
        
        # execute raw code
        run_kosis_script(file_path, raw_data_dir, kosis_env)
        
        # extract year
        survey_year <- stri_extract_first_regex(basename(file_path), '[0-9]{4}')
        message("[INFO] Working on survey year: ", survey_year)
        # save as .rds
        save_processed(survey_year, data_dir, kosis_env)
        
        if (list_num %% 5 == 0){
          message("[INFO] Memory cleanup at ", list_num, ' files')
          rm(kosis_env)
          gc()
        }}, error = function(e) {
          message("[ERROR] at ", file_path, " : ", e$message)
        }
      )
    }
  )
  
  elapsed_time <- (proc.time() - start_time)[3]
  elapsed_min <- floor(elapsed_time / 60)
  elapsed_sec <- round(elapsed_time %% 60, 2)
  
  message("[DONE] All KEAPS raw data processed and saved to ", data_dir)
  message(sprintf(
    "[INFO] Preprocessing execution time: %02d minutes %02.2f seconds.",
    elapsed_min, elapsed_sec)
  )
  
  # Removes all objects for memory
  rm(list = ls(envir = environment()))
}

