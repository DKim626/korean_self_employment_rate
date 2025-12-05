#### ==== 0. Initial set up ====
# Loading necessary libraries
library(dplyr)
library(stringi)
library(purrr)
library(assertthat)

# Loading modules for preprocessing
tryCatch(
  {
    source(here('R/KHTS/module/preprocess/preprocess_functions.R'),
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
  raw_code_dir <- 'R/KHTS/raw'
  raw_data_dir <- 'data/KHTS/raw'
  data_dir <- 'data/KHTS/processed'
  
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
  
  message("[DONE] All KHTS raw data processed and saved to ", data_dir)
  message(sprintf(
    "[INFO] Preprocessing execution time: %02d minutes %02.2f seconds.",
    elapsed_min, elapsed_sec)
  )
  
  # Removes all objects for memory
  rm(list = ls(envir = environment()))
}

