#### extract_code_list ####
#' Extract list of KOSIS .R code files directory
#' - Ignores KOSIS 1986 .R file
#'
#' @param raw_code_dir KOSIS .R file directory (code/raw/)
#'
#' @return list of .R raw code directory
extract_code_list <- function(raw_code_dir) {
  
  assert_that(is.character(raw_code_dir))
  
  code_list <- list.files(path = raw_code_dir, full.names = TRUE)
  assert_that(length(code_list) > 0,
              msg = paste0('[ERROR] No file was detected in ', raw_code_dir))
  
  return(code_list)
}

#### run_kosis_script ####
#' Run KOSIS code in new environment:
#' - Hooks utils package functions:
#'   - install.packages(), library(): ignored
#'   - read.table(): file path overwritten
#' - Read file
#'
#' @param file KOSIS .R file
#' @param raw_data_dir raw data file directory (data/raw/)
#' @param new_env new environment (kosis_env)
#'
#' @return New environment with raw data file
run_kosis_script <- function(file, raw_data_dir, new_env) {
  
  new_env$install.packages <- function(...){
    message("[INFO] install.packages() in .R is is ignored")
  }
  
  new_env$library <- function(...){
    message("[INFO] library() in .R is is ignored")
  }
  
  new_env$read.table <- function(f, ...) {
    fixed <- file.path(raw_data_dir, basename(f))
    
    if (!file.exists(fixed)) stop("[ERROR] File not found: ", fixed)
    
    message(
      "[INFO] File path in KOSIS code changed automatically to ", fixed
      )
    
    utils::read.table(fixed, ...)
  }
  
  source(file, local = new_env, encoding = 'CP949')
  
  return(new_env)
}

#### save_processed ####
#' Saves processed KOSIS data into .rdata file
#'
#' @param survey_year four digit year of survey
#' @param data_dir data file directory for .rdata (data/)
#' @param new_env new environment with relevant data
save_processed <- function(survey_year, data_dir, new_env) {
  
  data_name <- paste0('data_', survey_year)
  
  if (!"mdis" %in% ls(new_env)) {
    stop("[ERROR] 'mdis' not found in environment")
  }
  
  raw_data <- get('mdis', envir = new_env)
  
  if (!is.data.frame(raw_data)) {
    stop("[ERROR] 'mdis' is not a data.frame")
  }
  
  data_tibble <- as_tibble(raw_data, .name_repair = 'unique')
  
  file_path <- file.path(data_dir, paste0('data_', survey_year, '.rds'))
  
  saveRDS(data_tibble, file = file_path)
  
  message("[INFO] Saved ", survey_year, " file at ", file_path)
}
