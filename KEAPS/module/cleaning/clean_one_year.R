############################################################################## #
# Process one year of KEAPS data
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Provide user defined functions for data cleaning
# - Used as a pipeline in data_cleaning.R
#
#----------------------------------------------------------------------------- -
#
# Usage:
# - Source in 02_data_clean.R
# - Functions:
#   - process_one_year(year, target_age_group)
#     - This function integrates functions from other modules
#     - Performs data cleaning on one year of data
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, rlang
# - variable_select_rename.R: selects and renames variables
# - variable_recode.R: recodes variables
# - ksic_mapping.R: mapping for KSIC industry codes
# - rate_calc.R: computes self-employment, unemployment, participation rate
#
#----------------------------------------------------------------------------- -
# Loads module for variable selection and rename
source(file = here('R/KEAPS/module/cleaning/variable_select_rename.R'),
       local = environment())
# Loads industry key mapping and function
source(file = here('R/KEAPS/module/cleaning/variable_recode.R'),
       local = environment())
# Loads industry key mapping and function
source(file = here('R/KEAPS/module/cleaning/ksic_mapping.R'),
       local = environment())

#### Pipeline for data cleaning ====
# 1) Loads raw data in .rds format
# 2) Selects and renames variables
# 3) Recodes variables
# 4) Constructs self-employment rate
# 5) Computes trends:
#   - Overall trends          -> overall
#   - Industry-wise trends    -> ind
#   - Age & sex group trends  -> as
#   - Within group trends    -> as_wg
#   - Supplementary           -> supp
#### clean_one_year ####
#' Cleans KEAPS survey data of given year.
#' @param year Numeric value of survey year within the range between 1981 and 2024.
clean_one_year <- function(year) {
  on.exit(gc())
  ##### ==== 1. Load .rds for each year ====
  assert_that(is.numeric(year), length(year) == 1)
  assert_that(year >= 1981, year <= 2024,
              msg = '[ERROR] year is out of bound (1981 - 2024)')
  assert_that(
    file.exists('data/KEAPS/processed'),
    msg = "[ERROR] File directory data/processed/ does not exist."
  )
  
  survey_year <- year
  
  rds_path <- sprintf('data/KEAPS/processed/data_%s.rds', survey_year)
  assert_that(file.exists(rds_path),
              msg = sprintf(
                '[ERROR] Processed data for year %d does not exist.',
                survey_year)
  )
  data <- readRDS(rds_path)
  
  message('[INFO] Cleaning survey data of ', survey_year)
  
  #### ==== 2. Select and rename variables ====
  data <- var_select(data, survey_year)
  data <- var_rename(data, survey_year)
  
  #### ==== 3. Recode variables ====
  # Add month variable
  data <- extract_month(data)
  
  # Unique case: In year 1984, \xb is added pasted in front of weight variable
  if(survey_year == 1984){
    data$weight <- stri_replace_all_regex(data$weight, "\xb7", '7')
  }
  
  data <- data %>%
    # As structure of some variables are factor, transform to character for
    # further cleaning process.
    mutate(across(c(sex, age, emp_type, econ_act), as.character))
  data$sex <- recode_sex(data$sex)
  data$age <- parse_number(data$age)
  data$emp_type <- recode_emp_type(data$emp_type)
  data$econ_act <- recode_econ_act(data$econ_act)
  if (year >= 2003) {
    data$sec_work <- recode_sec_work(data$sec_work)
  }
  data$weight <- as.numeric(data$weight)
  if (any(is.na(data$weight))) {
    message("[WARNING] Missing values found in weight after as.numeric()")
  }
  
  data <- data %>%
    bind_cols(construct_age_group(data$age))
  
  if (survey_year >= 1986) {
    
    ind_cols <- grep('^ind', colnames(data), value = TRUE)
    
    for (col in ind_cols) {
      data[[paste0(col, '_kor')]] <- data[[col]]
    }
    
    safe_apply <- safely(apply_ksic_recode)
    
    data <- reduce(ind_cols,
                   .init = data,
                   .f = \(df, col){
                     df[[col]] <- as.character(df[[col]])
                     df[[col]] <- stri_replace_all_fixed(df[[col]], " ", "")
                     result <- safe_apply(df, col, survey_year)
                     
                     if(!is.null(result$error)) {
                       message(
                         sprintf(
                           "[ERROR] KSIC recode failed for %s (year %d): %s",
                           col, survey_year, result$error$message
                         )
                       )
                     }
                     
                     return(result$result)
                   }
    )
  }
  
  #### ==== 4. Constructing self-employment variable ====
  data <- data %>%
    bind_cols(construct_self_emp(data$emp_type))
  
  #### ==== 5. Save cleaned .rds file ====
  file_path <- here(paste0('data/KEAPS/clean/cleaned_data_', survey_year, '.rds'))
  
  saveRDS(data, file = file_path)
  
  message("[INFO] Cleaned data of year ", year, " saved at ", file_path)
  
}

