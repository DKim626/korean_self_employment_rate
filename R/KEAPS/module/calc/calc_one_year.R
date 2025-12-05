############################################################################## #
# Calculate relevant rates for each year
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Source in 03_rate_calculation.R
# - Calculate monthly self-employment rates and supplementary statistics
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
# - Directory:
#   - "data/clean/"
#     - cleaned_data_(yyyy).rds
#   - "R/module/calc"
#     - rate_calc.R
#
#----------------------------------------------------------------------------- -
# Load source
# - regular_rate()
# - decomp_rate()
# - calc_supp_rate()
source(here('R/KEAPS/module/calc/rate_calc.R'), local = environment())

#### calc_one_year ####
#' Compute self-employment rate by each year
#' 
#' This function computes self-employment rate, as well as labor force
#' participation and unemployment rate for supplementary materials.
#' 
#' @param target_age_group character string (e.g. "age_group_2")
#' @param year survey year
#'
#' @return list of trends
calc_one_year <- function(target_age_group, year) {
  assert_that(
    is.character(target_age_group),
    msg = '[ERROR] target_age_group must be a character string.'
  )
  age_group_sym <- sym(target_age_group)
  
  survey_year <- year
  
  message("[INFO] Calculating survey year ", survey_year)
  
  #### ==== 1. Load cleaned data for the year ====
  file_path <- paste0('data/KEAPS/clean/cleaned_data_', survey_year, '.rds')
  if(!file.exists('data/KEAPS/clean')) {
    stop('[ERROR] File directory data/KEAPS/clean/ does not exist')
  } else if(!file.exists(file_path)) {
    stop('[ERROR] Cleaned data set does not exist for year ', survey_year)
  }
  data <- readRDS(file = file_path)
  
  #### ==== 2. Compute labor participation rate and unemployment rate ====
  # This content is supplementary information
  data_supp <- calc_supp_rate(data, survey_year)
  
  #### ==== 3. Filter to labor force only ====
  data <- data %>%
    filter(
      econ_act %in% c(
        'On temporary leave', 'Worked occasionally', 'Worked mainly',
        'Employed'
      )
    )
  
  #### ==== 4. Compute overall self-employment trend ====
  data_overall <- regular_rate(data, group_vars = NULL, survey_year)
  
  #### ==== 5. Compute industry-wise trend (if applicable) ====
  # KSIC code was recorded only after 1985, handled with if() else
  data_ind_list <- list()
  
  if (survey_year >= 1986) {
    
    ind_ver_list <- grep('^ind_(\\d){1,2}$', colnames(data), value = TRUE)
    
    ind_ver_kor <- grep('^ind_(\\d){1,2}_kor$', colnames(data), value = TRUE)
    key_kor <- data[c(ind_ver_list, ind_ver_kor)]
    
    for(v in ind_ver_list){
      if(v %in% colnames(data)){
        
        key_kor_v <- data[c(v, paste0(v, '_kor'))] %>%
                            group_by(!!sym(v)) %>%
                            slice_head(n = 1) %>%
                              ungroup()
        colnames(key_kor_v) <- stri_replace_all_regex(colnames(key_kor_v),
                                                     '(\\d){1,2}_', '')
                          
        data_ind_list[[length(data_ind_list) + 1]] <- decomp_rate(
          data = data, group_vars = v, survey_year = survey_year
        ) %>%
          rename(ind = all_of(v)) %>%
          mutate(wave = as.integer(stri_extract_first_regex(v, '\\d+'))) %>%
          left_join(key_kor_v, by = c('ind' = v))
      }
    }
  }
  
  data_ind <- if(length(data_ind_list) != 0) bind_rows(data_ind_list) else NULL
  
  rm(data_ind_list)
  
  #### ==== 6. Compute group-wise trend ====
  data_a <- decomp_rate(
    data = data,
    group_vars = c(target_age_group),
    survey_year = survey_year
  )
  
  data_as <- decomp_rate(
    data = data,
    group_vars = c(target_age_group, 'sex'),
    survey_year = survey_year
  )
  
  #### ==== 7. Compute within-group trend ====
  data_as_wg <- regular_rate(
    data = data,
    group_vars = c(target_age_group, 'sex'),
    survey_year = survey_year
  )
  
  #### ==== 8. Return result as a named list ====
  result <- list(
    year = survey_year,
    overall = data_overall,
    ind = if(!is.null(data_ind) && nrow(data_ind) > 0) data_ind else NULL,
    a = data_a,
    as = data_as,
    as_wg = data_as_wg,
    supp = data_supp
  )
  
  return(result)
  
}
