############################################################################## #
# Calculation of Self-employment, Labor Force Participation, and Unemployment
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Pipeline:
# 1. 01_preprocess.R: Automized KOSIS data processing
#   - Runs each .R files provided by KOSIS
#   - Outputs raw data with variable names & codes
#
# 2. 02_data_cleaning.R: Standardizes variables, merges data
#   - Uses pipeline_clean.R for modularized cleaning
#   - Recodes key variables (econ_act, emp_type, industry code)
#
# 3. 03_rate_calculation.R
#   - Computes monthly self-employment rates (overall, by industry & groups)
#
# 4. 04_data_visualization.R:
#   - Loads yearly trends & generates plots
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Compute monthly & yearly trends of self-employment rate
#   - 3 Types of self-employment definition:
#     1) self_employ_1 (OECD):
#        Family workers + self-employed with/without employees
#     2) self_employ_2 (KOSIS):
#        Self-employed with/without employees
#     3) self_employ_3:
#        Self_employed without employees
#
#   - Provides tax data comparable version (_t suffix):
#     - Excludes family workers from denominator
#
# - Generate RData outputs:
#   - year_all: yearly overall trends
#   - year_ind: yearly industry-wise trends
#   - year_as: yearly trends by age & sex group
#   - year_as_wg: yearly within group trends
#   - year_supp: supplementary (participation & unemployment rates)
#
#----------------------------------------------------------------------------- -
#
# Known issues:
# - 1983 economic activity variable is unreliable:
#   - Labor force participation rate is 1 in most of months
#   - Likely due to misrecorded values in original data
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, stringr, rlang, purrr, stringi, data.table, readr, assertthat
# - Directory: "data/clean/"
#   - Files: cleaned_data_(yyyy).rds
# - scipts:
#   - calc_one_year.R: calculates relevant statistics for each year
#   - rate_calc.R: functions for calculation
#
#----------------------------------------------------------------------------- -
#
# Output Flow:
# - cleaned_data_(yyyy):
#   - month_overall ->    year_overall
#   - month_ind     ->    year_ind
#   - month_as      ->    year_as
#   - month_as_wg   ->    year_as_wg
#   - month_supp    ->    year_supp
#
# - Next Step:
#   - Plot trends (data_visualization.R)
#     - year_overall, year_ind, year_as, year_as_wg -> data visualization
#
# Final Outputs (saved as .rdata)
# - year_overall: year, self_pct_(1/2/3), self_pct_(2/3)_t
# - year_ind: year, ind, code, self_pct_(1/2/3), self_pct_(2/3)_t
# - year_as: year, age_group_2, sex, self_pct_(1/2/3), self_pct_(2/3)_t
# - year_as_wg: same as above but within-group weighted mean
# - year_supp: year, part_rate, unemp_rate, sec_work_rate
#
############################################################################## #


# Load calculation module
# - calc_one_year(): computes self-employment trends for a year
source(file = here('R/KEAPS/module/calc/calc_one_year.R'),
       local = environment())
#### ==== run_rate_calc() ====
#' Computes self_employment trends over a range of years
#' 
#' This function computes self-employment rate of each year.
#' Relevant data will be stored in both monthly and yearly format
#'
#' @param target_age_group name of age group column (e.g. 'age_group_2')
#' @param start_year numeric value of survey year
#' @param end_year same as above
#' 
#' - Provides three types of self-employment rate:
#'  - self_employ_1: family workers, self-employed with/without employees
#'  - self_employ_2: self-employed with/without employees
#'  - self_employ_3: self_employed without employees
#' - "_t" suffix: denotes tax data comparable rates
#'     - excludes family workers from employed population
#' 
#' - month_overall, year_overall: overall trends of self-employment rate
#' 
#' - month_ind, year_ind: industry-wise self-employment rate
#'   - Decomposes self-employment rate by industries
#'   - Sums up to overall rate
#'   
#' - month_as, year_as: self-employment rate by age and sex group
#'   - Decomposes self-employment rate by groups
#'   - Sums up to overall rate
#'   
#' - month_as_wg, year_as_wg: self-employment rate within age and sex group
#'   - Computes self-employment rate within each age and sex group
#'   - Does not sum up to overall rate
run_rate_calc <- function (target_age_group, start_year, end_year) {
  assert_that(
    is.character(target_age_group),
    msg = '[ERROR] target_age_group must be a character string.'
  )
  
  start_time <- proc.time()
  
  #### ==== 1. Computation of month trends ====
  
  safe_calc <- safely(calc_one_year)
  
  results <- map(start_year:end_year,
                 ~ safe_calc(target_age_group, .x)
  )
  
  failed <- map(results, 'error') |> keep(~!is.null(.x))
  
  if (length(failed) > 0) {
    message("[WARNING] The following years failed in calc_one_year:")
    walk(failed, ~ message(" - ", .x$error$message))
  }
  
  results <- map(results, 'result')
  
  # Transform stored results into tibble
  month_overall <- map_dfr(results, 'overall')
  month_ind <- map_dfr(results, 'ind') %>%
    mutate(month = as.numeric(month)) %>%
    arrange(year, month) %>%
    filter(ind != '0')
  month_a <- map_dfr(results, 'a') %>%
    mutate(month = as.numeric(month)) %>%
    arrange(year, month)
  month_as <- map_dfr(results, 'as') %>%
    mutate(month = as.numeric(month)) %>%
    arrange(year, month)
  month_as_wg <- map_dfr(results, 'as_wg') %>%
    mutate(month = as.numeric(month)) %>%
    arrange(year, month)
  month_supp <- map_dfr(results, 'supp') %>%
    mutate(month = as.numeric(month)) %>%
    arrange(year, month)
  
  ##### ==== 2. Saving the results ====
  if(!file.exists(here('data/KEAPS/output'))) {
    dir.create(here('data/KEAPS/output'), recursive = TRUE)
    message('[INFO] Directory created: data/KEAPS/output/')
  }
  file_month <- paste0('data/KEAPS/output/month_trends_', target_age_group, '.rdata')
  if (file.exists(file_month)) {
    message("[INFO] Overwriting existing file: ", file_month)
  }
  save(
    month_overall,
    month_ind,
    month_a,
    month_as,
    month_as_wg,
    month_supp,
    file = file_month
  )
  message("[INFO] Monthly trend data saved to data/KEAPS/output/", file_month)
  #### ==== 3. Computing year trends ====
  ##### ==== 3.1. Overall year trends ====
  year_overall <- year_trends(month_overall)
  
  ##### ==== 3.2. Trends by industry ====
  year_ind <- year_trends(month_ind, c('wave', 'ind', 'ind_kor'))
  
  ##### ==== 3.3. Trends by age ====
  year_a <- year_trends(month_a, c(target_age_group))
  
  ##### ==== 3.4. Trends by age and sex group ====
  year_as <- year_trends(month_as, c(target_age_group, 'sex'))
  
  ##### ==== 3.5. Trends within age and sex group ====
  year_as_wg <- year_trends(month_as_wg, c(target_age_group, 'sex'))
  
  ##### ==== 3.6. Trends of labor participation, unemployment, and secondary work rate ====
  year_supp <- year_supp_trends(month_supp)
  
  ##### ==== 4. Save outputs into .rdata file ====
  file_year <- paste0('data/KEAPS/output/year_trends_', target_age_group, '.rdata')
  if (file.exists(file_year)) {
    message("[INFO] Overwriting existing file: ", file_year)
  }
  save(
    year_overall,
    year_ind,
    year_a,
    year_as,
    year_as_wg,
    year_supp,
    file = file_year
  )
  message("[INFO] Yearly trend data saved to data/KEAPS/output/", file_year)
  
  
  elapsed_time <- (proc.time() - start_time)[3]
  elapsed_min <- floor(elapsed_time / 60)
  elapsed_sec <- round(elapsed_time %% 60, 2)
  
  message(sprintf(
    "[INFO] Calculation execution time: %02d minutes %02.2f seconds.",
    elapsed_min, elapsed_sec))
}

