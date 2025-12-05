############################################################################## #
# Self-employment, Unemployment, Labor Participation Rate Calculation 
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Provide user defined functions for data cleaning
# - Used as a dependency in data_cleaning.R
#
#----------------------------------------------------------------------------- -
#
# Usage:
# - Source in 03_rate_calculation.R
# - Functions:
#   - regular_rate(data, group_vars, survey_year):
#     - Computes regular or within-group rates
#   - decomp_rate(data, group_vars, survey_year):
#     - Decomposes overall rate with specified groups
#       (e.g. KSIC code, age & sex groups)
#   - calc_supp_rate(data, survey_year):
#     - Computes supplementary information
#     - part_rate: labor participation rate
#     - unemp_rate: unemployment rate
#     - sec_work_rate: secondary work rate
#
#----------------------------------------------------------------------------- -
#
# Notes:
# - group_vars must be character vector
# - Requires columns:
#   month, self_emp_(1/2/3), self_emp_(2/3)_t, emp_type, weight 
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, rlang
#
#----------------------------------------------------------------------------- -

#### regular_rate ####
#' Compute regular or within-group rate
#'
#' This function calculates ordinary rates.
#'
#' @param data data with month, self_emp_1/2/3, self_emp_2/3_t, emp_type, weight
#' @param group_vars variable names to group by
#' @param survey_year survey year of KEAPS
#'
#' @return tibble with monthly self-employment rate
regular_rate <- function(data, group_vars, survey_year){
  
  group_syms <- syms(group_vars)
  
  data_a <- data %>%
    group_by(month, !!!group_syms) %>%
    summarize(
      self_pct_1 = weighted.mean(self_emp_1, weight),
      self_pct_2 = weighted.mean(self_emp_2, weight),
      self_pct_3 = weighted.mean(self_emp_3, weight),
      .groups = 'drop'
    )
  
  data_b <- data %>%
    filter(emp_type != 'Family worker') %>%
    group_by(month, !!!group_syms) %>%
    summarize(
      self_pct_2_t = weighted.mean(self_emp_2, weight),
      self_pct_3_t = weighted.mean(self_emp_3, weight),
      .groups = 'drop'
    )
  
  join_keys <- c('month', group_vars)
  result <- left_join(data_a, data_b, by = join_keys) %>%
    mutate(year = survey_year) %>%
    select(year, everything())
  
  return(result)
}

#### decomp_rate ####
#' Compute decomposed rate
#'
#' This function decomposes the rates, which sum up to what can be achieved with
#' regular_rate()
#'
#' @param data tibble (month, self_emp_1/2/3, self_emp_2/3_t, emp_type, weight)
#' @param group_vars variable names to group by
#' @param survey survey year of KEAPS
#'
#' @return tibble with monthly self-employment rate, decomposed by groups
decomp_rate <- function(data, group_vars, survey_year){
  
  group_syms <- syms(group_vars)
  
  # Usual self-employment version
  data_a <- data %>%
    group_by(month, !!!group_syms) %>%
    summarize(
      self_pct_1 = sum(self_emp_1 * weight),
      self_pct_2 = sum(self_emp_2 * weight),
      self_pct_3 = sum(self_emp_3 * weight),
      tot = sum(weight),
      .groups = 'drop'
    ) %>%
    group_by(month) %>%
    mutate(
      tot = sum(tot),
      self_pct_1 = self_pct_1 / tot,
      self_pct_2 = self_pct_2 / tot,
      self_pct_3 = self_pct_3 / tot
    ) %>%
    ungroup() %>%
    select(-tot)
  
  # Tax data comparable version
  data_b <- data %>%
    filter(emp_type != 'Family worker') %>%
    group_by(month, !!!group_syms) %>%
    summarize(
      self_pct_2_t = sum(self_emp_2 * weight),
      self_pct_3_t = sum(self_emp_3 * weight),
      tot = sum(weight),
      .groups = 'drop_last'
    ) %>%
    group_by(month) %>%
    mutate(
      tot = sum(tot),
      self_pct_2_t = self_pct_2_t / tot,
      self_pct_3_t = self_pct_3_t / tot
    ) %>%
    ungroup() %>%
    select(-tot)
  
  join_keys <- c('month', group_vars)
  result <- left_join(data_a, data_b, by = join_keys) %>%
    mutate(year = survey_year)
  
  result <- result %>%
    select(year, month, everything())
  return(result)
}

#### calc_supp_rate ####
#' Constructs supplementary data set
#'
#' This function computes two supplementary statistics:
#' - Labor participation rate
#' - Unemployment rate
#'
#' @param data tibble with relevant variables
#' @param survey_year year of survey
#'
#' @return tibble with labor participation rate and unemployment rate
#' - part_rate: labor participation rate
#' - unemp_rate: unemployment rate
calc_supp_rate <- function(data, survey_year) {
  
  if (survey_year < 2003) {
    base_data <- data %>%
      select(month, econ_act, age, weight) %>%
      mutate(
        participation = ifelse(econ_act == 'Economically inactive', 0, 1),
        employed = ifelse(
          econ_act %in% c(
            'Employed', 'Worked mainly', 'Worked occasionally',
            'On temporary leave'), 1, 0
        )
      )
    
  } else if (survey_year >= 2003) {
    base_data <- data %>%
      select(month, econ_act, sec_work, age, emp_type, weight) %>%
      mutate(
        participation = ifelse(econ_act == 'Economically inactive', 0, 1),
        employed = ifelse(
          econ_act %in% c(
            'Employed', 'Worked mainly', 'Worked occasionally',
            'On temporary leave'), 1, 0
        ),
        sec_work = fcase(
          sec_work == '0', NA_real_,
          sec_work == 'Y', 1,
          sec_work == 'N', 0
        )
      )
    
  }
  
  data_part <- base_data %>%
    filter(age >= 15 & age < 65) %>%
    group_by(month) %>%
    summarize(
      part_rate = weighted.mean(participation, weight),
      .groups = 'drop'
    )
  
  data_unemp <- base_data %>%
    filter(participation == 1) %>%
    mutate(unemp = 1 - employed) %>%
    group_by(month) %>%
    summarize(
      unemp_rate = weighted.mean(unemp, weight),
      .groups = 'drop'
    )
  
  data_supp <- left_join(data_part, data_unemp, by = 'month')
  
  if (survey_year >= 2003) {
    
    data_sec_work <- base_data %>%
      filter(participation == 1) %>%
      group_by(month) %>%
      summarize(
        sec_work_rate = weighted.mean(sec_work, weight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    data_sec_work_t <- base_data %>%
      filter(employed == 1 & emp_type != 'Family worker') %>%
      group_by(month) %>%
      summarize(
        sec_work_rate_t = weighted.mean(sec_work, weight, na.rm = TRUE),
        .groups = 'drop'
      )
    
    data_sec_work <- left_join(data_sec_work, data_sec_work_t, by = 'month')
    
    data_supp <- left_join(data_supp, data_sec_work, by = 'month')
    
  }
  
  data_supp <- data_supp %>%
    mutate(year = survey_year) %>%
    select(year, month, part_rate, unemp_rate,
           any_of(c('sec_work_rate', 'sec_work_rate_t')))
  
  return(data_supp)
}

#### year_trends ####
#' Summarize monthly trends into year trends
#' 
#' This function simply summarizes monthly trends into year trends
#' 
#' @param data tibble with monthly trends
#' @param group_vars set of variables to group by
#' 
#' @return tibble with relevant year trends
year_trends <- function(data, group_vars = NULL) {
  
  group_syms <- syms(group_vars)
  
  data <- data %>%
    select(-month)
  
  if(is.null(group_vars)) {
    result <- data %>%
      group_by(year) %>%
      summarize(
        across(contains('self'), \(x) mean(x, na.rm = TRUE)),
        .groups = 'drop')
  } else {
    result <- data %>%
      group_by(year, !!!group_syms) %>%
      summarize(
        across(contains('self'), \(x) mean(x, na.rm = TRUE)),
        .groups = 'drop')
  }
  
  return(result)
}

#### year_supp_trends ####
#' Summarize monthly trends of supplemental data into year trends
#' 
#' This function simply summarizes monthly trends into year trends
#' 
#' @param data tibble with monthly trends
#' 
#' @return tibble with relevant year trends
year_supp_trends <- function(data) {
  
  result <- data %>%
    group_by(year) %>%
    summarize(across(c(part_rate, unemp_rate, sec_work_rate, sec_work_rate_t),
                     \(x) mean(x, na.rm = TRUE)),
              .groups = 'drop')
  
  return (result)
}
