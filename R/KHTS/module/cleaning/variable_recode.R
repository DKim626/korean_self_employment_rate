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
# - Source in 02_data_clean.R
# - Functions:
#   - extract_month(data):
#     - Computes regular or within-group rates
#   - recode_sex(sex), recode_emp_type(emp_type), recode_econ_act(econ_act):
#     - Recodes Korean -> English
#   - construct_age_group(age), collapse_age(group_3):
#     - Codes age groups with 5 year interval
#     - age_group_1: 15-19, 20-24, ..., 55-59, Above 60
#     - age_group_2: 15-19, 20-24, ..., 65-59, Above 70
#     - age_group_3: 15-19, 20-24, ..., 75-59, Above 80
#   - construct_self_emp(emp_type):
#     - Codes self-employment by definitions
#     - self_emp_1 (OECD): Family workers, employers with/without employees
#     - self_emp_2 (KOSIS): Employers with/without employees
#     - self_emp_3: Employers without employees
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

#### extract_month ####
#' Extract month column from yyyymm or yymmdd column
#'
#' @param data tibble with survey date columns
#'
#' @return tibble with survey month (numeric)
extract_month <- function(data) {
  if('yyyymm' %in% colnames(data)){
    data$month <- ifelse(
      str_length(data$yyyymm) == 4,
      as.numeric(str_sub(data$yyyymm, 3, -1)), # YYMM format
      as.numeric(str_sub(data$yyyymm, 5, -1))  # YYYYMM format
    )
  } else if('yymmdd' %in% colnames(data)){
    data$month <- as.numeric(str_sub(data$yymmdd, 3, 4))
  }
  
  return(data)
}


#### recode_sex ####
#' Recodes sex in Korean to English
#'
#' @param column vector of sex
#'
#' @return vector with English code (character)
recode_sex <- function(column) {
  column <- fcase(
    stri_detect_fixed(column, '남') | column == "1", "Male",
    stri_detect_fixed(column, '여') | column == "2", "Female",
    default = NA_character_
  )
  if(any(is.na(column))) message("NA in sex variable")
  return(column)
}

#### recode_rel ####
#' Recodes sex in Korean to English
#'
#' @param column vector of sex
#'
#' @return vector with English code (character)
recode_rel <- function(column) {
  column <- fcase(
    column == "1" | column == 1, "Head",
    column == "2" | column == 2, "Spouse",
    column %in% as.character(3:9) | column %in% 3:9, "Other",
    default = NA_character_
  )
  if(any(is.na(column))) message("NA in rel variable")
  return(column)
}

#### recode_emp_type ####
#' Recodes type of employment into English
#'
#' @param column vector of type of employment
#'
#' @return vector with English code (character)
recode_emp_type <- function(column) {
  
  column <- stri_replace_all_fixed(column, ' ', '')
  
  column <- fcase(
    stri_detect_fixed(column, "일용") | column == "3", "Day worker",
    stri_detect_regex(column, "상용.*임시") | column == "1", "Full-time/Temporary",
    stri_detect_regex(column, "상(시|용)"), "Full-time",
    stri_detect_fixed(column, "임시") | column == "2", "Temporary",
    stri_detect_regex(column, "(가족)|(무급)") | column == "6", "Family worker",
    column == "고용원이있는자영업자" | column == "4", "Self-employed with employee",
    column == "고용원이없는자영업자" | column == "5", "Self-employed without employee",
    stri_detect_regex(column, "(단독)?.*자영자"),
    "Self-employed without employee",
    stri_detect_fixed(column, "고용") | column == "4", "Self-employed with employee",
    stri_detect_fixed(column, "없음"), "None",
    column %in% c("", "00"), NA_character_,
    stri_detect_fixed(column, "아님"), NA_character_,
    column %in% c("7", '기타'), 'Other Work',
    default = column
  )
  
  return(column)
}

#### recode_employed ####
#' Recodes sex in Korean to English
#'
#' @param column vector of sex
#'
#' @return vector with English code (character)
recode_employed <- function(column) {
  column <- fcase(
    column == "1" | stri_detect_regex(column, "^취업$"), "Employed",
    column == "2" | stri_detect_regex(column, "^비취업$"), "Unemployed",
    default = NA_character_
  )
  if(any(is.na(column))) message("NA in employed variable")
  return(column)
}

#### construct_age_group ####
#' Codes five-year age group into three types
#' - age_group_1: 15-19, 20-24, ..., 55-59, Above 60
#' - age_group_2: 15-19, 20-24, ..., 65-59, Above 70
#' - age_group_3: 15-19, 20-24, ..., 75-59, Above 80
#'
#' @param column vector with age information
#' @param type: type of age group
#'
#' @return tibble with age group
construct_age_group <- function(column) {
  collapse_age <- function(group_3) {
    
    group_2 <- ifelse(
      group_3 %in% c('70-74', '75-79', 'Above 80'),
      'Above 70', as.character(group_3)
    )
    group_2 <- factor(
      group_2,
      levels = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49',
                 '50-54','55-59', '60-64', '65-69', 'Above 70')
    )
    
    group_1 <- ifelse(
      group_2 %in% c('60-64', '65-69', 'Above 70'),
      'Above 60', as.character(group_2)
    )
    group_1 <- factor(
      group_1,
      levels = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49',
                 '50-54', '55-59', 'Above 60')
    )
    
    return(list(group_2 = group_2, group_1 = group_1))
  }
  
  group_3 = cut(
    column,
    breaks = c(seq(15, 80, by = 5), Inf),
    labels = c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74',
               '75-79', 'Above 80'),
    right = FALSE
  )
  
  collapsed <- collapse_age(group_3)
  
  return(
    tibble(
      age_group_1 = collapsed$group_1,
      age_group_2 = collapsed$group_2,
      age_group_3 = group_3,
      )
  )
}

#### construct_self_emp ####
#' Constructs self-employment codes for three types of definition
#' - self_emp_1 (OECD): Family workers, employers with/without employees
#' - self_emp_2 (KOSIS): Employers with/without employees
#' - self_emp_3: Employers without employees
#'
#' @param column a vector with employment type
#'
#' @return tibble with self-employment codes
construct_self_emp <- function(column) {
  
  self_emp_1 = ifelse(
    column %in% c(
      'Family worker',
      'Self-employed without employee',
      'Self-employed with employee'
    ), 1, 0
  )
  
  self_emp_2 = ifelse(
    column %in% c(
      'Self-employed without employee',
      'Self-employed with employee'
    ), 1, 0
  )
  
  self_emp_3 = ifelse(
    column %in% c('Self-employed without employee'), 1, 0
  )
  
  return(tibble(self_emp_1, self_emp_2, self_emp_3))
}
