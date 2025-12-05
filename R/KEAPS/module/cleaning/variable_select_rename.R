############################################################################## #
# Variable Selection and Rename
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Selects relevant variables
# - Renames variables Korean -> English
#
#----------------------------------------------------------------------------- -
#
# Usage:
# - Source in 02_data_clean.R
# - Functions:
#   - var_select(data, survey_year):
#     - subset of raw KEAPS tibble with relevant variables only
#   - var_rename(data, survey_year):
#     - same tibble with standardized English column names
#
#----------------------------------------------------------------------------- -
#
# Notes:
# - There are year specific issues:
#   - 1984: survey date variable has different name and format
#   - < 1986: Industry variable exists but not in KSIC format -> removed
#   - >= 2000: Includes previous job's industry -> removed
# - KSIC industry code are included differently
#   - 5th KSIC: 1986 - 1992
#   - 6th/7th KSIC: 1993 - 2000
#   - 8th KSIC: 2000 - 2008
#   - 9th KSIC: 2004 - 2017
#   - 10th, 11th KSIC: 2013 - 2024
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, stringi
#
#----------------------------------------------------------------------------- -

#### var_select ####
#' Selects variables of interest
#'
#' @param data survey data
#' @param survey_year four digit survey year
#'
#' @return tibble() with selected variables only
var_select <- function(data, survey_year) {
  # Vector of regex for KEAPS core variables
  patterns <- c(
    '^조사.*월',           # Survey month
    '조사기준일',          # Exception for 1987 survey
    '성별',                # Sex
    '연령$',               # Age
    '종사.*지위',          # Job Position
    '부업.*여부',          # Secondary Job?
    '경제활동(상태|분류)', # Economic Status
    '산업',                # KSIC code for industry classification
    '승수|가중(치|값)' # Weight
  )
  data <- select(data, matches(paste(patterns, collapse = '|')))

  if(survey_year < 1986){
    # Before 1986, KEAPS did not record KSIC
    # However, they asked about the type of firm they worked in, which is
    # recorded as "산업" or industry even before 1986. However, it is not KSIC.
    data <- select(data, -matches('산업'))
  } else if (survey_year >= 2000) {
    # As of 2000, KEAPS also started asking the industry they have worked prior
    # to the current occupation. Hence, if() is used to exclude these cases.
    data <- select(data, -matches('이전'))
  }
  
  return(data)
}

#### var_rename ####
#' Standardizes column names of data with regular expression
#'
#' yyyymm: year-month survey date (YYYYMM)
#' yymmdd: full survey date (YYMMDD)
#' month: survey month only
#' To account for KSIC data, else if is used
#'
#' @param data survey data
#'
#' @return tibble() with renamed (English)
var_rename <- function(data, survey_year) {
  
  names_col <- colnames(data)
  
  # 1984 has different survey format
  if(survey_year == 1984){
    names_col <- stri_replace_all_regex(names_col,
                                        '.*조사.*(년|연)월.*', 'yymmdd')
  }
  
  # Common rename frame
  names_col <- stri_replace_all_regex(names_col,
                                      '.*조사.*(년|연)월일.*', 'yymmdd')
  names_col <- stri_replace_all_regex(names_col,
                                      '.*조사.*(년|연)월.*', 'yyyymm')
  names_col <- stri_replace_all_regex(names_col, '조사기준일', 'yymmdd')
  names_col <- stri_replace_all_regex(names_col, '.*조사.*월.*', 'month')
  names_col <- stri_replace_all_regex(names_col, '.*성별.*', 'sex')
  names_col <- stri_replace_all_regex(names_col, '.*연령.*', 'age')
  names_col <- stri_replace_all_regex(names_col, '.*종사.*지위.*', 'emp_type')
  names_col <- stri_replace_all_regex(names_col, '.*경제활동.*', 'econ_act')
  names_col <- stri_replace_all_regex(names_col,
                                      '.*(승수|가중(치|값)).*', 'weight')
  
  # KSIC mapping:
  # - 5th KSIC: 1986 - 1992         -> ind_5
  # - 6th/7th KSIC: 1993 - 2000     -> ind_6, ind_7
  # - 8th KSIC: 2000 - 2008         -> ind_8
  # - 9th KSIC: 2004 - 2017         -> ind_9
  # - 10th, 11th KSIC: 2013 - 2024  -> ind_10, ind_11
  if(survey_year >= 1986 & survey_year < 1993){
    names_col <- stri_replace_all_regex(names_col, '.*산업.*', 'ind_5')
  } else if(survey_year < 2000) {
    names_col <- stri_replace_all_regex(names_col, '.*산업.*', 'ind_6')
  } else {
    names_col <- stri_replace_all_regex(names_col, '.*7차산업.*', 'ind_7')
    names_col <- stri_replace_all_regex(names_col, '.*8차산업.*', 'ind_8')
    names_col <- stri_replace_all_regex(names_col, '.*9차산업.*', 'ind_9')
    names_col <- stri_replace_all_regex(names_col, '.*10차산업.*', 'ind_10')
    names_col <- stri_replace_all_regex(names_col, '.*11차산업.*', 'ind_11')
  }
  
  if (survey_year >= 2003) {
    names_col <- stri_replace_all_regex(names_col, '.*부업.*여부.*', 'sec_work')
  }
  
  colnames(data) <- names_col
  
  return(data)
}

