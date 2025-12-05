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
    '.*분기.*|조사연월',            # Quarter of Survey
    '가구.*번호',          # Household ID
    '성별',                # Sex
    '연령$',               # Age
    '관계',                # Relationship with head
    '종사.*지위',          # Job Position
    '취업',                # Employed?
    '산업',                # KSIC code for industry classification
    '^소득$',              # Income
    '사업',                # Business income
    '근로',                # Labor income
    '가구주소득',          # Labor income for head
    '배우자소득',          # Labor income for spouse
    '기타가구원소득',      # Labor income for other household member
    '승수|가중(치|값)'     # Weight
  )
  data <- select(data, matches(paste(patterns, collapse = '|')))

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
  
  # Common rename frame
  names_col <- stri_replace_all_regex(names_col, '.*분기.*', 'quarter')
  if (survey_year >= 2020) {
    names_col <- stri_replace_all_regex(names_col, '.*조사연월.*', 'quarter')
  }
  
  names_col <- stri_replace_all_regex(names_col, '가구.*번호', 'hhid')
  names_col <- stri_replace_all_regex(names_col, '성별', 'sex')
  names_col <- stri_replace_all_regex(names_col, '연령', 'age')
  names_col <- stri_replace_all_regex(names_col, '관계', 'rel')
  names_col <- stri_replace_all_regex(names_col, '종사.*지위', 'emp_type')
  names_col <- stri_replace_all_regex(names_col, '_취업', '_employed')
  names_col <- stri_replace_all_regex(names_col,
                                      '.*(승수|가중(치|값)).*', 'weight')
  
  names_col <- stri_replace_all_regex(names_col, '.*가구주.*사업소득', 'income_business_rp')
  names_col <- stri_replace_all_regex(names_col, '.*배우자.*사업소득', 'income_business_sp')
  names_col <- stri_replace_all_regex(names_col, '.*기타.*사업소득', 'income_business_o')
  names_col <- stri_replace_all_regex(names_col, '.*가구주소득', 'income_labor_rp')
  names_col <- stri_replace_all_regex(names_col, '.*배우자소득', 'income_labor_sp')
  names_col <- stri_replace_all_regex(names_col, '.*기타가구원소득', 'income_labor_o')
  names_col <- stri_replace_all_regex(names_col, '.*사업소득$', 'income_business')
  names_col <- stri_replace_all_regex(names_col, '.*근로소득$', 'income_labor')  
  names_col <- stri_replace_all_regex(names_col, '^소득$', 'income')
  
  # KSIC mapping:
  # - 5th KSIC: 1986 - 1992         -> ind_5
  # - 6th/7th KSIC: 1993 - 2000     -> ind_6, ind_7
  # - 8th KSIC: 2000 - 2008         -> ind_8
  # - 9th KSIC: 2004 - 2017         -> ind_9
  # - 10th, 11th KSIC: 2013 - 2024  -> ind_10, ind_11
  if(survey_year <= 2017){
    names_col <- stri_replace_all_regex(names_col, '산업.*', 'ind_9')
  } else if(survey_year > 2017 & survey_year <= 2019) {
    names_col <- stri_replace_all_regex(names_col, '산업.*', 'ind_10')
  } else {
    names_col <- stri_replace_all_regex(names_col, '10차산업.*', 'ind_10')
  }
  
  colnames(data) <- names_col
  
  return(data)
}

