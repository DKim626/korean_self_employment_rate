############################################################################## #
# KSIC Industry Code Mapping and Recoding
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Purpose of this script:
# - Provide recoding function & mapping tables for KSIC industry codes
# - Used for standardizing industry codes across different years
# - Supports special cases (e.g. 1998 KSIC survey code format)
# - Used as a dependency in data_cleaning.R
#
#----------------------------------------------------------------------------- -
#
# Usage:
# - Source in 02_data_clean.R
# - Functions:
#   + recode_ksic_wave(data, colname, mapping_tbl):
#     * Recodes a single column using a mapping table
#   + apply_ksic_wave(data, colname, survey_year):
#     * Selects appropriate mapping based on year & KSIC wave
# - Data structures:
#   + ksic_mappings (list): Contains mappings for each wave
#
#----------------------------------------------------------------------------- -
#
# Notes:
# - KSIC codes differ by wave (5 - 11th), and also by survey year
# - For example, 1998 KEAPS uses alphabet codes
# - Column names in data are expected as "ind_5", "ind_6", ..., "ind_11" form
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - dplyr, stringr, rlang
#
#----------------------------------------------------------------------------- -

#### recode_ksic_wave ####
#' Recode KSIC column based on a mapping table
#'
#' @param data data containing industry codes
#' @param colname column name (string) to recode (e.g. "ind_5")
#' @param mapping_tbl tibble with 'pattern' and 'eng' columns
#'
#' @return tibble with the specified column recoded
recode_ksic_wave <- function(data, colname, mapping_tbl) {
  for(i in seq_len(nrow(mapping_tbl))) {

    data[[colname]] <- ifelse(
      stri_detect_regex(data[[colname]], mapping_tbl[['pattern']][i]),
      mapping_tbl[['eng']][i],
      data[[colname]]
    )
  }
  
  return(data)
}

#### apply_ksic_recode ####
#' Automatically select KSIC mapping table based on year & wave
#'
#' @param data data containing industry codes
#' @param colname column name (string) to recode (e.g. "ind_5")
#' @param survey_year survey year (numeric)
#'
#' @return tibble with the specified column recoded using appropriate mapping table
apply_ksic_recode <- function(data, colname, survey_year) {
  
  ksic_mappings <- create_ksic_map()
  
  wave <- stri_extract_first_regex(colname, '\\d+')
  key <- if(survey_year == 1998) {
    paste0('wave', wave, '_1998')
  } else {
    paste0('wave', wave)
  }
  mapping_tbl <- ksic_mappings[[key]]
  
  recoded_ksic <- recode_ksic_wave(data, colname, mapping_tbl)
  
  if(!any(data[[colname]] %in% ksic_mappings[[key]][['eng']])) {
    message(
      sprintf(
        "[WARNING] There exist values outside of code at wave %s of year %d",
        wave, survey_year)
      )
  }
  
  return(recoded_ksic)
}

#### create_ksic_map ####
#' Create KSIC mappings for translating Korean labels into English labels
#'
#' @return List of KSIC mappings from 5th to 11th wave
create_ksic_map <- function () {
  # 9th KSIC
  ksic_mapping_wave9 <- tribble(
    ~pattern,       ~eng,
    '^기타|Z',        'Others',
    '국제|U',         'International/Foreign',
    '자가소비|T',     'Household Consumption',
    '개인서비스|S',   'Organization, Repairment, and Private Service',
    '예술|R',         'Arts, Sports, and Leisure',
    '사회복지|Q',     'Health/Social Work',
    '교육서비스|P',   'Education',
    '사회보장|O',     'Public Administration, Defence, and Social Security',
    '사업|N',         'Business Infrastructure Management and Support Service',
    '전문|M',         'Professional, Science, Technology Service',
    '부동산|L',       'Real Estate',
    '금융|K',         'Finance/Insurance',
    '통신|J',         'Press, Video, Communication, and Informaton',
    '숙박|I',         'Hotel/Restaurant',
    '운수|H',         'Transportation',
    '소매|G',         'Wholesale/Retail',
    '건설업|F',       'Construction',
    '하수|E',         'Sewer/Disposal/Recycle/Environment',
    '전기|D',         'Electricity, Gas, and Water',
    '제조업|C',       'Manufacturing',
    '광업|B',         'Mining',
    '농업|A',         'Farming/Forestry/Fishery'
  )
  
  # 10th/11th KSIC
  # Used in KEAPS surveys 2013 - 2024
  ksic_mapping_wave10 <- tribble(
    ~pattern,        ~eng,
    '^기타|Z',        'Others',
    '국제|U',         'International/Foreign',
    '자가소비|T',     'Household Consumption',
    '개인서비스|S',   'Organization, Repairment, and Private Service',
    '예술|R',         'Arts, Sports, and Leisure',
    '사회복지|Q',     'Health/Social Work',
    '교육서비스|P',   'Education',
    '사회보장|O',     'Public Administration, Defence, and Social Security',
    '사업|N',         'Business Infrastructure Management/Support/Rent Service',
    '전문|M',         'Professional, Science, Technology Service',
    '부동산|L',       'Real Estate',
    '금융|K',         'Finance/Insurance',
    '통신|J',         'Informaton Communication',
    '숙박|I',         'Hotel/Restaurant',
    '운수|H',         'Transportation/Storage',
    '소매|G',         'Wholesale/Retail',
    '건설업|F',       'Construction',
    '하수|E',         'Water/Sewer/Disposal/Recycle',
    '전기|D',         'Electricity/Gas/Steam/Air Condition',
    '제조업|C',       'Manufacturing',
    '광업|B',         'Mining',
    '농업|A',         'Farming/Forestry/Fishery'
  )
  
  # Combine all wave mappings into a single list
  ksic_mappings <- list(
    'wave9' = ksic_mapping_wave9,
    'wave10' = ksic_mapping_wave10
  )
  
  return (ksic_mappings)
}