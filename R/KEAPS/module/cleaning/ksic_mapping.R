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
  # 5th KSIC
  # Used in KEAPS surveys 1986 - 1992
  ksic_mapping_wave5 <- tribble(
    ~pattern,        ~eng,
    '개인서비스업', 'Social and Private Service',
    '사업서비스업', 'Finance, Insurance, Real Estate, and Business Service',
    '운수,창고',    'Transportation, Storage, and Telecommunication',
    '도소매',       'Wholesale/Retail, Restaurant, and Hotel Service',
    '건설업',       'Construction',
    '전기,가스',    'Electricity, Gas, and Water',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '농업',         'Farming, Hunting, Forestry, and Fishery'
  )
  
  # 6th/7th KSIC
  # Used in KEAPS surveys 1993 - 2000
  # 1998 survey uses single letter codes (A~Q)
  # - handled with ksic_mapping_wave6_1998
  ksic_mapping_wave6 <- tribble(
    ~pattern,        ~eng,
    '국제',         'International/Foreign',
    '가사서비스',   'House Chore Service',
    '개인서비스업', 'Other Public, Repairment, and Private Service',
    '사회복지사업', 'Health/Social Work',
    '교육서비스',   'Education',
    '사회보장행정', 'Public Administration, Defence, and Social Security',
    '사업서비스업', 'Real Estate/Business Service',
    '금융',         'Finance/Insurance',
    '운수',         'Transportation/Telecommunication',
    '숙박',         'Hotel/Restaurant',
    '소매',         'Wholesale/Retail and Consumer Good Repairment',
    '건설업',       'Construction',
    '전기,가스',    'Electricity, Gas, and Water',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '어업',         'Fishery',
    '농업',         'Farming/Forestry'
  )
  
  ksic_mapping_wave6_1998 <- tribble(
    ~pattern,        ~eng,
    '^Q$',          'International/Foreign',
    '^P$',          'House Chore Service',
    '^O$',          'Other Public, Repairment, and Private Service',
    '^N$',          'Health/Social Work',
    '^M$',          'Education',
    '^L$',          'Public Administration, Defence, and Social Security',
    '^K$',          'Real Estate/Business Service',
    '^J$',          'Finance/Insurance',
    '^I$',          'Transportation/Telecommunication',
    '^H$',          'Hotel/Restaurant',
    '^G$',          'Wholesale/Retail and Consumer Good Repairment',
    '^F$',          'Construction',
    '^E$',          'Electricity, Gas, and Water',
    '^D$',          'Manufacturing',
    '^C$',          'Mining',
    '^B$',          'Fishery',
    '^A$',          'Farming/Forestry'
  )
  
  ksic_mapping_wave7 <- tribble(
    ~pattern,        ~eng,
    '국제',         'International/Foreign',
    '가사서비스',   'House Chore Service',
    '개인서비스업', 'Other Public, Repairment, and Private Service',
    '사회복지사업', 'Health/Social Work',
    '교육서비스',   'Education',
    '사회보장행정', 'Public Administration, Defence, and Social Security',
    '사업서비스업', 'Real Estate/Business Service',
    '금융',         'Finance/Insurance',
    '운수',         'Transportation/Telecommunication',
    '숙박',         'Hotel/Restaurant',
    '소매',         'Wholesale/Retail and Consumer Good Repairment',
    '건설업',       'Construction',
    '전기,가스',    'Electricity, Gas, and Water',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '어업',         'Fishery',
    '농업',         'Farming/Forestry'
  )
  
  # 8th KSIC
  # Used in KEAPS surveys 2000 - 2008
  ksic_mapping_wave8 <- tribble(
    ~pattern,        ~eng,
    '국제',         'International/Foreign',
    '가사서비스',   'House Chore Service',
    '개인서비스업', 'Other Public, Repairment, and Private Service',
    '오락',         'Entertainment/Culture/Sports',
    '사회복지사업', 'Health/Social Work',
    '교육서비스',   'Education',
    '사회보장행정', 'Public Administration, Defence, and Social Security',
    '사업서비스업', 'Business Service',
    '부동산',       'Real Estate',
    '금융',         'Finance/Insurance',
    '통신',         'Telecommunication',
    '운수',         'Transportation',
    '숙박',         'Hotel/Restaurant',
    '소매',         'Wholesale/Retail',
    '건설업',       'Construction',
    '전기,가스',    'Electricity, Gas, and Water',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '어업',         'Fishery',
    '농업',         'Farming/Forestry'
  )
  
  # 9th KSIC
  # Used in KEAPS surveys 2004 - 2017
  ksic_mapping_wave9 <- tribble(
    ~pattern,       ~eng,
    '국제',         'International/Foreign',
    '자가소비',     'Household Consumption',
    '개인서비스',   'Organization, Repairment, and Private Service',
    '예술',         'Arts, Sports, and Leisure',
    '사회복지',     'Health/Social Work',
    '교육서비스',   'Education',
    '사회보장',     'Public Administration, Defence, and Social Security',
    '사업',         'Business Infrastructure Management and Support Service',
    '전문',         'Professional, Science, Technology Service',
    '부동산',       'Real Estate',
    '금융',         'Finance/Insurance',
    '통신',         'Press, Video, Communication, and Informaton',
    '운수',         'Transportation',
    '숙박',         'Hotel/Restaurant',
    '소매',         'Wholesale/Retail',
    '건설업',       'Construction',
    '전기',         'Electricity, Gas, and Water',
    '하수',         'Sewer/Disposal/Recycle/Environment',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '농업',         'Farming/Forestry/Fishery'
  )
  
  # 10th/11th KSIC
  # Used in KEAPS surveys 2013 - 2024
  ksic_mapping_wave10 <- tribble(
    ~pattern,        ~eng,
    '국제',         'International/Foreign',
    '자가소비',     'Household Consumption',
    '개인서비스',   'Organization, Repairment, and Private Service',
    '예술',         'Arts, Sports, and Leisure',
    '사회복지',     'Health/Social Work',
    '교육서비스',   'Education',
    '사회보장',     'Public Administration, Defence, and Social Security',
    '사업',         'Business Infrastructure Management/Support/Rent Service',
    '전문',         'Professional, Science, Technology Service',
    '부동산',       'Real Estate',
    '금융',         'Finance/Insurance',
    '통신',         'Informaton Communication',
    '운수',         'Transportation/Storage',
    '숙박',         'Hotel/Restaurant',
    '소매',         'Wholesale/Retail',
    '건설업',       'Construction',
    '전기',         'Electricity/Gas/Steam/Air Condition',
    '하수',         'Water/Sewer/Disposal/Recycle',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '농업',         'Farming/Forestry/Fishery'
  )
  
  ksic_mapping_wave11 <- tribble(
    ~pattern,       ~eng,
    '국제',         'International/Foreign',
    '자가소비',     'Household Consumption',
    '개인서비스',   'Organization, Repairment, and Private Service',
    '예술',         'Arts, Sports, and Leisure',
    '사회복지',     'Health/Social Work',
    '교육서비스',   'Education',
    '사회보장',     'Public Administration, Defence, and Social Security',
    '사업',         'Business Infrastructure Management/Support/Rent Service',
    '전문',         'Professional, Science, Technology Service',
    '부동산',       'Real Estate',
    '금융',         'Finance/Insurance',
    '통신',         'Informaton Communication',
    '운수',         'Transportation/Storage',
    '숙박',         'Hotel/Restaurant',
    '소매',         'Wholesale/Retail',
    '건설업',       'Construction',
    '전기',         'Electricity/Gas/Steam/Air Condition',
    '하수',         'Water/Sewer/Disposal/Recycle',
    '제조업',       'Manufacturing',
    '광업',         'Mining',
    '농업',         'Farming/Forestry/Fishery'
  )
  
  # Combine all wave mappings into a single list
  ksic_mappings <- list(
    'wave5' = ksic_mapping_wave5,
    'wave6' = ksic_mapping_wave6,
    'wave6_1998' = ksic_mapping_wave6_1998,
    'wave7' = ksic_mapping_wave7,
    'wave8' = ksic_mapping_wave8,
    'wave9' = ksic_mapping_wave9,
    'wave10' = ksic_mapping_wave10,
    'wave11' = ksic_mapping_wave11
  )
  
  return (ksic_mappings)
}