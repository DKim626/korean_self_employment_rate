# Loads module for variable selection and rename
source(file = here('R/KHTS/module/cleaning/variable_select_rename.R'),
       local = environment())
# Loads industry key mapping and function
source(file = here('R/KHTS/module/cleaning/variable_recode.R'),
       local = environment())
# Loads industry key mapping and function
source(file = here('R/KHTS/module/cleaning/ksic_mapping.R'),
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
#' Cleans KHTS survey data of given year.
#' @param year Numeric value of survey year within the range between 1981 and 2024.
clean_one_year <- function(year) {
  on.exit(gc())
  ##### ==== 1. Load .rds for each year ====
  assert_that(is.numeric(year), length(year) == 1)
  assert_that(
    file.exists('data/KHTS/processed'),
    msg = "[ERROR] File directory data/processed/ does not exist."
  )
  
  survey_year <- year
  
  rds_path <- sprintf('data/KHTS/processed/data_%s.rds', survey_year)
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
  
  hh_common <- if (year >= 2017) c('hhid', 'quarter') else 'hhid'
  sym_hh <- syms(hh_common)
  
  data_hh <- data %>%
    select(!!!sym_hh, contains('income'), weight)
  
  data_ind <- data %>%
    select(!!!sym_hh, contains(c('sex', 'age', 'rel', 'emp_type', 'employed', 'ind')))
  
  var_list <- c('sex', 'age', 'rel', 'emp_type', 'employed')
  if (any(stri_detect_fixed(colnames(data_ind), 'ind_9'))) var_list <- c(var_list, 'ind_9')
  if (any(stri_detect_fixed(colnames(data_ind), 'ind_10'))) var_list <- c(var_list, 'ind_10')
  
  data_ind <- map(var_list,
                  \(var_col) {
                    
                    temp <- data_ind %>%
                      select(!!!sym_hh, contains(var_col)) %>%
                      pivot_longer(contains(var_col),
                                   names_to = 'var', values_to = var_col) %>%
                      select(-var)
                    
                    if (var_col != 'sex') temp <- temp %>% select(contains(var_col))
                    
                    return (temp)
                  }) %>%
    bind_cols() %>%
    filter(!is.na(age))
  
  data <- left_join(data_hh, data_ind, by = hh_common) %>%
    filter(age >= 15)
  
  #### ==== 3. Recode variables ====
  # Add month variable
  # data <- extract_month(data)
  if (year >= 2014) {
    data <- data %>%
      mutate(across(c(sex, emp_type, employed, any_of(contains('ind'))),
                    \(x) as.character(x))) %>%
      mutate(across(c(rel), \(x) as.numeric(x)))
  }
  data$sex <- recode_sex(data$sex)
  data$rel <- recode_rel(data$rel)
  data$emp_type <- recode_emp_type(data$emp_type)
  data$employed <- recode_employed(data$employed)
  
  data <- data %>%
    group_by(hhid) %>%
    mutate(
      work_num_o = sum(employed == 'Employed' & rel == 'Other' &
                         stri_detect_fixed(emp_type, 'Self-employed'))
    ) %>%
    ungroup() %>%
    mutate(
      inc_busi = fcase(
        rel == "Head", income_business_rp,
        rel == 'Spouse', income_business_sp,
        rel == 'Other' & employed == 'Employed' &
          stri_detect_fixed(emp_type, 'Self-employed'),
        income_business_o / work_num_o
      ),
      inc_lab = fcase(
        rel == "Head", income_labor_rp,
        rel == 'Spouse', income_labor_sp,
        rel == 'Other' & employed == 'Employed' &
          stri_detect_fixed(emp_type, 'Self-employed'),
        income_labor_o / work_num_o
      )
    ) %>%
    select(any_of(hh_common), income, sex:inc_lab, weight)
  
  
  data <- data %>%
    bind_cols(construct_age_group(data$age))
  
  ind_cols <- grep('ind', colnames(data), value = TRUE)
  
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
  
  #### ==== 4. Constructing self-employment variable ====
  data <- data %>%
    bind_cols(construct_self_emp(data$emp_type))
  
  #### ==== 5. Save cleaned .rds file ====
  file_path <- here(paste0('data/KHTS/clean/cleaned_data_', survey_year, '.rds'))
  
  saveRDS(data, file = file_path)
  
  message("[INFO] Cleaned data of year ", year, " saved at ", file_path)
  
}

