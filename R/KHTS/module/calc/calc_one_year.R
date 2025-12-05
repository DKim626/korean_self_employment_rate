source(here('R/KHTS/module/calc/rate_calc.R'), local = environment())

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
  file_path <- paste0('data/KHTS/clean/cleaned_data_', survey_year, '.rds')
  if(!file.exists('data/KHTS/clean')) {
    stop('[ERROR] File directory data/KHTS/clean/ does not exist')
  } else if(!file.exists(file_path)) {
    stop('[ERROR] Cleaned data set does not exist for year ', survey_year)
  }
  data <- readRDS(file = file_path)
  
  #### ==== 3. Filter to labor force only ====
  data_t <- data %>%
    filter(inc_lab > 0 | inc_busi > 0)
  data <- data %>%
    filter(employed == "Employed")
  
  #### ==== 4. Compute overall self-employment trend ====
  if (survey_year < 2017) {
    data_overall <- regular_rate(data, group_vars = NULL, survey_year)
  } else if (survey_year >= 2017) {
    data_overall <- regular_rate_quarter(data, group_vars = NULL, survey_year) %>%
      year_trends()
  }
  
  
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
        
        if (survey_year < 2017) {
          
          data_ind_list[[length(data_ind_list) + 1]] <- decomp_rate(
            data = data, group_vars = v, survey_year = survey_year
          ) %>%
            rename(ind = all_of(v)) %>%
            mutate(wave = as.integer(stri_extract_first_regex(v, '\\d+'))) %>%
            left_join(key_kor_v, by = c('ind' = v))
          
        } else if (survey_year >= 2017) {
          
          data_ind_list[[length(data_ind_list) + 1]] <- decomp_rate_quarter(
            data = data, group_vars = v, survey_year = survey_year
          ) %>%
            rename(ind = all_of(v)) %>%
            mutate(wave = as.integer(stri_extract_first_regex(v, '\\d+'))) %>%
            left_join(key_kor_v, by = c('ind' = v)) %>%
            year_trends(c('wave', 'ind', 'ind_kor'))
          
        }
        
      }
    }
  }
  
  data_ind <- if(length(data_ind_list) != 0) bind_rows(data_ind_list) else NULL
  
  rm(data_ind_list)
  
  #### ==== 6. Compute group-wise trend ====
  if (survey_year < 2017) {
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
    
  } else if (survey_year >= 2017) {
    data_a <- decomp_rate_quarter(
      data = data,
      group_vars = c(target_age_group),
      survey_year = survey_year
    ) %>%
      year_trends(c(target_age_group))
    data_as <- decomp_rate_quarter(
      data = data,
      group_vars = c(target_age_group, 'sex'),
      survey_year = survey_year
    ) %>%
      year_trends(c(target_age_group, 'sex'))
  }
  
  #### ==== 7. Compute within-group trend ====
  if (survey_year < 2017) {
    data_as_wg <- regular_rate(
      data = data,
      group_vars = c(target_age_group, 'sex'),
      survey_year = survey_year
    )
  } else if (survey_year >= 2017) {
    data_as_wg <- regular_rate_quarter(
      data = data,
      group_vars = c(target_age_group, 'sex'),
      survey_year = survey_year
    ) %>%
      year_trends(c(target_age_group, 'sex'))
  }
  
  if (survey_year < 2017) {
    
    data_inc_busi <- data_t %>%
      mutate(
        inc_busi = ifelse(inc_busi > 0, 1, 0),
        inc_busi = ifelse(!is.na(inc_busi), inc_busi, 0)
        ) %>%
      summarize(inc_busi_rate = weighted.mean(inc_busi, weight)) %>%
      mutate(year = survey_year) %>%
      select(year, everything())
    
  } else if (survey_year >= 2017) {
    
    data_inc_busi <- data_t %>%
      mutate(
        inc_busi = ifelse(inc_busi > 0, 1, 0),
        inc_busi = ifelse(!is.na(inc_busi), inc_busi, 0)
      ) %>%
      group_by(quarter) %>%
      summarize(
        inc_busi_rate = weighted.mean(inc_busi, weight),
        .groups = 'drop'
        ) %>%
      summarize(inc_busi_rate = mean(inc_busi_rate)) %>%
      mutate(year = survey_year) %>%
      select(year, everything())
    
  }
  
  #### ==== 8. Return result as a named list ====
  result <- list(
    year = survey_year,
    overall = data_overall,
    ind = if(!is.null(data_ind) && nrow(data_ind) > 0) data_ind else NULL,
    a = data_a,
    as = data_as,
    as_wg = data_as_wg,
    busi_inc = data_inc_busi
  )
  
  return(result)
  
}
