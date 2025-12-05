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
    group_by(!!!group_syms) %>%
    summarize(
      self_pct_1 = weighted.mean(self_emp_1, weight),
      self_pct_2 = weighted.mean(self_emp_2, weight),
      self_pct_3 = weighted.mean(self_emp_3, weight),
      .groups = 'drop'
    )
  
  data_b <- data %>%
    filter(emp_type != 'Family worker') %>%
    group_by(!!!group_syms) %>%
    summarize(
      self_pct_2_t = weighted.mean(self_emp_2, weight),
      self_pct_3_t = weighted.mean(self_emp_3, weight),
      .groups = 'drop'
    )
  
  join_keys <- c(group_vars)
  if (is.null(group_vars)) {
    result <- bind_cols(data_a, data_b) %>%
      mutate(year = survey_year) %>%
      select(year, everything())
  } else {
    result <- left_join(data_a, data_b, by = join_keys) %>%
      mutate(year = survey_year) %>%
      select(year, everything())
  }
  
  
  return(result)
}

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
regular_rate_quarter <- function(data, group_vars, survey_year){
  
  group_syms <- syms(group_vars)
  
  data_a <- data %>%
    group_by(quarter, !!!group_syms) %>%
    summarize(
      self_pct_1 = weighted.mean(self_emp_1, weight),
      self_pct_2 = weighted.mean(self_emp_2, weight),
      self_pct_3 = weighted.mean(self_emp_3, weight),
      .groups = 'drop'
    )
  
  data_b <- data %>%
    filter(emp_type != 'Family worker') %>%
    group_by(quarter, !!!group_syms) %>%
    summarize(
      self_pct_2_t = weighted.mean(self_emp_2, weight),
      self_pct_3_t = weighted.mean(self_emp_3, weight),
      .groups = 'drop'
    )
  
  join_keys <- c('quarter', group_vars)
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
    group_by(!!!group_syms) %>%
    summarize(
      self_pct_1 = sum(self_emp_1 * weight),
      self_pct_2 = sum(self_emp_2 * weight),
      self_pct_3 = sum(self_emp_3 * weight),
      tot = sum(weight),
      .groups = 'drop'
    ) %>%
    mutate(
      tot = sum(tot),
      self_pct_1 = self_pct_1 / tot,
      self_pct_2 = self_pct_2 / tot,
      self_pct_3 = self_pct_3 / tot
    ) %>%
    select(-tot)
  
  # Tax data comparable version
  data_b <- data %>%
    filter(emp_type != 'Family worker') %>%
    group_by(!!!group_syms) %>%
    summarize(
      self_pct_2_t = sum(self_emp_2 * weight),
      self_pct_3_t = sum(self_emp_3 * weight),
      tot = sum(weight),
      .groups = 'drop_last'
    ) %>%
    mutate(
      tot = sum(tot),
      self_pct_2_t = self_pct_2_t / tot,
      self_pct_3_t = self_pct_3_t / tot
    ) %>%
    select(-tot)
  
  join_keys <- c(group_vars)
  result <- left_join(data_a, data_b, by = join_keys) %>%
    mutate(year = survey_year)
  
  result <- result %>%
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
decomp_rate_quarter <- function(data, group_vars, survey_year){
  
  group_syms <- syms(group_vars)
  
  # Usual self-employment version
  data_a <- data %>%
    group_by(quarter, !!!group_syms) %>%
    summarize(
      self_pct_1 = sum(self_emp_1 * weight),
      self_pct_2 = sum(self_emp_2 * weight),
      self_pct_3 = sum(self_emp_3 * weight),
      tot = sum(weight),
      .groups = 'drop'
    ) %>%
    group_by(quarter) %>%
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
    group_by(quarter, !!!group_syms) %>%
    summarize(
      self_pct_2_t = sum(self_emp_2 * weight),
      self_pct_3_t = sum(self_emp_3 * weight),
      tot = sum(weight),
      .groups = 'drop_last'
    ) %>%
    group_by(quarter) %>%
    mutate(
      tot = sum(tot),
      self_pct_2_t = self_pct_2_t / tot,
      self_pct_3_t = self_pct_3_t / tot
    ) %>%
    ungroup() %>%
    select(-tot)
  
  join_keys <- c('quarter', group_vars)
  result <- left_join(data_a, data_b, by = join_keys) %>%
    mutate(year = survey_year)
  
  result <- result %>%
    select(year, quarter, everything())
  return(result)
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
    select(-quarter)
  
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
