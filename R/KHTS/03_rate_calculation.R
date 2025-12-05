source(file = here('R/KHTS/module/calc/calc_one_year.R'),
       local = environment())
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
  trend_overall <- map_dfr(results, 'overall') %>%
    arrange(year)
  trend_ind <- map_dfr(results, 'ind') %>%
    arrange(year) %>%
    filter(ind != '0')
  trend_a <- map_dfr(results, 'a') %>%
    arrange(year)
  trend_as <- map_dfr(results, 'as') %>%
    arrange(year)
  trend_as_wg <- map_dfr(results, 'as_wg') %>%
    arrange(year)
  trend_busi_inc <- map_dfr(results, 'busi_inc') %>%
    arrange(year)
  
  ##### ==== 2. Saving the results ====
  if(!file.exists(here('data/KHTS/output'))) {
    dir.create(here('data/KHTS/output'), recursive = TRUE)
    message('[INFO] Directory created: data/KHTS/output/')
  }
  file_trend <- paste0('data/KHTS/output/year_trends_', target_age_group, '.rdata')
  save(
    trend_overall,
    trend_ind,
    trend_a,
    trend_as,
    trend_as_wg,
    trend_busi_inc,
    file = file_trend
  )
  
  elapsed_time <- (proc.time() - start_time)[3]
  elapsed_min <- floor(elapsed_time / 60)
  elapsed_sec <- round(elapsed_time %% 60, 2)
  
  message(sprintf(
    "[INFO] Calculation execution time: %02d minutes %02.2f seconds.",
    elapsed_min, elapsed_sec))
}

