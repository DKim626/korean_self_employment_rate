

load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
load('data/KHTS/output/year_trends_age_group_2.rdata', envir = environment())

year_overall
trend_overall
trend_busi_inc
trend_ind

plot <- inner_join(
  year_overall %>%
    select(year, self_pct_2_t),
  trend_overall %>%
    select(year, self_pct_2_t),
  by = 'year',
  suffix = c('_KEAPS', '_KHTS')
) %>%
  inner_join(
    trend_busi_inc %>%
      select(year, inc_busi_rate),
    by = 'year'
  ) %>%
  pivot_longer(self_pct_2_t_KEAPS:inc_busi_rate,
               names_to = 'type', values_to = 'rate') %>%
  mutate(
    type = fcase(
      stri_detect_fixed(type, "KEAPS"), "KEAPS Rate",
      stri_detect_fixed(type, "KHTS"), "KHTS Rate",
      type == 'inc_busi_rate', "KHTS Business Income Based"
    ),
    type = factor(type,
                  levels = c("KEAPS Rate", "KHTS Rate",
                             "KHTS Business Income Based"))
  ) %>%
  ggplot(aes(x = year, y = rate, color = type)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 45)
  ) +
  scale_y_continuous(limits = c(0, 0.35)) +
  scale_x_continuous(breaks = 2010:2024)

ggsave(
  filename = 'different_trend.jpeg',
  plot = plot,
  device = 'jpeg',
  path = 'image/',
  width = 12, height = 9,
  dpi = 600
)



get_overall_trend <- function (data) {
  
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  data_plot_overall <- year_overall %>%
    pivot_longer(
      self_pct_1:self_pct_3_t,
      names_to = 'Type', values_to = 'Rate'
    ) %>%
    mutate(
      TC = ifelse(str_detect(Type, '_t$'), 'Tax Data Comparable', 'Not Comparable'),
      Type = str_extract(Type, '\\d')
    )
  
  overall_trend_plot <- ggplot(data = data_plot_overall) +
    geom_line(aes(x = year, y = Rate, color = Type, linetype = TC),
              linewidth = 1) +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    labs(y = 'Self Employment Rate', x = 'Year') +
    scale_x_continuous(breaks = 1981:2024) +
    grafify::scale_color_grafify(palette = 'fishy') +
    annotate(
      'segment', x = 1997, xend = 1997, y = 0, yend = 0.5,
      color = 'red', linetype = 'dashed', linewidth = 1
    ) +
    annotate('text', x = 1997, y = 0.52, label = '1997 IMF Crisis')
  
  return (overall_trend_plot)
}

get_top_industry_trend <- function () {
  
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  # Without employees only
  top_ind_t3 <- year_ind %>%
    mutate(wave = ifelse(wave == '7', '6', wave)) %>%
    filter(ind != '0') %>%
    group_by(wave, ind) %>%
    mutate(
      mean_3 = mean(self_pct_3)
    ) %>%
    ungroup() %>%
    filter(wave != 11 & year >= 1993) %>%
    filter(mean_3 >= 0.005) %>%
    mutate(
      ind = case_when(
        str_detect(ind, 'Farming') ~ 'Farming/Forestry',
        str_detect(ind, 'Private') ~
          'Organization, Repairment, and Private Service',
        str_detect(ind, 'Transportation') ~ 'Transportation',
        str_detect(ind, 'Wholesale') ~ 'Wholesale/Retail',
        str_detect(ind, 'Real Estate') ~ 'Real Estate',
        T ~ ind
      ),
      ind = fct_rev(factor(ind)),
      wave = factor(as.numeric(wave))
    )
  
  industry_trend_plot <- top_ind_t3 %>%
    ggplot(aes(x = year, y = self_pct_3, color = ind)) +
    geom_line(aes(linetype = wave), linewidth = 1.5) +
    geom_vline(xintercept = 1997, color = 'red', linetype = 'dashed') +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    labs(y = 'Self Employment Rate', x = 'Year', color = 'waveustry') +
    scale_x_continuous(breaks = 1993:2024) +
    grafify::scale_color_grafify(palette = 'fishy') +
    scale_y_continuous(breaks = seq(0, 0.08, 0.01), limits = c(0, 0.08))
  
  
  # With employees only
  # List for comparison between with and without employees
  top_ind_t3_list <- top_ind_t3 %>%
    select(year, wave, ind)
  
  # With employees only
  top_ind_t3_comp <- year_ind %>%
    mutate(
      ind = case_when(
        str_detect(ind, 'Farming') ~ 'Farming/Forestry',
        str_detect(ind, 'Private') ~
          'Organization, Repairment, and Private Service',
        str_detect(ind, 'Transportation') ~ 'Transportation',
        str_detect(ind, 'Wholesale') ~ 'Wholesale/Retail',
        str_detect(ind, 'Real Estate') ~ 'Real Estate',
        T ~ ind
      ),
      ind = fct_rev(factor(ind)),
      wave = ifelse(wave == '7', '6', wave),
      wave = factor(as.numeric(wave))
    ) %>%
    right_join(
      top_ind_t3_list, by = c('year', 'ind', 'wave')
    ) %>%
    mutate(
      self_pct_2 = self_pct_2 - self_pct_3
    )
  
  industry_trend_plot_wwo <- top_ind_t3_comp %>%
    ggplot(aes(x = year, y = self_pct_2, color = ind)) +
    geom_line(aes(linetype = wave), linewidth = 1.5) +
    geom_vline(xintercept = 1997, color = 'red', linetype = 'dashed') +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    labs(y = 'Self Employment Rate', x = 'Year', color = 'waveustry') +
    scale_x_continuous(breaks = 1993:2024) +
    grafify::scale_color_grafify(palette = 'fishy') +
    scale_y_continuous(breaks = seq(0, 0.08, 0.01), limits = c(0, 0.08))
  
  plot_list <- list('With Employees' = industry_trend_plot,
                    'Without Employees' = industry_trend_plot_wwo)
  
  return (plot_list)
}

get_agegroup_trend <- function () {
  
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  p_1 <- year_as %>%
    filter(year %in% seq(1984, 2024, by = 5)) %>%
    select(year, age_group_2, sex, self_pct_3, self_pct_3_t) %>%
    ggplot(aes(x = age_group_2, y = self_pct_3, fill = sex)) +
    geom_col(position = position_dodge()) +
    facet_wrap( ~ year) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    labs(x = 'Age Group', y = 'Self-employment Rate',
         fill = 'Gender', title = 'Decomposition of Self-employment Rate') +
    grafify::scale_fill_grafify(palette = 'fishy')
  
  p_2 <- year_as_wg %>%
    filter(year %in% seq(1984, 2024, by = 5)) %>%
    select(year, age_group_2, sex, self_pct_3, self_pct_3_t) %>%
    ggplot(aes(x = age_group_2, y = self_pct_3, fill = sex)) +
    geom_col(position = position_dodge()) +
    facet_wrap( ~ year) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45),
      legend.position = 'bottom'
    ) +
    labs(x = 'Age Group', y = 'Self-employment Rate',
         fill = 'Gender',
         title = 'Self-employment Rate Within Each Age and Gender Group') +
    grafify::scale_fill_grafify(palette = 'fishy')
  
  
  plot_list <- list(
    'Decomposed' = p_1,
    'Within' = p_2
  )
  
  return (plot_list)
}

get_tax_comparison_plot <- function() {
  
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  survey_data <- year_a %>%
    select(year, age_group_2, self_pct_2_t) %>%
    mutate(
      age_group_2 = as.character(age_group_2),
      age = fcase(
        age_group_2 %in% c('15-19', '20-24', '25-29'), '0-29',
        age_group_2 %in% c('30-34', '35-39'), '30-39',
        age_group_2 %in% c('40-44', '45-49'), '40-49',
        age_group_2 %in% c('50-54', '55-59'), '50-59',
        age_group_2 %in% c('60-64', '65-69'), '60-69',
        default = age_group_2
      )
    ) %>%
    group_by(year, age) %>%
    summarize(self_rate = sum(self_pct_2_t), .groups = 'drop')
  
  tax_data <- read_excel('data/external/data.xlsx', sheet = 2) %>%
    rename(year = 년도, age = 나이, count = 관측수) %>%
    mutate(
      age = str_replace_all(age, 'to', '-'),
      age = ifelse(age == '70over', 'Above 70', age)
      ) %>%
    group_by(year, age) %>%
    summarize(count = sum(count), .groups = 'drop') %>%
    mutate(self_rate = ifelse(year == 2015, count / 2037934, count / 2435984)) %>%
    select(year, age, self_rate)
  
  comparison_data <- inner_join(survey_data, tax_data,
                                by = c('year', 'age'),
                                suffix = c('_survey', '_tax')) %>%
    pivot_longer(self_rate_survey:self_rate_tax,
                 names_to = 'data', values_to = 'self_rate') %>%
    mutate(
      year = as.character(year),
      data = fcase(
        str_detect(data, 'survey'), 'Survey',
        str_detect(data, 'tax'), 'Tax'
      )
    )
  
  comparison_plot <- comparison_data %>%
    ggplot(aes(x = age, y = self_rate, fill = data)) +
    geom_col(position = 'dodge') +
    facet_wrap( ~ year) +
    theme_minimal() +
    grafify::scale_fill_grafify(palette = 'fishy') +
    labs(x = 'Age Group', y = 'Self-employment Rate', fill = 'Data') +
    theme(legend.position = 'bottom')
  
  return(comparison_plot)
}

get_official_comparison_plot <- function() {
  
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  official_data <- read_excel('Data/external/self_employment_official.xlsx',
                         skip = 3,
                         col_names = c('Type', 1996:2023)) %>%
    pivot_longer(`1996`:`2023`, names_to = 'year', values_to = 'Number') %>%
    filter(!str_detect(Type, '출처')) %>%
    mutate(
      Type = fcase(
        Type == '취업자', 'employed',
        Type == '자영업자', 'self_employed',
        str_detect(Type, '있는'), 'self_emp_w',
        str_detect(Type, '없는'), 'self_emp_wo',
        Type == '비중', 'self_emp_rate'
      )
    ) %>%
    pivot_wider(names_from = 'Type', values_from = 'Number')
  
  data_survey <- year_overall %>%
    select(year, self_pct_2) %>%
    rename(survey = self_pct_2)
  
  data_official <- official_data %>%
    select(year, self_emp_rate) %>%
    rename(official = self_emp_rate) %>%
    mutate(
      year = as.numeric(year),
      official = as.numeric(official) / 100
      )
  
  merged <- inner_join(data_survey, data_official, by = 'year')
  
  comparison_plot_1 <- merged %>%
    pivot_longer(survey:official, values_to = 'rate', names_to = 'type') %>%
    mutate(
      type = fcase(
        type == 'survey', 'Our Replication',
        type == 'official', 'Official Statistics'
      )
    ) %>%
    ggplot(mapping = aes(x = year, y = rate, color = type)) +
    geom_point(aes(shape = type)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +
    labs(x = 'Year of Survey', y = 'Self-employment Rate',
         color = 'Data Source', shape = 'Data Source') +
    theme_minimal() +
    theme(legend.position = 'bottom') +
    scale_y_continuous(limits = c(0, 0.3))
  
  comparison_plot_2 <- ggplot(data = merged,
                            mapping = aes(x = survey, y = official,
                                          color = year)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +
    theme_minimal()
  
  comparison_list <- list(
    'Style 1' = comparison_plot_1,
    'Style 2' = comparison_plot_2
  )
  
  return (comparison_list)
}

get_supp_trends <- function () {
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  part_rate <- year_supp %>%
    filter(year >= 1990) %>%
    ggplot(mapping = aes(x = year, y = part_rate)) +
    geom_point(shape = 24) +
    geom_line() +
    geom_vline(xintercept = 1997, color = 'red', linetype = 'dashed') +
    annotate('text', x = 1998, y = 0.7, label = 'IMF') +
    theme_minimal() +
    labs(x = 'Year of Survey', y = 'Labor Participation Rate')
  
  unemp_rate <- year_supp %>%
    filter(year >= 1990) %>%
    ggplot(mapping = aes(x = year, y = unemp_rate)) +
    geom_point(shape = 24) +
    geom_line() +
    geom_vline(xintercept = 1997, color = 'red', linetype = 'dashed') +
    annotate('text', x = 1998, y = 0.08, label = 'IMF') +
    theme_minimal() +
    labs(x = 'Year of Survey', y = 'Unemployment Rate')
  
  second_work_rate <- year_supp %>%
    filter(year >= 2003) %>%
    ggplot(mapping = aes(x = year, y = sec_work_rate)) +
    geom_point(shape = 24) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.03)) +
    labs(x = 'Year of Survey', y = 'Secondary Work Participation Rate')
  
  second_work_rate_t <- year_supp %>%
    filter(year >= 2003) %>%
    select(year, sec_work_rate, sec_work_rate_t) %>%
    pivot_longer(sec_work_rate:sec_work_rate_t,
                 values_to = 'sec_work_rate',
                 names_to = 'Type') %>%
    mutate(
      Type = fcase(
        Type == 'sec_work_rate', 'Ordinary',
        Type == 'sec_work_rate_t', 'Tax Comparable'
      )
    ) %>%
    ggplot(mapping = aes(x = year, y = sec_work_rate, color = Type)) +
    geom_point(shape = 24) +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.03)) +
    labs(x = 'Year of Survey', y = 'Secondary Work Participation Rate')
  
  results <- list(
    'Participation Rate' = part_rate,
    'Unemployment Rate' = unemp_rate,
    'Secondary Work Rate' = second_work_rate,
    'Secondary Work Rate (Tax Comparable)' = second_work_rate_t
  )
  
  return(results)
}


get_sec_adj_plot <- function () {
  
  load('data/KEAPS/output/year_trends_age_group_2.rdata', envir = environment())
  
  adj_plot <- left_join(year_overall, year_supp, by = 'year') %>%
    select(year, self_pct_2_t, sec_work_rate_t) %>%
    filter(year >= 2003) %>%
    mutate(adj_self = self_pct_2_t + sec_work_rate_t) %>%
    select(year, self_pct_2_t, adj_self) %>%
    pivot_longer(self_pct_2_t:adj_self,
                 names_to = 'Type',
                 values_to = 'rate') %>%
    mutate(
      Type = fcase(
        Type == 'self_pct_2_t', 'Ordinary',
        Type == 'adj_self', 'Hypothetical'
      ),
      Type = factor(Type, levels = c('Ordinary', 'Hypothetical'))
    ) %>%
    ggplot(aes(x = year, y = rate, color = Type)) +
    geom_point(shape = 24) +
    geom_line(aes(linetype = Type)) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 0.35)) +
    labs(x = 'Year of Survey', y = 'Self-employment Rate')
  
  return (adj_plot)
}

