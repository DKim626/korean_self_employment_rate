library(dplyr)
library(readr)

load('data/output/month_trends_age_group_2.rdata')
load('data/output/year_trends_age_group_2.rdata')

month_overall_1 <- month_overall
month_ind_1 <- month_ind
month_as_1 <- month_as
month_as_wg_1 <- month_as_wg

year_overall_1 <- year_overall
year_ind_1 <- year_ind
year_as_1 <- year_as
year_as_wg_1 <- year_as_wg

rm(month_overall, month_ind, month_as, month_as_wg)
load('data/output/month_trends.rdata')
sum(month_overall_1$self_pct_1 != month_overall$self_pct_1)
sum(month_overall_1$self_pct_2 != month_overall$self_pct_2)
sum(month_overall_1$self_pct_3 != month_overall$self_pct_3)
sum(month_overall_1$self_pct_2_t != month_overall$self_pct_2_t)
sum(month_overall_1$self_pct_3_t != month_overall$self_pct_3_t)

month_ind <- month_ind %>% filter(ind != '0')
sum(month_ind_1$self_pct_1 != month_ind$self_pct_1)
sum(month_ind_1$self_pct_2 != month_ind$self_pct_2)
sum(month_ind_1$self_pct_3 != month_ind$self_pct_3)
sum(month_ind_1$self_pct_2_t != month_ind$self_pct_2_t)
sum(month_ind_1$self_pct_3_t != month_ind$self_pct_3_t)

sum(month_as_1$self_pct_1 != month_as$self_pct_1)
sum(month_as_1$self_pct_2 != month_as$self_pct_2)
sum(month_as_1$self_pct_3 != month_as$self_pct_3)
sum(month_as_1$self_pct_2_t != month_as$self_pct_2_t)
sum(month_as_1$self_pct_3_t != month_as$self_pct_3_t)

sum(month_as_wg_1$self_pct_1 != month_as_wg$self_pct_1)
sum(month_as_wg_1$self_pct_2 != month_as_wg$self_pct_2)
sum(month_as_wg_1$self_pct_3 != month_as_wg$self_pct_3)
sum(month_as_wg_1$self_pct_2_t != month_as_wg$self_pct_2_t)
sum(month_as_wg_1$self_pct_3_t != month_as_wg$self_pct_3_t)



rm(year_overall, year_ind, year_as, year_as_wg)
load('data/output/year_trends.rdata')
sum(year_overall_1$self_pct_1 != year_overall$self_pct_1)
sum(year_overall_1$self_pct_2 != year_overall$self_pct_2)
sum(year_overall_1$self_pct_3 != year_overall$self_pct_3)
sum(year_overall_1$self_pct_2_t != year_overall$self_pct_2_t)
sum(year_overall_1$self_pct_3_t != year_overall$self_pct_3_t)

year_ind <- year_ind %>% filter(ind != '0')
year_ind <- year_ind %>%
  mutate(wave = as.numeric(wave)) %>%
  arrange(year, wave, ind)
year_ind_1 <- year_ind_1 %>%
  arrange(year, wave, ind)
sum(year_ind_1$self_pct_1 != year_ind$self_pct_1)
sum(year_ind_1$self_pct_2 != year_ind$self_pct_2)
sum(year_ind_1$self_pct_3 != year_ind$self_pct_3)
sum(year_ind_1$self_pct_2_t != year_ind$self_pct_2_t)
sum(year_ind_1$self_pct_3_t != year_ind$self_pct_3_t)

year_ind_1[year_ind_1$self_pct_1 != year_ind$self_pct_1,]
year_ind[year_ind_1$self_pct_1 != year_ind$self_pct_1,]

sum(year_as_1$self_pct_1 != year_as$self_pct_1)
sum(year_as_1$self_pct_2 != year_as$self_pct_2)
sum(year_as_1$self_pct_3 != year_as$self_pct_3)
sum(year_as_1$self_pct_2_t != year_as$self_pct_2_t)
sum(year_as_1$self_pct_3_t != year_as$self_pct_3_t)

sum(year_as_wg_1$self_pct_1 != year_as_wg$self_pct_1)
sum(year_as_wg_1$self_pct_2 != year_as_wg$self_pct_2)
sum(year_as_wg_1$self_pct_3 != year_as_wg$self_pct_3)
sum(year_as_wg_1$self_pct_2_t != year_as_wg$self_pct_2_t)
sum(year_as_wg_1$self_pct_3_t != year_as_wg$self_pct_3_t)



load('data/output/month_trends_age_group_2.rdata')
load('data/output/year_trends_age_group_2.rdata')

month_ind <- select(month_ind,
                    year, month, wave, ind, ind_kor, everything()
                    )

write_excel_csv(month_ind, file = 'data/output/month_trends_ind.csv')
write_excel_csv(year_ind, file = 'data/output/year_trends_ind.csv')

temp <- readRDS('data/clean/cleaned_data_2024.rds')

temp <- readRDS('data/output/')