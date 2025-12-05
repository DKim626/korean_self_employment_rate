library(here)
library(dplyr)
library(tidyr)
library(tibble)
library(data.table)

# Create isolated environments for each step
env_list <- list(
  preprocess = new.env(), # Step 1: preprocessing with KOSIS .R script
  clean      = new.env(), # Step 2: data cleaning
  rate_calc  = new.env() # Step 3: Self-employment calculation
)

#### ==== Step 1. Preprocessing with KOSIS .R script ====
# - Runs .R scripts provided by KOSIS to extract raw survey data
source(here('R/KHTS/01_preprocess.R'), local = env_list$preprocess)
env_list$preprocess$run_preprocess()

#### ==== Step 2. Data cleaning ====
# - Standardizes variable names and codes
# - Saved cleaned data as .rds files
source(here('R/KHTS/02_data_clean.R'), local = env_list$clean)
env_list$clean$run_data_cleaning(start_year = 2010, end_year = 2024)

#### ==== Step 3. Self-employment rate calculation ====
# - Calculate monthly and yearly trends for:
#   - Overall rate
#   - By industry
#   - By age and gender groups
#   - Withing age and gender groups
#   - Supplementary: Labor force participation and unemployment rate
source(here('R/KHTS/03_rate_calculation.R'), local = env_list$rate_calc)
env_list$rate_calc$run_rate_calc(target_age_group = 'age_group_2',
                                 start_year = 2010,
                                 end_year = 2024
)
