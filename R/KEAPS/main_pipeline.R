############################################################################## #
# Pipeline for Self-employment Rate Analysis (KEAPS 1981 - 2024)
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Description
# - This script will sequentially run the entire analysis pipeline.
# - It uses isolated environments for each stage to avoid namespace conflicts
#
# Pipeline steps:
# 1. Preprocessing: Executes raw KOSIS .r scripts to extract coded data
# 2. Cleaning: Standardizes variables and recodes Korean -> English
# 3. Calculation: Computes self-employment rate trends
# 4. Visualization: Generates plots from the trends
#
#----------------------------------------------------------------------------- -


library(here)

# Create isolated environments for each step
env_list <- list(
  preprocess = new.env(), # Step 1: preprocessing with KOSIS .R script
  clean      = new.env(), # Step 2: data cleaning
  rate_calc  = new.env(), # Step 3: Self-employment calculation
  viz        = new.env()  # Step 4: Visualization
)

#### ==== Step 1. Preprocessing with KOSIS .R script ====
# - Runs .R scripts provided by KOSIS to extract raw survey data
source(here('R/KEAPS/01_preprocess.R'), local = env_list$preprocess)
env_list$preprocess$run_preprocess()

#### ==== Step 2. Data cleaning ====
# - Standardizes variable names and codes
# - Saved cleaned data as .rds files
source(here('R/KEAPS/02_data_clean.R'), local = env_list$clean)
env_list$clean$run_data_cleaning(start_year = 1981, end_year = 2024)

#### ==== Step 3. Self-employment rate calculation ====
# - Calculate monthly and yearly trends for:
#   - Overall rate
#   - By industry
#   - By age and gender groups
#   - Withing age and gender groups
#   - Supplementary: Labor force participation and unemployment rate
source(here('R/KEAPS/03_rate_calculation.R'), local = env_list$rate_calc)
env_list$rate_calc$run_rate_calc(target_age_group = 'age_group_2',
                                        start_year = 1981,
                                        end_year = 2024
                                        )

#### ==== Step 4. Data visualization ====
# - Loads aggregated yearly trends and generates plots
source(here('R/KEAPS/04_data_visualization.R'), local = env_list$viz)
env_list$viz$generate_plots()
