############################################################################## #
# Data Cleaning for Korean Economically Active Population Survey (KEAPS)
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
#----------------------------------------------------------------------------- -
#
# Pipeline:
# 0. [External/Pre-requisite] KOSIS initial data cleaning code
#   -> Download raw KEAPS data and apply initial data cleaning code
# 1. data_cleaning.R: Standardizes variables, merges data
#   + Standardizes variable name
#   + Recodes key variables (econ_act, emp_type, industry code)
#   + Uses module KSIC_ind_code.R for industry recoding
#   + Merges across years (1981 - 2024)
#   + Computes monthly self-employment rates (overall, industry, sex/age groups)
# 2. data_visualization.R: Loads yearly trends & generates plots
#   + Loads yearly trends and generate plots
#
#----------------------------------------------------------------------------- -
#
# Purpose:
# - Load year trend .rdata file
# - Provide plots:
#   + Overall trend from 1981 - 2024
#   + Trend by industry from 1993 - 2024
#   + Changes in self-employment across age and sex groups
#     * Illustrates trends on 5 year basis
#
#----------------------------------------------------------------------------- -
#
# Notes:
# - KEAPS survey spans from 1981 to 2024
# - Issues relevant to industry trends:
#   + Although industry data starts from 1986, plots from 1993
#     * Restriction due to the number of KSIC codes
#   + Some of KSIC codes are integrated for illustration
# - 1997 Financial Crisis (IMF) is denoted with red skipped line
#   + Note that 1997 Financial Crisis was publicize on November 1997
#
#----------------------------------------------------------------------------- -
#
# Dependencies:
# - Libraries:
#   + tidyverse: data handling, plotting
#   + patchwork: merging plots
# - .rdata files must be stored in "Data" folder
#   + year_overall.rdata, year_ind.rdata, year_as.rdata, year_as_wg.rdata
#   + Refer to data_clean.R for these files
#
#----------------------------------------------------------------------------- -
#
# Output Flow:
# - year_overall          ->    overall_trend_plot
# - year_ind              ->    industry_trend_plot
# - year_as + year_as_wg  ->    as_merge_trend_plot
#
############################################################################## #

#### ==== 0. Initial set up ====
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)
library(readxl)
library(data.table)

source(here::here('R/KEAPS/module/visualization/plots.R'), local = environment())

generate_plots <- function () {
  #### ==== 1. Overall self-employment trends (1981 - 2024) ====
  overall_trend <- get_overall_trend()
  
  ggsave(
    filename = 'overall_trend.jpeg',
    plot = overall_trend,
    device = 'jpeg',
    path = 'image/',
    width = 10, height = 7,
    dpi = 600
  )
  
  #### ==== 2. Trends by industry (1993 - 2024) ====
  industry_plots <- get_top_industry_trend()
  
  ggsave(
    filename = 'industry_trend.jpeg',
    plot = industry_plots[[1]],
    device = 'jpeg',
    path = 'image/',
    width = 12, height = 9,
    dpi = 600
  )
  
  compare_industry <- industry_plots[[1]] + industry_plots[[2]] +
    plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
  
  ggsave(
    filename = 'with_without_employee_industry_trend.jpeg',
    plot = compare_industry,
    device = 'jpeg',
    path = 'image/',
    width = 12, height = 9,
    dpi = 600
  )
  
  
  #### ==== 3. Trends by groups (1984 - 2024) ====
  agegroup_plots <- get_agegroup_trend()
  
  decomp_within_age <- agegroup_plots[[1]] + agegroup_plots[[2]] +
    plot_layout(guides = 'collect') &
    theme(legend.position = 'bottom')
  
  ggsave(
    filename = 'age_group.jpeg',
    plot = decomp_within_age,
    device = 'jpeg',
    path = 'image/',
    width = 10, height = 7,
    dpi = 600
  )
  
  
  #### ==== 4. Comparison between our replication and official data ====
  official_comparison_plots <- get_official_comparison_plot()
  
  ggsave(
    filename = 'official_survey_comparison.jpeg',
    plot = official_comparison_plots[[1]],
    device = 'jpeg',
    path = 'image/',
    width = 10, height = 7,
    dpi = 600
  )
  
  #### ==== 5. Tax data comparison ====
  tax_compare_plot <- get_tax_comparison_plot()
  
  ggsave(
    filename = 'tax_survey_comparison.jpeg',
    plot = tax_compare_plot,
    device = 'jpeg',
    path = 'image/',
    width = 10, height = 7,
    dpi = 600
  )
  
  #### ==== 5. Supplementary ====
  supp_plots <- get_supp_trends()
  
  ggsave(
    filename = 'secondary_work.jpeg',
    plot = supp_plots[[3]],
    device = 'jpeg',
    path = 'image/',
    width = 8, height = 5,
    dpi = 600
  )
  
  adj_plot <- get_sec_adj_plot()
  
  ggsave(
    filename = 'secondary_work_adjusted.jpeg',
    plot = adj_plot,
    device = 'jpeg',
    path = 'image/',
    width = 8, height = 5,
    dpi = 600
  )
}
