# AUTHOR: Colin Cookman
# CONTACT: ccookman at gmail dot com
# DATE: May 23 2018
#
library(tidyverse)
census_data <- read_csv("Pakistan_2017_Census_Blocks.csv")
#
# aggregate by first administrative sublevel (in most cases, tehsils)
#
tehsil_data <- census_data %>% group_by(report_date, province, district, sublvl_01) %>%
  summarize(
    total_blocks = n(),
    missing_blocks = sum(is.na(population)),
    population = sum(population, na.rm = TRUE),
    households = sum(households, na.rm = TRUE),
    avg_hh_size = population / households
  )
#
summary(tehsil_data)  
