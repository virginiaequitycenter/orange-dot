# UW Self-Sufficiency Standard
# Source: https://selfsufficiencystandard.org/Virginia/

# Load packages ----
library(here)
library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

# Set WD
setwd(here("OrangeDot6_files"))

# Localities ----
county_codes <- c("003", "540", "065", "079", "109", "125", "029")
localities <- c("Albemarle County", "Buckingham County", "Charlottesville city", "Fluvanna County", "Greene County", "Louisa County", "Nelson County")

# Read data
self_suff_byfamily <- read_excel("data/tempdata/VA2021_all_families.xlsx", sheet = "By Family") %>% 
  clean_names()

# Family Composition: Single Parent + 1 preschooler + 1 school age
self_suff_a1p1s1 <- self_suff_byfamily %>% 
  filter(county %in% localities) %>% 
  filter(family_type == "a1i0p1s1t0") %>% 
  rename_with(~paste0(. , "_a1p1s1"), 11:24)

# SSS Mean across family compositions (Adults: 1|2, Total Children (all age): 1|2)
self_suff_mean <- self_suff_byfamily %>% 
  filter(county %in% localities) %>% 
  mutate_at(c("adult_s", "infant_s","preshooler_s", "schoolager_s", "teenager_s"), as.numeric) %>% 
  mutate(total_child = rowSums(across(infant_s:teenager_s), na.rm =TRUE)) %>% 
  filter(adult_s <= 2 & total_child <= 2) %>%
  group_by(county, state, year) %>% 
  summarise(housing_costs = round(mean(housing_costs), 2),
            child_care_costs = round(mean(child_care_costs), 2),
            food_costs = round(mean(food_costs), 2),
            transportation_costs = round(mean(transportation_costs), 2),
            health_care_costs = round(mean(health_care_costs), 2),
            miscellaneous_costs = round(mean(miscellaneous_costs), 2),
            taxes = round(mean(taxes), 2),
            earned_income_tax_credit = round(mean(earned_income_tax_credit), 2),
            child_tax_credit = round(mean(child_tax_credit), 2),
            hourly_self_sufficiency_wage = round(mean(hourly_self_sufficiency_wage), 2),
            monthly_self_sufficiency_wage = round(mean(monthly_self_sufficiency_wage), 2),
            annual_self_sufficiency_wage = round(mean(annual_self_sufficiency_wage), 2),
            emergency_savings = round(mean(emergency_savings),2)) %>% 
  rename_with(~paste0(. , "_mean"), 4:16)
  


mean_ssw <- mean(self_suff_mean$annual_self_sufficiency_wage_mean)

# Join tables
self_suff_county <- self_suff_a1p1s1 %>% 
  left_join(self_suff_mean, by = join_by(state, year, county)) %>% 
  select(state, year, county, annual_self_sufficiency_wage_a1p1s1, annual_self_sufficiency_wage_mean,
         housing_costs_a1p1s1, housing_costs_mean, child_care_costs_a1p1s1, child_care_costs_mean,
         food_costs_a1p1s1, food_costs_mean, transportation_costs_a1p1s1, transportation_costs_mean,
         health_care_costs_a1p1s1, health_care_costs_mean, miscellaneous_costs_a1p1s1, miscellaneous_costs_mean,
         taxes_a1p1s1, taxes_mean, hourly_self_sufficiency_wage_a1p1s1, hourly_self_sufficiency_wage_mean,
         monthly_self_sufficiency_wage_a1p1s1, monthly_self_sufficiency_wage_mean 
         )



# Write CSV
write_csv(self_suff_county, "data/self_suff_county_2021.csv")
