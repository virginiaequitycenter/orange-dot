# BLS data prep
# Employment, median and mean wages by occupation
# Charlottesville Metro area, 2023
# https://data.bls.gov/oes/#/geoOcc/Multiple%20occupations%20for%20one%20geographical%20area

# Setup ----
library(tidyverse)
library(readxl)
library(janitor)


# Data ---- 
bls <- read_excel("downloads/OES_Report.xlsx", skip = 5) %>% 
  clean_names()

# Prep ---- 
bls_occ <- bls[1:386,] %>% 
  mutate(soc_code = str_extract(occupation_soc_code, "(?<=\\()\\d+(?=\\))"),
         occupation = str_remove(occupation_soc_code, "\\([^\\)]+\\)"),
         across(employment_1:employment_per_1_000_jobs, as.numeric)) %>% 
  filter(str_detect(occupation, "Occupations$", negate = TRUE))

bls_catchall_occ <- bls[1:386,] %>% 
  mutate(soc_code = str_extract(occupation_soc_code, "(?<=\\()\\d+(?=\\))"),
         occupation = str_remove(occupation_soc_code, "\\([^\\)]+\\)"),
         across(employment_1:employment_per_1_000_jobs, as.numeric)) %>% 
  filter(str_detect(occupation, "Occupations$", negate = FALSE))

# Create Wage bins/labels ---- 
summary(bls$annual_median_wage_2)

bls_occ <- bls_occ %>% 
  filter(!is.na(annual_median_wage_2)) %>% 
  mutate(median_wage_bin = ntile(annual_median_wage_2, n = 5)) 

bls_occ <- bls_occ %>% 
  group_by(median_wage_bin) %>% 
  mutate(median_wage_bin_label = paste0(min(annual_median_wage_2), "-", max(annual_median_wage_2))) %>% 
  ungroup()

bls_occ %>% 
  count(median_wage_bin_label)

bls_catchall_occ <- bls_catchall_occ %>% 
  mutate(median_wage_bin = case_when(
    annual_median_wage_2 >= 25610 & annual_median_wage_2 < 37761 ~ 1,
    annual_median_wage_2 >= 37761 & annual_median_wage_2 < 48321 ~ 2,
    annual_median_wage_2 >= 48321 & annual_median_wage_2 < 60601 ~ 3,
    annual_median_wage_2 >= 60601 & annual_median_wage_2 < 86951 ~ 4,
    annual_median_wage_2 >= 86951 ~ 5
  ),
  median_wage_bin_label = case_when(
   median_wage_bin == 1 ~ "25610-37760",
   median_wage_bin == 2 ~ "37860-48320",
   median_wage_bin == 3 ~ "48340-60600",
   median_wage_bin == 4 ~ "60640-86950",
   median_wage_bin == 5 ~ "88050-238990"
  ))
  

# For appendix ---- 
# To generate tables of occupations and median wages by median wage bin
occupation_wages <- bls_occ %>% 
  arrange(annual_median_wage_2) %>% 
  select(occupation, annual_median_wage_2, employment_per_1_000_jobs, median_wage_bin, median_wage_bin_label) 

write_csv(occupation_wages, file = "data/occupations_by_wagebin.csv")


# Save ----
write_csv(bls_occ, file = "data/occupation_wages.csv")
write_csv(bls_catchall_occ, file = "data/occupation_wages_catchallcategories.csv")
