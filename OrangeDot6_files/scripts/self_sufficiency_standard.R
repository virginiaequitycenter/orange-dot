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
self_suff_sheet <- read_excel("data/tempdata/VA2021_all_families.xlsx", sheet = "By Family") %>% 
  clean_names()

self_suff_county <- self_suff_sheet %>% 
  filter(county %in% localities) %>% 
  filter(family_type == "a1i0p1s1t0")

mean_ssw <- mean(self_suff_county$annual_self_sufficiency_wage)

# Write CSV
write_csv(self_suff_county, "data/self_suff_county_2021.csv")
