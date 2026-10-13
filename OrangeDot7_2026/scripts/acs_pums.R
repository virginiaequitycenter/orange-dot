# American Community Survey Public Use Microdata for the Charlottesville Region 
# https://www.census.gov/programs-surveys/acs/microdata/access.html
# 2024 5-year release
# Downloaded on 2026-09-17
#
# FTP access: https://www2.census.gov/programs-surveys/acs/data/pums/2024/5-Year/
# Virginia person-level records: https://www2.census.gov/programs-surveys/acs/data/pums/2024/5-Year/csv_pva.zip
# Virginia housing-level records: https://www2.census.gov/programs-surveys/acs/data/pums/2024/5-Year/csv_hva.zip
# Documentation: https://www.census.gov/programs-surveys/acs/microdata/documentation.html

# Libraries ----
library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(stringi)

# Set WD ----
setwd(here("OrangeDot7_2026"))

# Get ACS 5-yr PUMS Data ----
# Download ----
pva_link <- "https://www2.census.gov/programs-surveys/acs/data/pums/2024/5-Year/csv_pva.zip"
hva_link <- "https://www2.census.gov/programs-surveys/acs/data/pums/2024/5-Year/csv_hva.zip"

download.file(pva_link, "data/rawdata/microdata/csv_pva.zip")
download.file(hva_link, "data/rawdata/microdata/csv_hva.zip")

unzip("data/rawdata/microdata/csv_pva.zip", exdir = "data/rawdata/microdata/")
unzip("data/rawdata/microdata/csv_hva.zip", exdir = "data/rawdata/microdata/")

pva <- read_csv("data/rawdata/microdata/psam_p51.csv")
hva <- read_csv("data/rawdata/microdata/psam_h51.csv")

# Filter to cville region pumas ----
# these contain the six localities of the
# Thomas Jefferson Planning District:
# Charlottesville, Albemarle, Louisa, Nelson, Fluvanna, Greene
# PUMA 10901 : 2020 CENSUS - PUMA REFERENCE MAP: Thomas Jefferson Planning District Commission (South & East)
# PUMA 54001 : 2020 CENSUS - PUMA REFERENCE MAP: Thomas Jefferson Planning District Commission (North)
# Buckingham is in puma 51-08300 (Soutside Planning District Commision & Commonwealth Regional Council)
#   so is excluded from this analysis
pcvl <- pva %>% 
  filter(PUMA %in% c("54001", "10901"))

hcvl <- hva %>% 
  filter(PUMA %in% c("54001", "10901"))

# save ----
write_csv(pcvl, "data/rawdata/microdata/cvl_acspums_person.csv")
write_csv(hcvl, "data/rawdata/microdata/cvl_acspums_household.csv")

# Data prep Budgets ----
# Budgets, Path to Excel file
budgets_workbook_path <- "data/household_budgets_2024.xlsx"

# Get workbook sheets and create data frame
budget_df <- budgets_workbook_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(~ read_excel(budgets_workbook_path, sheet = .x)) |> 
  list_rbind(names_to = "County")

# Filter by annual total and families only
budget_allfam <- budget_df |> 
  filter(Categories == "ANNUAL TOTAL") |> 
  select(-c(Categories, `Two Seniors`, `Single Adult`, `Single Senior`)) |> 
  clean_names()

# Prep budget for analysis
budget_allfam <- budget_allfam |> 
  pivot_longer(c(2:14)) |> 
  rename(family_type = name,
         budget = value) |> 
  pivot_wider(names_from = county, values_from = budget)
  
# Get median budget for region
budget_region <- budget_allfam |> 
  rowwise() %>% 
  mutate(median_budget = median(c_across(where(is.numeric)))) %>% 
  select(family_type, median_budget)

# Mutate family types
budget_region_rename <- budget_region |> 
  mutate(family_type = str_remove_all(family_type, "_")) |> 
  mutate(family_type = str_replace_all(family_type, "one", "1"),
         family_type = str_replace_all(family_type, "two", "2"),
         family_type = str_replace_all(family_type, "three", "3"),
         family_type = str_replace(family_type, "adults|adult", "a"),
         family_type = str_replace(family_type, "infant", "i"),
         family_type = str_replace(family_type, "preschooler", "p"),
         family_type = str_replace(family_type, "schoolage", "s")) |> 
  mutate(family_type = case_when(!str_detect(family_type, "i") ~ stri_sub_replace(family_type, from = 3, length = 0, replacement = "0i"),
                                 .default = family_type),
         family_type = case_when(!str_detect(family_type, "p") ~ stri_sub_replace(family_type, from = 5, length = 0, replacement = "0p"),
                                 .default = family_type),
         family_type = case_when(!str_detect(family_type, "s") ~ stri_sub_replace(family_type, from = 7, length = 0, replacement = "0s"),
                                 .default = family_type))

# Generate regional budget estimates for PUMS ----
budget_est_df <- budget_region_rename |> 
  mutate(adults = as.integer(str_sub(family_type, 1,1)),
         infants = as.integer(str_sub(family_type, 3,3)),
         preschool = as.integer(str_sub(family_type, 5,5)),
         schoolage = as.integer(str_sub(family_type, 7,7)))

est <- lm(median_budget ~ adults + infants + preschool + schoolage,
          data = budget_est_df)

summary(est)

budget_est_df$predicted <- predict(est)

ggplot(budget_est_df, aes(x = median_budget, y = predicted)) +
  geom_point() +
  geom_abline()

# generate estimates based on combined budgets
# could probably reduce this -- but it costs nothing
# to have all of these combinations
adults <- c(1,2,3)
infants <- c(0,1,2,3)
preschool <- c(0,1,2,3)
schoolage <- c(0,1,2,3,4)

budget_estimates <- expand.grid(
  adults = adults,
  infants = infants,
  preschool = preschool,
  schoolage = schoolage
)

budget_estimates$budget <- predict(est, newdata = budget_estimates)

# create family type 
# replace combinations that were directly downloaded
# and save for use
budget_estimates <- budget_estimates |> 
  mutate(family_type = paste0(adults, "a", infants, "i", preschool, "p", schoolage, "s")) |> 
  left_join(budget_region_rename, by = "family_type") |> 
  mutate(budget = round(budget, 0),
         budget = if_else(!is.na(median_budget), median_budget, budget)) |> 
  select(family_type, budget)

write_csv(budget_estimates, "data/regional_budget_estimates.csv")

# Data Prep PUMS ----
# Read in data ---- 
household <- read_csv("data/rawdata/microdata/cvl_acspums_household.csv") %>% clean_names()
person <- read_csv("data/rawdata/microdata/cvl_acspums_person.csv") %>% clean_names()

## Housing records ----
# # remove records that aren't housing units (institutional and group quarters)
# # remove records that are vacant housing units
household_reduced <- household %>% 
  filter(typehugq == 1, is.na(vacs)) 

## Person records ----
# remove person records that aren't related to household/reference person
# relationship is roommate (34), other nonrelative (36)
# theoretically, this should reduce the number of adults in household unit
person_reduced <- person %>% 
  filter(!(relshipp %in% c(34, 36))) 
# what does NA on relshipp mean?

# create family composition variable 
# to match self-sufficiency variable

# assign age group
person_reduced <- person_reduced %>% 
  mutate(age_type = case_when(agep <= 2 ~ 'i',
                              agep > 2 & agep <= 5 ~ 'p',
                              agep > 5 & agep <= 18 ~ 's',
                              agep >= 18 ~ 'a'
  ))  

# Create binary variables for each type 
person_reduced <- person_reduced %>% 
  mutate(i = case_when(age_type == 'i' ~ 1, TRUE ~ 0),
         p = case_when(age_type == 'p' ~ 1, TRUE ~ 0),
         a = case_when(age_type == 'a' ~ 1, TRUE ~ 0),
         s = case_when(age_type == 's' ~ 1, TRUE ~ 0)
  ) 

# Collapse by serial number
collapsed_person_reduced <- person_reduced %>%
  group_by(serialno) %>% 
  summarize(a = sum(a),
            i = sum(i),
            p = sum(p),
            s = sum(s)
  ) %>% 
  mutate(c = i+p+s, # for cases with more than 6 children
         # create family type conditional on # adults and # children
         family_type = case_when(
           a < 4 & c < 7 ~ paste0(a, "a", i, "i", p, "p", s, "s"),
           a >= 4 ~ paste0(a, "a", c, "c"),
           c >= 7 ~ paste0(a, "a", c, "c"),
           a == 11 ~ paste0("a10", c, "c")
         )) %>% 
  select(serialno, family_type)

## Merge family type with household records ----
household_type <- household_reduced %>% 
  left_join(collapsed_person_reduced, by = "serialno")

## Merge budgets and pums data ---- 
household_type <- household_type %>% 
  left_join(budget_estimates, by = "family_type")

## Identify families/households income below budget ----
household_type_inc <- household_type %>%
  drop_na(budget) |> 
  mutate(
    income = ifelse(!is.na(fincp), fincp, hincp),
    below_budget_inc = case_when(
      income >= budget ~ "No",
      income < budget ~ "Yes",
      TRUE ~ NA_character_), 
    below_budget_finc = case_when(
      fincp >= budget ~ "No", 
      fincp < budget ~ "Yes",
      TRUE ~ NA_character_),
    below_35_finc = case_when(
      fincp >= 35000 ~ "No",
      fincp < 35000 ~ "Yes",
      TRUE ~ NA_character_)
  )

count(household_type_inc, below_budget_inc)
count(household_type_inc, below_budget_finc)
count(household_type_inc, below_35_finc)

## Join below_budget to person records ----
person_type <- person_reduced %>% 
  left_join(household_type_inc %>% 
              select(serialno, income, below_budget_inc, below_budget_finc, below_35_finc),
            by = "serialno")

count(person_type, below_budget_inc)
count(person_type, below_budget_finc)
count(person_type, below_35_finc)

# Save data ----
write_csv(household_type_inc, file = "data/pums_household_processed_updated.csv")
write_csv(person_type, file = "data/pums_person_processed_updated.csv")

