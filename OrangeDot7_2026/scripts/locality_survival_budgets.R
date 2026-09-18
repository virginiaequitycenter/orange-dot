# Script for calculating average family survival budget and wage for localities

# Libraries
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(purrr)

# Set WD ----
setwd(here("OrangeDot7_2026"))

# Localities ----
county_codes <- c("003", "540", "065", "079", "109", "125", "029")

# Read in ALICE Budgets and create average for families ----

# Path to Excel file
budgets_workbook_path <- "data/household_budgets_2024.xlsx"

# Get workbook sheets and create data frame
budget_df <- budgets_workbook_path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(~ read_excel(budgets_workbook_path, sheet = .x)) |> 
  list_rbind(names_to = "County")

# Reduce to all families ---
# Remove three children due to high budget creates too much of an outlier
budget_allfam_df <- budget_df |> 
  select(-c("Single Adult", "Single Senior", "Two Adults Three School-Age"))

# Create Average Family Budget
budget_allfam_df <- budget_allfam_df |> 
  mutate(`Average Family` = round(rowMeans(budget_allfam_df[, 3:15], na.rm = TRUE), 2), .after = "Categories")

# Save for analysis
write_csv(budget_allfam_df, "data/budgets_all_families_2024.csv")

# Reduce to families w/ children only ---
budget_famwchild_df <- budget_df |> 
  select(-c("Single Adult", "Two Adults", "Single Senior", "Two Seniors", "Two Adults Three School-Age"))

# Create Average Family Budget
budget_famwchild_df <- budget_famwchild_df |> 
  mutate(`Average Budget With Children` = round(rowMeans(budget_famwchild_df[, 3:13], na.rm = TRUE), 2), .after = "Categories")

# Save for analysis
write_csv(budget_famwchild_df, "data/budgets_families_wchild_2024.csv")

# Reduce to families w no children. ----
budget_fam_nochild_df <- budget_df |> 
  select(c("County", "Categories", "Two Adults", "Two Seniors"))

# Create Average Budget - family no children
budget_fam_nochild_df <- budget_fam_nochild_df |> 
  mutate(`Average Budget No Children` = round(rowMeans(budget_fam_nochild_df[, 3:4], na.rm = TRUE), 2), .after = "Categories")

# Save for analysis
write_csv(budget_fam_nochild_df, "data/budgets_no_children_2024.csv")

# Reduce to 65+ households ----
budget_hh_65over_df <- budget_df |> 
  select(c("County", "Categories", "Single Senior", "Two Seniors"))

# Create Average Budget - family no children
budget_hh_65over_df <- budget_hh_65over_df |> 
  mutate(`Average Budget 65+ Household` = round(rowMeans(budget_hh_65over_df[, 3:4], na.rm = TRUE), 2), .after = "Categories")

# Save for analysis
write_csv(budget_hh_65over_df, "data/budgets_65plus_households_2024.csv")

# # Get names of all sheets and set them as vector names
# sheet_names <- excel_sheets(budgets_workbook_path)
# names(sheet_names) <- sheet_names
# 
# # Read each sheet into a named list of data frames
# budget_list <- map(sheet_names, ~ read_excel(budgets_workbook_path, sheet = .x))
# 
# # Apply mutate to every data frame in the list
# budget_list_avg <- map(budget_list, ~mutate(.x, `Average Family Budget` = existing_col * 2))
# 
# # Access individual data frames using the sheet names:
# # data_list$Sheet1
# # data_list[["Sheet2"]]
# 
# # Extract all list elements directly into independent global data frames
# # list2env(data_list, envir = .GlobalEnv)