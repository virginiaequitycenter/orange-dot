# ALICE Data for Orange Dot 7 - 2026 Report
# Updated 7-30-26
# Elizabeth Mitchell

# Libraries
library(tidyverse)
library(readxl)
library(janitor)
library(here)

# Set WD
setwd(here("OrangeDot7_2026"))

# Orange Dot # Localities ----
county_codes <- c("003", "540", "065", "079", "109", "125", "029")

# ALICE Households & Threshold -----
# Get ALICE Data: https://www.unitedforalice.org/state-overview/Virginia

url <- "https://www.unitedforalice.org/Attachments/StateDataSheet/2026%20ALICE%20-%20Virginia%20Data%20Sheet.xlsx"

# Download file
download.file(url, destfile="data/rawdata/2026_ALICE_Virginia_Data_Sheet.xlsx", method="libcurl")

# Read data - County 
ALICE_sheet_county <- read_excel("data/rawdata/2026_ALICE_Virginia_Data_Sheet.xlsx", sheet = "County") %>% 
  clean_names()

# Filter
ALICE_county <- ALICE_sheet_county %>% 
  filter(str_detect(geo_id2, paste0("51", county_codes, collapse = '|'))) %>% 
  rename(GEOID = geo_id2,
         locality = geo_display_label)

# Household survival budgets ----
# Read in 
budget_workbook <- "data/household_budgets_2024.xlsx"

budget_df <- budget_workbook %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(~ read_excel(budget_workbook, sheet = .x)) |> 
  list_rbind(names_to = "County")

budget_sheet_single <- budget_df |> filter(County == "Albemarle") |> 
  select(-c("County", "Single Senior", "Two Seniors", "Two Adults Three School-Age"))

df <- data.frame(ID=budget_sheet_single[,1], Means=rowMeans(budget_sheet_single[,-1]))


# Family type ----
family_type_files <- list.files(path = "data/rawdata/family_type", pattern = "*.xlsx", full.names = TRUE)

county_names <- as.list(str_remove(family_type_files, "data/rawdata/family_type/families-with-children-")) 
county_names = str_to_title(substr(county_names,1,nchar(county_names)-5))

family_type_data <- map(family_type_files, ~read_excel(.x, sheet = 1))
names(family_type_data) <- county_names 

# bind into data frame
household_family <- bind_rows(family_type_data, .id = "County") |> 
  mutate(Measure = "Family", .after = "County")

# Household Age ----
age_files <- list.files(path = "data/rawdata/age", pattern = "*.xlsx", full.names = TRUE)

county_names <- as.list(str_remove(age_files, "data/rawdata/age/households-by-age-")) 
county_names = str_to_title(substr(county_names,1,nchar(county_names)-5))

age_data <- map(age_files, ~read_excel(.x, sheet = 1))
names(age_data) <- county_names 

# bind into data frame
household_age <- bind_rows(age_data, .id = "County") |> 
  mutate(Measure = "Age", .after = "County")

# Household Race/Ethnicity ----
race_files <- list.files(path = "data/rawdata/race", pattern = "*.xlsx", full.names = TRUE)

county_names <- as.list(str_remove(race_files, "data/rawdata/race/households-by-race-ethnicity-")) 
county_names = str_to_title(substr(county_names,1,nchar(county_names)-5))

race_data <- map(race_files, ~read_excel(.x, sheet = 1))
names(race_data) <- county_names 

# bind into data frame
household_race <- bind_rows(race_data, .id = "County") |>
  rename("Name" = "Year") |> 
  mutate(Year = 2024, .after = "County") |> 
  mutate(Measure = "Race", .after = "County")

# Combine all measure tables ----
household_measures <- rbind(household_family, household_age, household_race) |> 
  filter(Name != "Total Households")

# Create percents
household_measures <- household_measures |> 
  mutate(across(Above:Poverty, ~ round((.x/`# Households`)*100, 2), .names = "Percent {.col}"))

# Save
write_csv(household_measures, "data/alice_household_demographics_2024.csv")
