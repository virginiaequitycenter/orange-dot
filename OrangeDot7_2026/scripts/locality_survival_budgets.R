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

# Create Workbook for Report ----
library(openxlsx)

# Initialize the workbook container
wb <- createWorkbook()

# Get the names of all sheets in the workbook
sheet_names <- excel_sheets(budgets_workbook_path)

# Read each sheet and apply select() to all of them
selected_sheets_list <- sheet_names %>% 
  set_names() %>% 
  map(~ read_excel(budgets_workbook_path, sheet = .x) %>% 
        select("Categories", "Single Adult", "One Adult One Infant One Preschooler", "One Adult One Preschooler", 
               "One Adult One Preschooler One School-Age", "One Adult One School-Age", "One Adult Two School-Age", 
               "Two Adults", "Two Adults One Infant", "Two Adults One Infant One Preschooler", "Two Adults One Preschooler", 
               "Two Adults One Preschooler One School-Age", "Two Adults One School-Age", "Two Adults Two School-Age", 
               "Two Adults Three School-Age", "Single Senior", "Two Seniors"))

# Define custom styles
header_style <- createStyle(
  fontName = "Arial",
  textDecoration = "bold",
  # fontColour = "#FFFFFF",
  # fgFill = "#1F497D",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

title_style <- createStyle(
  fontName = "Arial",
  fontSize = 14,
  textDecoration = "bold"
)

currency_style <- createStyle(numFmt = "$#,##0.00")

# Loop through the names of the list to add worksheets and write data
lapply(names(selected_sheets_list), function(sheet_name) {
  addWorksheet(wb, sheetName = sheet_name)
  # Write title text at row 1, column 1
  writeData(wb, sheet = sheet_name, x = paste0(sheet_name, ": Survival Budgets by Family Composition"), startRow = 1, startCol = 1)
  addStyle(wb, sheet = sheet_name, style = title_style, rows = 1, cols = 1)
  modifyBaseFont(wb, fontName = "Arial", fontSize = 12) # Set global font
  writeData(wb, sheet = sheet_name, x = selected_sheets_list[[sheet_name]],
            startRow = 2, 
            startCol = 1
            # headerStyle = header_style
            )
  # addStyle(wb, sheet = sheet_name, style = header_style, rows = 3, cols = 1:17, gridExpand = TRUE)
  addStyle(wb, sheet = sheet_name, style = currency_style, cols = 2:17, rows = 3:13, gridExpand = TRUE)

# Auto-adjust column widths to prevent text truncation ("###" errors)
setColWidths(wb, sheet = sheet_name, cols = 1:17, widths = "auto")

})

# Save the compiled workbook
saveWorkbook(wb, "my_workbook.xlsx", overwrite = TRUE)
