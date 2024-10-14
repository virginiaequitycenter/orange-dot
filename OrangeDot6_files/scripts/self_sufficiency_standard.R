# UW Self-Sufficiency Standard
# Source: https://selfsufficiencystandard.org/Virginia/

# Load packages ----
library(here)
library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)
library(openxlsx)

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
            child_care_tax_credit = round(mean(child_care_tax_credit), 2),
            child_tax_credit = round(mean(child_tax_credit), 2),
            hourly_self_sufficiency_wage = round(mean(hourly_self_sufficiency_wage), 2),
            monthly_self_sufficiency_wage = round(mean(monthly_self_sufficiency_wage), 2),
            annual_self_sufficiency_wage = round(mean(annual_self_sufficiency_wage), 2),
            emergency_savings = round(mean(emergency_savings),2)) %>% 
  rename_with(~paste0(. , "_mean"), 4:17)
  


mean_ssw <- mean(self_suff_mean$annual_self_sufficiency_wage_mean)

# Join tables
self_suff_county <- self_suff_a1p1s1 %>% 
  left_join(self_suff_mean, by = join_by(state, year, county)) %>% 
  select(state, year, county, annual_self_sufficiency_wage_a1p1s1, annual_self_sufficiency_wage_mean,
         housing_costs_a1p1s1, housing_costs_mean, child_care_costs_a1p1s1, child_care_costs_mean,
         food_costs_a1p1s1, food_costs_mean, transportation_costs_a1p1s1, transportation_costs_mean,
         health_care_costs_a1p1s1, health_care_costs_mean, miscellaneous_costs_a1p1s1, miscellaneous_costs_mean,
         taxes_a1p1s1, taxes_mean, earned_income_tax_credit_a1p1s1, earned_income_tax_credit_mean,
         child_care_tax_credit_a1p1s1, child_care_tax_credit_mean, child_tax_credit_a1p1s1, child_tax_credit_mean,
         hourly_self_sufficiency_wage_a1p1s1, hourly_self_sufficiency_wage_mean,
         monthly_self_sufficiency_wage_a1p1s1, monthly_self_sufficiency_wage_mean,
         emergency_savings_a1p1s1, emergency_savings_mean
         )



# Write CSV
# write_csv(self_suff_county, "data/self_suff_county_2021.csv")

# Appendix tables ----

# Filter by common family types
family_types <- c("a1i0p0s0t1", "a1i1p1s0t0", "a1i0p1s1t0", "a1i0p0s2t0", "a2i0p0s0t0", "a2i1p0s0t0", "a2i0p1s0t0", "a2i1p1s0t0",
                  "a2i0p1s1t0", "a2i0p0s2t0", "a2i0p0s1t2")

self_suff_byfamily_append <- self_suff_byfamily %>% 
  filter(county %in% localities) %>% 
  filter(family_type  %in% family_types) %>% 
  select(-c(2:9)) %>% 
  # mutate(sum = (housing_costs + child_care_costs + food_costs + transportation_costs + health_care_costs + miscellaneous_costs + taxes + earned_income_tax_credit + child_care_tax_credit + child_tax_credit))
  pivot_longer(cols = 3:16) %>% 
  pivot_wider(names_from = family_type, values_from = value) %>% 
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) %>% 
  rename("1 adult, 1 teen" = a1i0p0s0t1,       
         "1 adult, 1 infant, 1 preschooler" = a1i1p1s0t0,
         "1 adult, 1 preschooler, 1 school-age" = a1i0p1s1t0, 
         "1 adult, 2 school-age" = a1i0p0s2t0, 
         "2 adults, no children" = a2i0p0s0t0, 
         "2 adults, 1 infant" = a2i1p0s0t0,
         "2 adults, 1 preschooler" = a2i0p1s0t0,
         "2 adults, 1 infant, 1 preschooler" = a2i1p1s0t0, 
         "2 adults, 1 preschooler, 1 school-age" = a2i0p1s1t0,
         "2 adults, 2 school-age" = a2i0p0s2t0,
         "2 adults, 1 school-age, 2 teens" = a2i0p0s1t2)

# Write Excel
# write_csv(self_suff_byfamily_append, "data/self_suff_byfamily_append.csv")
# write.xlsx(self_suff_byfamily_append, file = "self_suff_byfamily_append.xlsx")

wb <- createWorkbook()

options("openxlsx.numFmt" = "#,#0.00")
options("openxlsx.borderColour" = "black")
modifyBaseFont(wb, fontSize = 12)
headSty <- createStyle(textDecoration=c("bold"), halign="center", border = "TopRight", valign = "top", wrapText = TRUE)

addWorksheet(wb, sheetName = "Albemarle County")
addWorksheet(wb, sheetName = "Buckingham County")
addWorksheet(wb, sheetName = "Charlottesville City")
addWorksheet(wb, sheetName = "Fluvanna County")
addWorksheet(wb, sheetName = "Greene County")
addWorksheet(wb, sheetName = "Louisa County")
addWorksheet(wb, sheetName = "Nelson County")

# Albemarle
writeData(wb, sheet = 1, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Albemarle County") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 1, x = "Albemarle County: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 1, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 1, cols = "A", widths = 28)
setColWidths(wb, sheet = 1, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 1, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Buckingham
writeData(wb, sheet = 2, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Buckingham County") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 2, x = "Buckingham County: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 2, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 2, cols = "A", widths = 28)
setColWidths(wb, sheet = 2, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 2, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Charlottesville
writeData(wb, sheet = 3, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Charlottesville city") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 3, x = "Charlottesville City: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 3, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 3, cols = "A", widths = 28)
setColWidths(wb, sheet = 3, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 3, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Fluvanna
writeData(wb, sheet = 4, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Fluvanna County") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 4, x = "Fluvanna County: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 4, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 4, cols = "A", widths = 28)
setColWidths(wb, sheet = 4, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 4, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Greene
writeData(wb, sheet = 5, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Greene County") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 5, x = "Greene County: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 5, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 5, cols = "A", widths = 28)
setColWidths(wb, sheet = 5, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 5, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Louisa
writeData(wb, sheet = 6, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Louisa County") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 6, x = "Louisa County: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 6, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 6, cols = "A", widths = 28)
setColWidths(wb, sheet = 6, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 6, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Nelson
writeData(wb, sheet = 7, startRow = 2, 
          x = self_suff_byfamily_append %>% filter(county == "Nelson County") %>% select(-county), 
          borders = "column", headerStyle = headSty)
writeData(wb, sheet = 7, x = "Nelson County: Expenses and Self-Sufficiency Wages by Family Composition", startCol = 1, startRow = 1, 
          headerStyle = headSty)
writeData(wb, sheet = 7, x = "Data Source: The Self-Sufficiency Standard for Virginia, 2021; The Center for Women’s Welfare, University of Washington", startCol = 1, startRow = 17, 
          headerStyle = headSty)

setColWidths(wb, sheet = 7, cols = "A", widths = 28)
setColWidths(wb, sheet = 7, cols = 2:13, widths = 14)
setRowHeights(wb, sheet = 1, rows = 1, heights = 30)
setRowHeights(wb, sheet = 1, rows = 2, heights = 68)
pageSetup(wb, sheet = 7, orientation = "landscape", fitToWidth = TRUE, paperSize = 1)

# Save workbook
saveWorkbook(wb, "SelfSufficienyTablesByCounty.xlsx", overwrite = TRUE)
