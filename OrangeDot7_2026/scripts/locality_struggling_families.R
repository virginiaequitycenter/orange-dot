# Script for calculating number of struggling families by: 
# All families, Families with Children, w/o children, All families by Race/Ethnicity, by county
# All families by census tract

# Libraries
library(here)
library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(tidycensus)

# Set WD ----
setwd(here("OrangeDot7_2026"))

# Set values ----

# Localities
county_codes <- c("003", "540", "065", "079", "109", "125", "029")

# Year for ACS data (single year)
year <- 2024

# Get avg family budget data ----
# All families
budget_allfam_df <- read_csv("data/budgets_all_families_2024.csv")

# Filter by average family and annual total
budget_allfam_avg <- budget_allfam_df |> 
  filter(Categories == "ANNUAL TOTAL") |> 
  select(County, `Average Family`) |> 
  clean_names() |> 
  rename(avg_allfamily_budget = average_family)

# W/ Children
budget_famwchild_df <- read_csv("data/budgets_families_wchild_2024.csv")

# Filter by average family and annual total
budget_famwchild_avg <- budget_famwchild_df |> 
  filter(Categories == "ANNUAL TOTAL") |> 
  select(County, `Average Budget With Children`) |> 
  clean_names() |> 
  rename(avg_wchild_budget = average_budget_with_children)

# W/O Children
budget_fam_nochild_df <- read_csv("data/budgets_no_children_2024.csv")

# Filter by average family and annual total
budget_nochild_avg <- budget_fam_nochild_df |> 
  filter(Categories == "ANNUAL TOTAL") |> 
  select(County, `Average Budget No Children`) |> 
  clean_names() |> 
  rename(avg_nochild_budget = average_budget_no_children)

# 65+ Households
budget_hh_65over_df <- read_csv("data/budgets_65plus_households_2024.csv")

# Filter by average family and annual total
budget_65plus_avg <- budget_hh_65over_df |> 
  filter(Categories == "ANNUAL TOTAL") |> 
  select(County, `Average Budget 65+ Household`) |> 
  clean_names() |> 
  rename(avg_65plus_budget = average_budget_65_household)

# ACS Data----

acs5_vars <- load_variables(year, "acs5", cache = TRUE)

# Family Income w/ and w/o Children under 18, All families, County level ----
# Source: ACS Table B19131: Family Type by Presence of Own Children Under 18 Years by Family Income
# Not available in racial iterations

acs_B19131_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  table = "B19131",
  summary_var = "B19131_001",
  year = year, 
  survey = "acs5")

# get labels
acs_B19131_labels <- acs5_vars |> 
  filter(startsWith(name, "B19131")) |> 
  rename(variable = name)

# join labels with acs data
famwchild_inc_county <- acs_B19131_county |> 
  left_join(acs_B19131_labels) |> 
  mutate(year = year,
         total_families = summary_est,
         label = str_remove(label, "Estimate!!Total:"),
         label = str_remove(label, "!!Other family:")) |> 
  separate(label, into=c("label_extra", "family_type", "child_in_home", "income_band"), sep="!!", remove=FALSE) |> 
  mutate(across(family_type:child_in_home, ~ str_remove(.x, ":"))) |> 
  rename("locality" = "NAME") |> 
  mutate(income_band = case_when(variable == "B19131_001" ~ "Total Families",
                                 variable == "B19131_002" ~ "Total Married-couple Families",
                                 variable == "B19131_003" ~ "Total Married-couple Families with own children",
                                 variable == "B19131_020" ~ "Total Married-couple Families with no children",
                                 variable == "B19131_037" ~ "Total Other Families",
                                 variable == "B19131_038" ~ "Total Male householder",
                                 variable == "B19131_039" ~ "Total Male householder with own children",
                                 variable == "B19131_056" ~ "Total Male householder with no children",
                                 variable == "B19131_073" ~ "Total Female householder",
                                 variable == "B19131_074" ~ "Total Female householder with own children",
                                 variable == "B19131_091" ~ "Total Female householder with no children",
                                 .default = income_band)) |> 
  select(GEOID, locality, year, variable, family_type, child_in_home, income_band, estimate, moe, total_families)

# Combine family types
famwchild_inc_county <- famwchild_inc_county |> 
  group_by(GEOID, locality, year, child_in_home, income_band) |> 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families))

# Get total families with own children
famwchild_total <- famwchild_inc_county |> 
  filter(str_detect(income_band, "with own children")) |> 
  group_by(GEOID, locality, year, child_in_home) |> 
  summarise(estimate = sum(estimate)) |> 
  rename("total_families_by_group" = "estimate")

# Get total families with no children
famnochild_total <- famwchild_inc_county |> 
  filter(str_detect(income_band, "with no children")) |> 
  group_by(GEOID, locality, year, child_in_home) |> 
  summarise(estimate = sum(estimate)) |> 
  rename("total_families_by_group" = "estimate")

fam_totals <- rbind(famwchild_total, famnochild_total)

# Join total with table
famwchild_inc_county <- famwchild_inc_county |> 
  left_join(fam_totals)

# Clean up income band table for saving

fam_type_inc_band <- famwchild_inc_county |> 
  filter(!startsWith(income_band,"Total")) |> 
  mutate(inc_band = str_remove_all(income_band, ","),
         inc_band = str_remove_all(inc_band, "\\$"),
         inc_group = case_when(inc_band=="Less than 10000" ~ "below_15k",
                               inc_band=="10000 to 14999" ~ "below_15k",
                               inc_band=="15000 to 19999" ~ "below_30k",
                               inc_band=="20000 to 24999" ~ "below_30k",
                               inc_band=="25000 to 29999" ~ "below_30k",
                               inc_band=="30000 to 34999" ~ "below_45k",
                               inc_band=="35000 to 39999" ~ "below_45k",
                               inc_band=="40000 to 44999" ~ "below_45k",
                               inc_band=="45000 to 49999" ~ "below_60k",
                               inc_band=="50000 to 59999" ~ "below_60k",
                               inc_band=="60000 to 74999" ~ "below_75k",
                               inc_band=="75000 to 99999" ~ "below_100k",
                               inc_band=="100000 to 124999" ~ "above_100k",
                               inc_band=="125000 to 149999" ~ "above_100k",
                               inc_band=="150000 to 199999" ~ "above_100k",
                               inc_band=="200000 or more" ~ "above_100k"))

# Save
write_csv(fam_type_inc_band, "data/income_bands_fam_type_county_2024.csv")


# Families with Children ----
# generate proportion 
famwchild_county_prop <- famwchild_inc_county %>%
  filter(child_in_home == "With own children of the householder under 18 years") |>
  mutate(county = str_remove(locality, " County, Virginia| city, Virginia")) |>
  left_join(budget_famwchild_avg) |>
  filter(!startsWith(income_band,"Total")) |> 
  mutate(income_band = str_remove_all(income_band, ","),
         income_band = str_remove_all(income_band, "\\$")) |> 
  separate(income_band, into=c("inc_band_low","sep", "inc_band_high"), sep=" ", remove=FALSE) |>
  select(-sep) |>
  mutate(across(inc_band_low:inc_band_high, as.numeric)) |> 
  mutate(prop_test = case_when(avg_wchild_budget >= inc_band_low & avg_wchild_budget <= inc_band_high ~ TRUE,
                               .default = FALSE)) |> 
  mutate(prop = case_when(prop_test == TRUE ~ (avg_wchild_budget - inc_band_low) / (inc_band_high - inc_band_low),
                          .default = 1),
         est_prop = estimate * prop)

view(famwchild_county_prop |> filter(prop_test==TRUE) |> select(GEOID, county, income_band))

# Calculate totals and percent for budgets in income band 75000 to 99999 / below_100k
below_100k_geo <- c("51003", "51065", "51079", "51125", "51540")

famwchild_county_prop100k <- famwchild_county_prop %>% 
  filter(GEOID %in% below_100k_geo) %>% 
  mutate(group = case_when(inc_band_high <= 99999 ~ "inc_under_budget",
                           inc_band_high > 99999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, avg_wchild_budget, child_in_home, group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families_by_group = first(total_families_by_group),
            total_families = first(total_families)) %>% 
  ungroup() 

famwchild_county_prop100k_adj <- famwchild_county_prop100k |> 
  select(GEOID, group, est_prop, total_families_by_group) |> 
  pivot_wider(names_from = group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families_by_group - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "group", values_to = "est_prop_adj") %>%
  left_join(famwchild_county_prop100k) |> 
  mutate(percent = round((est_prop_adj/total_families_by_group)*100, 2))

# Calculate totals and percent for budgets in income band 60000 to 74999 / below_75k

below_75k_geo <- c("51029", "51109")

famwchild_county_prop75k <- famwchild_county_prop %>% 
  filter(GEOID %in% below_75k_geo) %>% 
  mutate(group = case_when(inc_band_high <= 74999 ~ "inc_under_budget",
                           inc_band_high > 74999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, avg_wchild_budget, child_in_home, group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families_by_group = first(total_families_by_group),
            total_families = first(total_families)) %>% 
  ungroup() 

famwchild_county_prop75k_adj <- famwchild_county_prop75k |> 
  select(GEOID, group, est_prop, total_families_by_group) |> 
  pivot_wider(names_from = group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families_by_group - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "group", values_to = "est_prop_adj") %>%
  left_join(famwchild_county_prop75k) |> 
  mutate(percent = round((est_prop_adj/total_families_by_group)*100, 2))

# bind tables
famwchild_county_summary <- rbind(famwchild_county_prop100k_adj, famwchild_county_prop75k_adj) |> 
  rename(avg_budget_by_group = avg_wchild_budget)

# Families with no Children ----
# generate proportion 
famnochild_county_prop <- famwchild_inc_county %>%
  filter(child_in_home == "No own children of the householder under 18 years") |>
  mutate(county = str_remove(locality, " County, Virginia| city, Virginia")) |>
  left_join(budget_nochild_avg) |>
  filter(!startsWith(income_band,"Total")) |> 
  mutate(income_band = str_remove_all(income_band, ","),
         income_band = str_remove_all(income_band, "\\$")) |> 
  separate(income_band, into=c("inc_band_low","sep", "inc_band_high"), sep=" ", remove=FALSE) |>
  select(-sep) |>
  mutate(across(inc_band_low:inc_band_high, as.numeric)) |> 
  mutate(prop_test = case_when(avg_nochild_budget >= inc_band_low & avg_nochild_budget <= inc_band_high ~ TRUE,
                               .default = FALSE)) |> 
  mutate(prop = case_when(prop_test == TRUE ~ (avg_nochild_budget - inc_band_low) / (inc_band_high - inc_band_low),
                          .default = 1),
         est_prop = estimate * prop)

view(famnochild_county_prop |> filter(prop_test==TRUE) |> select(GEOID, county, income_band))

# Calculate totals and percent for budgets in income band 60000 to 74999 / below 75k

below_75k_geo <- c("51079", "51540")

famnochild_county_prop75k <- famnochild_county_prop %>% 
  filter(GEOID %in% below_75k_geo) %>% 
  mutate(group = case_when(inc_band_high <= 74999 ~ "inc_under_budget",
                           inc_band_high > 74999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, avg_nochild_budget, child_in_home, group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families_by_group = first(total_families_by_group),
            total_families = first(total_families)) %>% 
  ungroup() 

famnochild_county_prop75k_adj <- famnochild_county_prop75k |> 
  select(GEOID, group, est_prop, total_families_by_group) |> 
  pivot_wider(names_from = group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families_by_group - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "group", values_to = "est_prop_adj") %>%
  left_join(famnochild_county_prop75k) |> 
  mutate(percent = round((est_prop_adj/total_families_by_group)*100, 2))

# Calculate totals and percent for budgets in income band 50000 to 59999 / below 60k

below_60k_geo <- c("51003", "51065", "51109", "51125")

famnochild_county_prop60k <- famnochild_county_prop %>% 
  filter(GEOID %in% below_60k_geo) %>% 
  mutate(group = case_when(inc_band_high <= 59999 ~ "inc_under_budget",
                           inc_band_high > 59999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, avg_nochild_budget, child_in_home, group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families_by_group = first(total_families_by_group),
            total_families = first(total_families)) %>% 
  ungroup()

famnochild_county_prop60k_adj <- famnochild_county_prop60k |> 
  select(GEOID, group, est_prop, total_families_by_group) |> 
  pivot_wider(names_from = group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families_by_group - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "group", values_to = "est_prop_adj") %>%
  left_join(famnochild_county_prop60k) |> 
  mutate(percent = round((est_prop_adj/total_families_by_group)*100, 2))

# Calculate totals and percent for budgets in income band 45000 to 49999 / below_50k

below_50k_geo <- c("51029")

famnochild_county_prop50k <- famnochild_county_prop %>% 
  filter(GEOID %in% below_50k_geo) %>% 
  mutate(group = case_when(inc_band_high <= 49999 ~ "inc_under_budget",
                           inc_band_high > 49999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, avg_nochild_budget, child_in_home, group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families_by_group = first(total_families_by_group),
            total_families = first(total_families)) %>% 
  ungroup() 

famnochild_county_prop50k_adj <- famnochild_county_prop50k |> 
  select(GEOID, group, est_prop, total_families_by_group) |> 
  pivot_wider(names_from = group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families_by_group - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "group", values_to = "est_prop_adj") %>%
  left_join(famnochild_county_prop50k) |> 
  mutate(percent = round((est_prop_adj/total_families_by_group)*100, 2))

# bind no child tables

famnochild_county_summary <- rbind(famnochild_county_prop75k_adj, famnochild_county_prop60k_adj, famnochild_county_prop50k_adj) |> 
  rename(avg_budget_by_group = avg_nochild_budget)
  
# Combine all family type tables ----

family_county_summary <- rbind(famwchild_county_summary, famnochild_county_summary) |> 
  select(GEOID, locality, year, total_families, child_in_home, avg_budget_by_group, inc_v_budget=group, estimate, moe, est_prop, est_prop_adj, total_families_by_group, percent)

# save
write_csv(family_county_summary, "data/family_type_county_2024.csv")
  
# Family Income, All families, County level ----
# Source: ACS Table B19101
acs_B19101_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  table = "B19101",
  summary_var = "B19101_001",
  year = year, 
  survey = "acs5")

faminc_county <- acs_B19101_county %>% 
  mutate(group = "All Families") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, group, estimate, moe, total_families)

# Family Income, White Alone, Not Hispanic ----
# Source: ACS Table B19101H
acs_B19101H_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  table = "B19101H",
  summary_var = "B19101H_001",
  year = year, 
  survey = "acs5")

faminc_white <- acs_B19101H_county %>% 
  mutate(group = "White, Not Hispanic") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, group, estimate, moe, total_families)

# Family Income, Black Alone ----
# Source: ACS Table B19101B
acs_B19101B_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  table = "B19101B",
  summary_var = "B19101B_001",
  year = year, 
  survey = "acs5")

faminc_black <- acs_B19101B_county %>% 
  mutate(group = "Black") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, group, estimate, moe, total_families)

# Family Income, Hispanic ----
# Source: ACS Table B19101I
acs_B19101I_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  table = "B19101I",
  summary_var = "B19101I_001",
  year = year, 
  survey = "acs5")

faminc_hispanic <- acs_B19101I_county %>% 
  mutate(group = "Hispanic") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, group, estimate, moe, total_families)

# Join tables
faminc_race_county <- rbind(faminc_county, faminc_white, faminc_black, faminc_hispanic)

# Wrangle data
faminc_race_county <- faminc_race_county %>% 
  mutate(year = year,
         inc_band = case_when(str_detect(variable, "_001") ~ "Total Families",
                              str_detect(variable, "_002") ~ "Less than 10000",
                              str_detect(variable, "_003") ~ "10000 to 14999",
                              str_detect(variable, "_004") ~ "15000 to 19999",
                              str_detect(variable, "_005") ~ "20000 to 24999",
                              str_detect(variable, "_006") ~ "25000 to 29999",
                              str_detect(variable, "_007") ~ "30000 to 34999",
                              str_detect(variable, "_008") ~ "35000 to 39999",
                              str_detect(variable, "_009") ~ "40000 to 44999",
                              str_detect(variable, "_010") ~ "45000 to 49999",
                              str_detect(variable, "_011") ~ "50000 to 59999",
                              str_detect(variable, "_012") ~ "60000 to 74999",
                              str_detect(variable, "_013") ~ "75000 to 99999",
                              str_detect(variable, "_014") ~ "100000 to 124999",
                              str_detect(variable, "_015") ~ "125000 to 149999",
                              str_detect(variable, "_016") ~ "150000 to 199999",
                              str_detect(variable, "_017") ~ "200000 or more"),
         inc_group = case_when(str_detect(variable, "_002") ~ "below_15k",
                               str_detect(variable, "_003") ~ "below_15k",
                               str_detect(variable, "_004") ~ "below_30k",
                               str_detect(variable, "_005") ~ "below_30k",
                               str_detect(variable, "_006") ~ "below_30k",
                               str_detect(variable, "_007") ~ "below_45k",
                               str_detect(variable, "_008") ~ "below_45k",
                               str_detect(variable, "_009") ~ "below_45k",
                               str_detect(variable, "_010") ~ "below_60k",
                               str_detect(variable, "_011") ~ "below_60k",
                               str_detect(variable, "_012") ~ "below_75k",
                               str_detect(variable, "_013") ~ "below_100k",
                               str_detect(variable, "_014") ~ "above_100k",
                               str_detect(variable, "_015") ~ "above_100k",
                               str_detect(variable, "_016") ~ "above_100k",
                               str_detect(variable, "_017") ~ "above_100k"))

# Save CSV
write_csv(faminc_race_county, "data/income_bands_race_county_2024.csv")


# Estimated Families making under annual survival budget (estimated), by County ----
# Using ALICE method (see text below) to estimate proportion in income bracket

faminc_county_proportion <- faminc_race_county %>% 
  mutate(county = str_remove(locality, " County, Virginia| city, Virginia")) |> 
  left_join(budget_allfam_avg) |> 
  filter(inc_band != "Total Families") %>% 
  separate(inc_band, into=c("inc_band_low","sep", "inc_band_high"), sep=" ", remove=FALSE) |> 
  select(-sep) |> 
  mutate(across(inc_band_low:inc_band_high, as.numeric)) |> 
  mutate(prop_test = case_when(avg_allfamily_budget >= inc_band_low & avg_allfamily_budget <= inc_band_high ~ TRUE,
                          .default = FALSE)) |> 
  mutate(prop = case_when(prop_test == TRUE ~ (avg_allfamily_budget - inc_band_low) / (inc_band_high - inc_band_low),
                          .default = 1),
         est_prop = estimate * prop)

view(faminc_county_proportion |> filter(prop_test==TRUE) |> filter(group=="All Families") |> select(GEOID, county, inc_band))

# Calculate totals and percent for budgets in income band 75000 to 99999 / below_100k

below_100k_geo <- c("51003", "51065", "51079", "51125", "51540")

faminc_county_prop100k <- faminc_county_proportion %>% 
  filter(GEOID %in% below_100k_geo) %>% 
  mutate(budget_group = case_when(inc_band_high <= 99999 ~ "inc_under_budget",
                           inc_band_high > 99999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, group, avg_allfamily_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families = first(total_families)) %>% 
  ungroup() 

faminc_county_prop100k_adj <- faminc_county_prop100k |> 
  select(GEOID, budget_group, est_prop, total_families) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(faminc_county_prop100k) |> 
  mutate(percent = round((est_prop_adj/total_families)*100, 2))

# Calculate totals and percent for budgets in income band 60000 to 74999 / below_75k

below_75k_geo <- c("51029", "51109")

faminc_county_prop75k <- faminc_county_proportion %>% 
  filter(GEOID %in% below_75k_geo) %>% 
  mutate(budget_group = case_when(inc_band_high <= 74999 ~ "inc_under_budget",
                                  inc_band_high > 74999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, locality, year, group, avg_allfamily_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families = first(total_families)) %>% 
  ungroup() 

faminc_county_prop75k_adj <- faminc_county_prop75k |> 
  select(GEOID, budget_group, est_prop, total_families) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(faminc_county_prop75k) |> 
  mutate(percent = round((est_prop_adj/total_families)*100, 2))

# Combine tables ----

family_race_county_summary <- rbind(faminc_county_prop100k_adj, faminc_county_prop75k_adj) |> 
  select(GEOID, locality, year, group, avg_allfamily_budget, inc_v_budget=budget_group, estimate, moe, est_prop, est_prop_adj, total_families, percent)

# Save
write_csv(family_race_county_summary, "data/family_race_county_summary.csv")

# For reference ALICE Methodology (https://www.unitedforalice.org/methodology):
# Applying the Threshold to Determine ALICE Status: The ACS estimates for household income are aggregated into
# the following categories: less than $10,000, $15,000, $20,000, $25,000, $30,000, $35,000, $40,000, $45,000, $50,000,
# $60,000, $75,000, $100,000, $125,000, $150,000, $200,000, and more than $200,000. With 2021 data and earlier, the
# ALICE Threshold was rounded to the closest income bracket. Starting with 2022 data, the number of households within
# each income bracket will be divided in proportion to where the Threshold falls within the Census income bracket. For
# example, if within the $30,000–$34,999 income bracket there are 100 households and the ALICE Threshold is
# $32,500, only 50 of those households would be determined to be below the ALICE Threshold. This minimizes
# fluctuation when budgets move from one income bracket to the next.

# Number of Households in Poverty: Households in poverty are reported by the Census Bureau, which uses the
# Poverty Thresholds. (Note: In the ALICE Reports, we often reference the U.S. Department of Health and Human
#                      Services’ Poverty Guidelines when referencing the FPL, because these simplified values are used for program
#                      administration and are most familiar/relevant for our many partners.)

# Number of ALICE Households: The number of ALICE households is derived by subtracting the number of
# households in poverty from the total number below the ALICE Threshold. Poverty numbers are provided by the ACS
# for most demographic groups. Because the ACS does not provide poverty estimates by race/ethnicity, the income
# breakpoint of $15,000 per year is used as a proxy for this category.

# Struggling Families by Census Tract ----

# Read in tractnames ----
tractnames <- read_csv("data/tractnames.csv") |> mutate(GEOID = as.character(GEOID))

# Source: ACS Table B19101
acs_B19101_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  table = "B19101",
  summary_var = "B19101_001",
  year = year, 
  survey = "acs5")

faminc_tract <- acs_B19101_tract %>% 
  mutate(group = "All Families") %>% 
  rename("total_families" = "summary_est") %>% 
  select(GEOID, NAME, variable, group, estimate, moe, total_families)

# Wrangle data
faminc_tract <- faminc_tract %>% 
  mutate(year = year,
         inc_band = case_when(str_detect(variable, "_001") ~ "Total Families",
                              str_detect(variable, "_002") ~ "Less than 10000",
                              str_detect(variable, "_003") ~ "10000 to 14999",
                              str_detect(variable, "_004") ~ "15000 to 19999",
                              str_detect(variable, "_005") ~ "20000 to 24999",
                              str_detect(variable, "_006") ~ "25000 to 29999",
                              str_detect(variable, "_007") ~ "30000 to 34999",
                              str_detect(variable, "_008") ~ "35000 to 39999",
                              str_detect(variable, "_009") ~ "40000 to 44999",
                              str_detect(variable, "_010") ~ "45000 to 49999",
                              str_detect(variable, "_011") ~ "50000 to 59999",
                              str_detect(variable, "_012") ~ "60000 to 74999",
                              str_detect(variable, "_013") ~ "75000 to 99999",
                              str_detect(variable, "_014") ~ "100000 to 124999",
                              str_detect(variable, "_015") ~ "125000 to 149999",
                              str_detect(variable, "_016") ~ "150000 to 199999",
                              str_detect(variable, "_017") ~ "200000 or more"),
         inc_group = case_when(str_detect(variable, "_002") ~ "below_15k",
                               str_detect(variable, "_003") ~ "below_15k",
                               str_detect(variable, "_004") ~ "below_35k",
                               str_detect(variable, "_005") ~ "below_35k",
                               str_detect(variable, "_006") ~ "below_35k",
                               str_detect(variable, "_007") ~ "below_35k",
                               str_detect(variable, "_008") ~ "below_45k",
                               str_detect(variable, "_009") ~ "below_45k",
                               str_detect(variable, "_010") ~ "below_50k",
                               str_detect(variable, "_011") ~ "below_60k",
                               str_detect(variable, "_012") ~ "below_75k",
                               str_detect(variable, "_013") ~ "below_100k",
                               str_detect(variable, "_014") ~ "above_100k",
                               str_detect(variable, "_015") ~ "above_100k",
                               str_detect(variable, "_016") ~ "above_100k",
                               str_detect(variable, "_017") ~ "above_100k"))

# Save CSV
write_csv(faminc_tract, "data/income_bands_tract_2024.csv")

# Estimated Families making under annual survival budget (estimated), by tract ----
# Using ALICE method (see text below) to estimate proportion in income bracket

faminc_tract_proportion <- faminc_tract %>% 
  left_join(tractnames) |> 
  rename(county = locality) |> 
  left_join(budget_allfam_avg) |> 
  filter(inc_band != "Total Families") %>% 
  separate(inc_band, into=c("inc_band_low","sep", "inc_band_high"), sep=" ", remove=FALSE) |> 
  select(-sep) |> 
  mutate(across(inc_band_low:inc_band_high, as.numeric)) |> 
  mutate(prop_test = case_when(avg_allfamily_budget >= inc_band_low & avg_allfamily_budget <= inc_band_high ~ TRUE,
                               .default = FALSE)) |> 
  mutate(prop = case_when(prop_test == TRUE ~ (avg_allfamily_budget - inc_band_low) / (inc_band_high - inc_band_low),
                          .default = 1),
         est_prop = estimate * prop)

view(faminc_tract_proportion |> filter(prop_test==TRUE) |> filter(group=="All Families") |> select(GEOID, county, inc_band))

# Calculate totals and percent for budgets in income band 75000 to 99999 / below_100k

below_100k_geo <- c("51003", "51065", "51079", "51125", "51540")
pattern <- paste0("^(", paste(below_100k_geo, collapse = "|"), ")")

faminc_tract_prop100k <- faminc_tract_proportion %>% 
  filter(str_detect(GEOID, pattern)) %>% 
  mutate(budget_group = case_when(inc_band_high <= 99999 ~ "inc_under_budget",
                                  inc_band_high > 99999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, county, tractnames, NAME, year, group, avg_allfamily_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families = first(total_families)) %>% 
  ungroup() 

faminc_tract_prop100k_adj <- faminc_tract_prop100k |> 
  select(GEOID, budget_group, est_prop, total_families) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(faminc_tract_prop100k) |> 
  mutate(percent = round((est_prop_adj/total_families)*100, 2))

# Calculate totals and percent for budgets in income band 60000 to 74999 / below_75k

below_75k_geo <- c("51029", "51109")
pattern <- paste0("^(", paste(below_75k_geo, collapse = "|"), ")")

faminc_tract_prop75k <- faminc_tract_proportion %>% 
  filter(str_detect(GEOID, pattern)) %>% 
  mutate(budget_group = case_when(inc_band_high <= 74999 ~ "inc_under_budget",
                                  inc_band_high > 74999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, county, tractnames, NAME, year, group, avg_allfamily_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_families = first(total_families)) %>% 
  ungroup() 

faminc_tract_prop75k_adj <- faminc_tract_prop75k |> 
  select(GEOID, budget_group, est_prop, total_families) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_families - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(faminc_tract_prop75k) |> 
  mutate(percent = round((est_prop_adj/total_families)*100, 2))

# Combine tables ----

family_tract_summary <- rbind(faminc_tract_prop100k_adj, faminc_tract_prop75k_adj) |> 
  select(GEOID, county, tractnames, NAME, year, group, avg_allfamily_budget, inc_v_budget=budget_group, estimate, moe, est_prop, est_prop_adj, total_families, percent)

# Save
write_csv(family_tract_summary, "data/family_tract_summary.csv")

# Median Family Income, County & Tract ----
# Source: ACS Table B19119

vars_B19119 <- c("Median Family Income" = "B19119_001")

acs_B19119_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B19119,
  year = year, 
  survey = "acs5")

acs_B19119_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  var = vars_B19119,
  year = year, 
  survey = "acs5")

# Wrangle tables:
med_faminc_county <- acs_B19119_county %>% 
  mutate(year = year) %>% 
  rename("locality" = "NAME",
         "label" = "variable") %>% 
  select(GEOID, locality, label, estimate, moe, year)

med_faminc_tract <- acs_B19119_tract %>% 
  mutate(year = year) %>% 
  rename("label" = "variable") %>% 
  # separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, NAME, label, estimate, moe, year)

# Join tract names and budget
med_faminc_tract <- med_faminc_tract %>%  
  left_join(tractnames) |> 
  rename(county = locality) |> 
  left_join(budget_allfam_avg)

# Calculate if budget above median income
med_faminc_tract <- med_faminc_tract |> 
  mutate(med_inc_below = case_when(estimate < avg_allfamily_budget ~ TRUE,
                                   estimate >= avg_allfamily_budget ~ FALSE))

# Write CSVs
write_csv(med_faminc_county, "data/med_faminc_county_2024.csv")
write_csv(med_faminc_tract, "data/med_faminc_tract_2024.csv")

# Household Income for 65+ by tract ----
# Source: ACS Table B19037: Age of Householder by Household Income
vars_B19037 <- c("Total Households 65+" = "B19037_053" ,
                 "Less than 10000" = "B19037_054",
                 "10000 to 14999" = "B19037_055",
                 "15000 to 19999" = "B19037_056",
                 "20000 to 24999" = "B19037_057",
                 "25000 to 29999" = "B19037_058",
                 "30000 to 34999" = "B19037_059",
                 "35000 to 39999" = "B19037_060",
                 "40000 to 44999" = "B19037_061",
                 "45000 to 49999" = "B19037_062",
                 "50000 to 59999" = "B19037_063",
                 "60000 to 74999" = "B19037_064",
                 "75000 to 99999" = "B19037_065",
                 "100000 to 124999" = "B19037_066",
                 "125000 to 149999" = "B19037_067",
                 "150000 to 199999" = "B19037_068",
                 "200000 or more" = "B19037_069")

acs_B19037_tract <- get_acs(
  geography = "tract",
  state = "VA",
  county = county_codes,
  variables = vars_B19037,
  summary_var = "B19037_053",
  year = year, 
  survey = "acs5")

inc65over_tract <- acs_B19037_tract %>% 
  mutate(group = "Households 65+") %>% 
  rename("total_hh_65over" = "summary_est") %>% 
  select(GEOID, NAME, variable, group, estimate, moe, total_hh_65over)

# Wrangle data
inc65over_tract <- inc65over_tract %>% 
  mutate(year = year,
         inc_group = case_when(variable=="Less than 10000" ~ "below_15k",
                               variable=="10000 to 14999" ~ "below_15k",
                               variable=="15000 to 19999" ~ "below_30k",
                               variable=="20000 to 24999" ~ "below_30k",
                               variable=="25000 to 29999" ~ "below_30k",
                               variable=="30000 to 34999" ~ "below_45k",
                               variable=="35000 to 39999" ~ "below_45k",
                               variable=="40000 to 44999" ~ "below_45k",
                               variable=="45000 to 49999" ~ "below_60k",
                               variable=="50000 to 59999" ~ "below_60k",
                               variable=="60000 to 74999" ~ "below_75k",
                               variable=="75000 to 99999" ~ "below_100k",
                               variable=="100000 to 124999" ~ "above_100k",
                               variable=="125000 to 149999" ~ "above_100k",
                               variable=="150000 to 199999" ~ "above_100k",
                               variable=="200000 or more" ~ "above_100k"))

# Save CSV
write_csv(inc65over_tract, "data/income_bands_65over_tract_2024.csv")

# Estimated Families making under annual survival budget (estimated), by tract ----
# Using ALICE method (see text below) to estimate proportion in income bracket


inc65over_tract_proportion <- inc65over_tract %>% 
  left_join(tractnames) |> 
  rename(county = locality) |> 
  left_join(budget_65plus_avg) |> 
  rename(inc_band = variable) |> 
  filter(inc_band != "Total Households 65+") %>% 
  separate(inc_band, into=c("inc_band_low","sep", "inc_band_high"), sep=" ", remove=FALSE) |> 
  select(-sep) |> 
  mutate(across(inc_band_low:inc_band_high, as.numeric)) |> 
  mutate(prop_test = case_when(avg_65plus_budget >= inc_band_low & avg_65plus_budget <= inc_band_high ~ TRUE,
                               .default = FALSE)) |> 
  mutate(prop = case_when(prop_test == TRUE ~ (avg_65plus_budget - inc_band_low) / (inc_band_high - inc_band_low),
                          .default = 1),
         est_prop = estimate * prop)

view(inc65over_tract_proportion |> filter(prop_test==TRUE) |> filter(group=="Households 65+") |> select(GEOID, county, inc_band))

# Calculate totals and percent for budgets in income band 50000 to 59999 / below_60k

below_60k_geo <- c("51003", "51065", "51079", "51125", "51540")
pattern <- paste0("^(", paste(below_60k_geo, collapse = "|"), ")")

inc65over_tract_prop60k <- inc65over_tract_proportion %>% 
  filter(str_detect(GEOID, pattern)) %>% 
  mutate(budget_group = case_when(inc_band_high <= 59999 ~ "inc_under_budget",
                                  inc_band_high > 59999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, county, tractnames, NAME, year, group, avg_65plus_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_hh_65over = first(total_hh_65over)) %>% 
  ungroup() 

inc65over_tract_prop60k_adj <- inc65over_tract_prop60k |> 
  select(GEOID, budget_group, est_prop, total_hh_65over) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_hh_65over - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(inc65over_tract_prop60k) |> 
  mutate(percent = round((est_prop_adj/total_hh_65over)*100, 2))

# Calculate totals and percent for budgets in income band 45000 to 49999 / below_50k

below_50k_geo <- c("51109")
pattern <- paste0("^(", paste(below_50k_geo, collapse = "|"), ")")

inc65over_tract_prop50k <- inc65over_tract_proportion %>% 
  filter(str_detect(GEOID, pattern)) %>% 
  mutate(budget_group = case_when(inc_band_high <= 49999 ~ "inc_under_budget",
                                  inc_band_high > 49999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, county, tractnames, NAME, year, group, avg_65plus_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_hh_65over = first(total_hh_65over)) %>% 
  ungroup() 

inc65over_tract_prop50k_adj <- inc65over_tract_prop50k |> 
  select(GEOID, budget_group, est_prop, total_hh_65over) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_hh_65over - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(inc65over_tract_prop50k) |> 
  mutate(percent = round((est_prop_adj/total_hh_65over)*100, 2))

# Calculate totals and percent for budgets in income band 40000 to 44999 / below_45k

below_45k_geo <- c("51029")
pattern <- paste0("^(", paste(below_45k_geo, collapse = "|"), ")")

inc65over_tract_prop45k <- inc65over_tract_proportion %>% 
  filter(str_detect(GEOID, pattern)) %>% 
  mutate(budget_group = case_when(inc_band_high <= 44999 ~ "inc_under_budget",
                                  inc_band_high > 44999 | is.na(inc_band_high) ~ "inc_over_budget")) |> 
  group_by(GEOID, county, tractnames, NAME, year, group, avg_65plus_budget, budget_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            est_prop = sum(est_prop),
            total_hh_65over = first(total_hh_65over)) %>% 
  ungroup() 

inc65over_tract_prop45k_adj <- inc65over_tract_prop45k |> 
  select(GEOID, budget_group, est_prop, total_hh_65over) |> 
  pivot_wider(names_from = budget_group, values_from = est_prop) |> 
  mutate(inc_over_budget = total_hh_65over - inc_under_budget) |> 
  pivot_longer(inc_over_budget:inc_under_budget, names_to = "budget_group", values_to = "est_prop_adj") %>%
  left_join(inc65over_tract_prop45k) |> 
  mutate(percent = round((est_prop_adj/total_hh_65over)*100, 2))

# Combine tables ----

inc65over_tract_summary <- rbind(inc65over_tract_prop60k_adj, inc65over_tract_prop50k_adj, inc65over_tract_prop45k_adj) |> 
  select(GEOID, county, tractnames, NAME, year, group, avg_65plus_budget, inc_v_budget=budget_group, estimate, moe, est_prop, est_prop_adj, total_hh_65over, percent)

# Save
write_csv(inc65over_tract_summary, "data/over65_tract_summary.csv")
