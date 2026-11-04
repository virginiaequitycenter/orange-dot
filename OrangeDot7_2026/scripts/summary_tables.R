# Libraries
library(here)
library(tidyverse)

# Set WD ----
setwd(here("OrangeDot7_2026"))

# Summary of families struggling
# Read in data
sum_fam_region <- read_csv("data/family_race_county_summary.csv") |> 
  filter(group=="All Families") |> 
  mutate(locality = str_to_title(locality),
         locality = str_remove(locality, ", Virginia")) |> 
  select("locality","year","group","inc_v_budget","est_prop_adj","total_families","percent")

sum_fam_region <- sum_fam_region[order(sum_fam_region$locality), ]

sum_fam_region_total <- sum_fam_region |> 
  group_by(year, group, inc_v_budget) |> 
  summarise(est_prop_adj = sum(est_prop_adj),
            total_families = sum(total_families)) |> 
  mutate(percent = round(est_prop_adj/total_families *100, 2)) |> 
  mutate(locality = "Total for Region", .before = year) |> 
  ungroup()

sum_fam_region_table <- rbind(sum_fam_region, sum_fam_region_total)

sum_fam_region_table <- sum_fam_region_table |> 
  mutate(description = case_when(inc_v_budget=="inc_over_budget" ~ "Families earning at or above survival budget",
                                 inc_v_budget=="inc_under_budget" ~ "Families earning below survival budget")) |> 
  rename(estimate = est_prop_adj)

write_csv(sum_fam_region_table, "data/summary_all_families.csv")

# Families w/ and w/o children
# Read in data
fam_type_region <- read_csv("data/family_type_county_2024.csv") |> 
  mutate(locality = str_to_title(locality),
         locality = str_remove(locality, ", Virginia")) |>
  select("locality","year", "child_in_home","inc_v_budget", "est_prop_adj","total_families_by_group","percent") |> 
  mutate(child_in_home = case_when(str_detect(child_in_home, "With own") ~ "with_child",
                                   str_detect(child_in_home, "No own") ~ "no_child")) |> 
  pivot_wider(names_from = child_in_home, values_from = c(est_prop_adj, total_families_by_group, percent))

fam_type_region <- fam_type_region[order(fam_type_region$locality), ]

fam_type_region_total <- fam_type_region |> 
  group_by(year, inc_v_budget) |> 
  summarise(est_prop_adj_with_child = sum(est_prop_adj_with_child),
            est_prop_adj_no_child = sum(est_prop_adj_no_child),
            total_families_by_group_with_child = sum(total_families_by_group_with_child),
            total_families_by_group_no_child = sum(total_families_by_group_no_child)) |> 
  mutate(percent_with_child = round(est_prop_adj_with_child/total_families_by_group_with_child *100, 2),
         percent_no_child = round(est_prop_adj_no_child/total_families_by_group_no_child *100, 2)) |> 
  mutate(locality = "Total for Region", .before = year) |> 
  ungroup()

fam_type_region_table <- rbind(fam_type_region, fam_type_region_total)

fam_type_region_table <- fam_type_region_table |> 
  mutate(description = case_when(inc_v_budget=="inc_over_budget" ~ "Families earning at or above survival budget",
                                 inc_v_budget=="inc_under_budget" ~ "Families earning below survival budget")) |> 
  rename(estimate_no_child = est_prop_adj_no_child,
         estimate_with_child = est_prop_adj_with_child)

write_csv(fam_type_region_table, "data/summary_by_family_type.csv")

# Race/Ethnicity summary 
# Read in data
all_fam_race <- read_csv("data/family_race_county_summary.csv")

sum_race <- all_fam_race |> 
  group_by(year, group, inc_v_budget) |> 
  summarise(est_prop_adj = sum(est_prop_adj),
            total_families = sum(total_families)) |> 
  mutate(percent = round(est_prop_adj/total_families *100, 2)) |> 
  mutate(locality = "Total for Region", .before = year) |> 
  ungroup()

sum_race <- sum_race |> 
  mutate(description = case_when(inc_v_budget=="inc_over_budget" ~ "Families earning at or above survival budget",
                                 inc_v_budget=="inc_under_budget" ~ "Families earning below survival budget")) |> 
  rename(estimate = est_prop_adj)

write_csv(sum_race, "data/summary_race_ethn_region.csv")


