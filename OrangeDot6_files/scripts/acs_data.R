# ACS Data for Orange Dot Report 6.0

# Load packages ----
library(here)
library(tidyverse)
library(tidycensus)

# Set WD
setwd(here("OrangeDot6_files"))

# Creating basic objects ----

# Year for ACS data (single year)
year <- 2022

# Localities
county_codes <- c("003", "540", "065", "079", "109", "125", "029")

# Read in tract names
tract_names <- read_csv("data/tractnames.csv")
tract_names <- tract_names %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  rename("county" = "locality")

# ACS Variables ----
# A custom R function that creates a table of variable codes and metadata 
# for ACS 5-year, including subject and profile tables
all_acs_meta <- function(){
  # Gets the list of all variables from all acs5 metadata tables
  vars1 <- load_variables(year, "acs5", cache = TRUE) %>% select(-geography)
  vars2 <- load_variables(year, "acs5/subject", cache = TRUE)
  vars3 <- load_variables(year, "acs5/profile", cache = TRUE)
  
  # Provides column with specific lookup
  vars1$dataset_table <- "acs5"
  vars2$dataset_table <- "acs5/subject"
  vars3$dataset_table <- "acs5/profile"
  
  # Combine all table rows
  all_vars_meta <- rbind(vars1, vars2, vars3)
  
  return(all_vars_meta)
}

# Creates a table of all the metadata called "meta_table"
meta_table <- all_acs_meta()

# Opens the newly made table
# View(meta_table)

# Family Income by County ----
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
  mutate(label = case_when(str_detect(variable, "_001") ~ "Total Families",
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
         year = year,
         group = "All Families") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, label, group, estimate, moe, total_families, year)

# Save CSV
write_csv(faminc_county, "data/faminc_county_2022.csv")

# Estimated Families making under annual Self Sufficiency Wage, by County ----
# Localities where mean annual self sufficiency wage is approx $60,000 (fam compositions adults 1|2, child 1|2)
# Albemarle ($63321)
# Charlottesville City ($60,876)
# Fluvanna ($59,819)
# Greene (55744)
# Nelson (54249 / 57013 for a1p1s1)

ssw_60k_GEOID <- c("51003", "51540", "51065", "51079", "51125")

faminc_county_60k <- faminc_county %>% 
  filter(GEOID %in% ssw_60k_GEOID) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$60,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "below",
                               str_detect(variable, "_011") ~ "below",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, year, group, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Localities where mean annual self sufficiency wage is approx $50,000 (fam compositions adults 1|2, child 1|2)
# Louisa ($49157)

ssw_50k_GEOID <- c("51109")

faminc_county_50k <- faminc_county %>% 
  filter(GEOID %in% ssw_50k_GEOID) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$50,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "below",
                               str_detect(variable, "_011") ~ "above",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, year, group, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Localities where mean annual self sufficiency wage is approx $45,000 (fam compositions adults 1|2, child 1|2)
# Buckingham ($44475)

ssw_45k_GEOID <- c("51029")

faminc_county_45k <- faminc_county %>% 
  filter(GEOID %in% ssw_45k_GEOID) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$45,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "above",
                               str_detect(variable, "_011") ~ "above",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, year, group, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Bind tables
faminc_ssw_county <- rbind(faminc_county_60k, faminc_county_50k, faminc_county_45k) 
  
# Save CSV
write_csv(faminc_ssw_county, "data/faminc_ssw_county_2022.csv")

# Family Income by Race/Ethnicity, by County ----

# White Alone, Not Hispanic
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
  mutate(label = case_when(str_detect(variable, "_001") ~ "Total Families",
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
         year = year,
         group = "White, Not Hispanic") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, label, group, estimate, moe, total_families, year)

# Black Alone
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
  mutate(label = case_when(str_detect(variable, "_001") ~ "Total Families",
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
         year = year,
         group = "Black") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, label, group, estimate, moe, total_families, year)

# Hispanic
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
  mutate(label = case_when(str_detect(variable, "_001") ~ "Total Families",
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
         year = year,
         group = "Hispanic") %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, variable, label, group, estimate, moe, total_families, year)

# Join Race tables
faminc_race <- rbind(faminc_white, faminc_black, faminc_hispanic)

# Save CSV
write_csv(faminc_race, "data/faminc_race_county_2022.csv")

# Estimated Families making under annual Self Sufficiency Wage, by Race, by County ----
# Localities where mean annual self sufficiency wage is approx $60,000 (fam compositions adults 1|2, child 1|2)
# Albemarle ($63321)
# Charlottesville City ($60,876)
# Fluvanna ($59,819)
# Greene (55744)
# Nelson (54249 / 57013 for a1p1s1)

ssw_60k_GEOID <- c("51003", "51540", "51065", "51079", "51125")

faminc_race_60k <- faminc_race %>% 
  filter(GEOID %in% ssw_60k_GEOID) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$60,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "below",
                               str_detect(variable, "_011") ~ "below",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, year, group, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Localities where mean annual self sufficiency wage is approx $50,000 (fam compositions adults 1|2, child 1|2)
# Louisa ($49157)

ssw_50k_GEOID <- c("51109")

faminc_race_50k <- faminc_race %>% 
  filter(GEOID %in% ssw_50k_GEOID) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$50,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "below",
                               str_detect(variable, "_011") ~ "above",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, year, group, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Localities where mean annual self sufficiency wage is approx $45,000 (fam compositions adults 1|2, child 1|2)
# Buckingham ($44475)

ssw_45k_GEOID <- c("51029")

faminc_race_45k <- faminc_race %>% 
  filter(GEOID %in% ssw_45k_GEOID) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$45,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "above",
                               str_detect(variable, "_011") ~ "above",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, year, group, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Bind tables
faminc_ssw_race_county <- rbind(faminc_ssw_county, faminc_race_60k, faminc_race_50k, faminc_race_45k) 

# Save CSV
write_csv(faminc_ssw_race_county, "data/faminc_ssw_race_county_2022.csv")


# Family Income by Tract ----
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
  mutate(label = case_when(str_detect(variable, "_001") ~ "Total Families",
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
         year = year) %>% 
  rename("total_families" = "summary_est") %>% 
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, variable, label, estimate, moe, total_families, year)

# Join tract names
faminc_tract <- faminc_tract %>% 
  left_join(tract_names)

# Estimated Families making under annual Self Sufficiency Wage, by Tract ----
# Localities where mean annual self sufficiency wage is approx $60,000 (fam compositions adults 1|2, child 1|2)
# Albemarle ($63321)
# Charlottesville City ($60,876)
# Fluvanna ($59,819)
# Greene (55744)
# Nelson (54249 / 57013 for a1p1s1)

faminc_tract_60k <- faminc_tract %>% 
  filter(str_detect(GEOID, "51003|51540|51065|51079|51125")) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$60,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "below",
                               str_detect(variable, "_011") ~ "below",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, tract, tractnames, year, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>% 
  mutate(percent = round((estimate/total_families)*100, 2))

# Localities where mean annual self sufficiency wage is approx $50,000 (fam compositions adults 1|2, child 1|2)
# Louisa ($49157)
faminc_tract_50k <- faminc_tract %>% 
  filter(str_detect(GEOID, "51109")) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$50,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "below",
                               str_detect(variable, "_011") ~ "above",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, tract, tractnames, year, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Localities where mean annual self sufficiency wage is approx $45,000 (fam compositions adults 1|2, child 1|2)
# Buckingham ($44475)

faminc_tract_45k <- faminc_tract %>% 
  filter(str_detect(GEOID,"51029")) %>% 
  filter(label != "Total Families") %>% 
  mutate(self_suff_wage = "~$45,000",
         ssw_group = case_when(str_detect(variable, "_002") ~ "below",
                               str_detect(variable, "_003") ~ "below",
                               str_detect(variable, "_004") ~ "below",
                               str_detect(variable, "_005") ~ "below",
                               str_detect(variable, "_006") ~ "below",
                               str_detect(variable, "_007") ~ "below",
                               str_detect(variable, "_008") ~ "below",
                               str_detect(variable, "_009") ~ "below",
                               str_detect(variable, "_010") ~ "above",
                               str_detect(variable, "_011") ~ "above",
                               str_detect(variable, "_012") ~ "above",
                               str_detect(variable, "_013") ~ "above",
                               str_detect(variable, "_014") ~ "above",
                               str_detect(variable, "_015") ~ "above",
                               str_detect(variable, "_016") ~ "above",
                               str_detect(variable, "_017") ~ "above")) %>% 
  group_by(GEOID, locality, tract, tractnames, year, self_suff_wage, ssw_group) %>% 
  summarise(estimate = sum(estimate),
            moe = moe_sum(moe = moe, estimate = estimate),
            total_families = first(total_families)) %>% 
  ungroup() %>%
  mutate(percent = round((estimate/total_families)*100, 2))

# Bind tables
faminc_ssw_tract <- rbind(faminc_tract_60k, faminc_tract_50k, faminc_tract_45k) 

# Save CSV
write_csv(faminc_ssw_tract, "data/faminc_ssw_tract_2022.csv")

# Families with Income below Poverty Level, by County ----
# Source: ACS Table B17010: Poverty Status of Families by Family Type by Presence of Related Children by Age of Related Children

vars_B17010 <- c("Income below poverty level" = "B17010_002", 
                 "Income in the past 12 months at or above poverty level" = "B17010_022")

acs_B17010_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B17010,
  summary_var = "B17010_001",
  year = year, 
  survey = "acs5")

family_poverty_county <- acs_B17010_county %>% 
  mutate(group = "All Families",
         year = year,
         percent = round(estimate/summary_est *100, 2)) %>% 
  rename("locality" = "NAME",
         "label" = "variable", 
         "total_families" = "summary_est") %>% 
  select(GEOID, locality, label, group, estimate, moe, total_families, percent, year)

# Save CSV
write_csv(family_poverty_county, "data/family_poverty_county_2022.csv")

# Family Poverty status by Race/Ethnicity, by County

# White Alone, Not Hispanic
# Source: B17010H
vars_B17010H <- c("Income below poverty level; White, Not Hispanic" = "B17010H_002", 
                 "Income in the past 12 months at or above poverty level; White, Not Hispanic" = "B17010H_022")

acs_B17010H_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B17010H,
  summary_var = "B17010H_001",
  year = year, 
  survey = "acs5")

fam_poverty_white <- acs_B17010H_county %>% 
  mutate(year = year,
         percent = round(estimate/summary_est *100, 2)) %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  separate(variable, into=c("label", "group"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, label, group, estimate, moe, total_families, percent, year)

# Black Alone
# Source: B17010B
vars_B17010B <- c("Income below poverty level; Black" = "B17010B_002", 
                  "Income in the past 12 months at or above poverty level; Black" = "B17010B_022")

acs_B17010B_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B17010B,
  summary_var = "B17010B_001",
  year = year, 
  survey = "acs5")

fam_poverty_black <- acs_B17010B_county %>% 
  mutate(year = year,
         percent = round(estimate/summary_est *100, 2)) %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  separate(variable, into=c("label", "group"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, label, group, estimate, moe, total_families, percent, year)

# Hispanic
# Source: B17010I
vars_B17010I <- c("Income below poverty level; Hispanic" = "B17010I_002", 
                  "Income in the past 12 months at or above poverty level; Hispanic" = "B17010I_022")

acs_B17010I_county <- get_acs(
  geography = "county",
  state = "VA",
  county = county_codes,
  var = vars_B17010I,
  summary_var = "B17010I_001",
  year = year, 
  survey = "acs5")

fam_poverty_hispanic <- acs_B17010I_county %>% 
  mutate(year = year,
         percent = round(estimate/summary_est *100, 2)) %>% 
  rename("locality" = "NAME",
         "total_families" = "summary_est") %>% 
  separate(variable, into=c("label", "group"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, label, group, estimate, moe, total_families, percent, year)

# Combine tables
fam_poverty_race_county <- rbind(family_poverty_county, fam_poverty_white, fam_poverty_black, fam_poverty_hispanic)

# Save CSV
write_csv(fam_poverty_race_county, "data/fam_poverty_race_county_2022.csv")

# Single table for below poverty, below ssw, above ssw ----
fam_pov_race <- fam_poverty_race_county %>% 
  filter(label == "Income below poverty level") %>% 
  mutate(ssw_group = case_when(label == "Income below poverty level" ~ "poverty")) %>% 
  select(GEOID, locality, year, group, ssw_group, estimate, moe, total_families, percent)

family_poverty_ssw_race_county <- faminc_ssw_race_county %>% 
  select(-c(self_suff_wage)) %>% 
  rbind(fam_pov_race)

family_poverty_ssw_race_county <- family_poverty_ssw_race_county[order(family_poverty_ssw_race_county$GEOID, family_poverty_ssw_race_county$group),]

# Save CSV
write_csv(family_poverty_ssw_race_county, "data/fam_pov_ssw_race_county_2022.csv")

# Median Family Income: County & Tract ----
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
  separate(NAME, into=c("tract","locality", "state"), sep="; ", remove=FALSE) %>%
  select(GEOID, locality, tract, label, estimate, moe, year)

# Join tract names
med_faminc_tract <- med_faminc_tract %>% 
  left_join(tract_names)

# Write CSVs
write_csv(med_faminc_county, "data/med_faminc_county_2022.csv")
write_csv(med_faminc_tract, "data/med_faminc_tract_2022.csv")

