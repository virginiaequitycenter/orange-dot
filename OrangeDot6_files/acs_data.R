# ACS Data for Orange Dot Report 6.0

# Load packages ----
library(tidyverse)
library(tidycensus)

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
