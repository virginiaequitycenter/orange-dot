# ALICE 
# Source: https://www.unitedforalice.org/state-overview/Virginia

# Load packages ----
library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

# Localities ----
county_codes <- c("003", "540", "065", "079", "109", "125", "029")

# ALICE Households & Threshold 2010-2022 -----
# Get ALICE Data (2010-2022): https://www.unitedforalice.org/state-overview/Virginia

url <- "https://www.unitedforalice.org/Attachments/StateDataSheet/2024%20ALICE%20-%20Virginia%20Data%20Sheet.xlsx"

# Download file
download.file(url, destfile="data/tempdata/2024_ALICE_Virginia_Data_Sheet.xlsx", method="libcurl")

# Read data - County (2010-2022)
ALICE_sheet_county <- read_excel("data/tempdata/2024_ALICE_Virginia_Data_Sheet.xlsx", sheet = "County 2010-2022") %>% 
  clean_names()

# ALICE Thresholds, 2010-2022
ALICE_county_2022 <- ALICE_sheet_county %>% 
  filter(str_detect(geo_id2, paste0("51", county_codes, collapse = '|'))) %>% 
  filter(year == 2022) %>% 
  rename(GEOID = geo_id2,
         locality = geo_display_label)

write_csv(ALICE_county_2022, "data/ALICE_county_2022.csv")
