# American Community Survey Public Use Microdata for the Charlottesville Region 
# https://www.census.gov/programs-surveys/acs/microdata/access.html
# 2022 5-year release
# Downloaded on 2024-03-07 mpc
#
# FTP access: https://www2.census.gov/programs-surveys/acs/data/pums/2022/5-Year/
# Virginia person-level records: https://www2.census.gov/programs-surveys/acs/data/pums/2022/5-Year/csv_pva.zip
# Virginia housing-level records: https://www2.census.gov/programs-surveys/acs/data/pums/2022/5-Year/csv_hva.zip
# Documentation: https://www.census.gov/programs-surveys/acs/microdata/documentation.html


# setup ----
library(tidyverse)


# download ----
pva_link <- "https://www2.census.gov/programs-surveys/acs/data/pums/2022/5-Year/csv_pva.zip"
hva_link <- "https://www2.census.gov/programs-surveys/acs/data/pums/2022/5-Year/csv_hva.zip"

download.file(pva_link, "downloads/csv_pva.zip")
download.file(hva_link, "downloads/csv_hva.zip")

unzip("downloads/csv_pva.zip", exdir = "downloads/")
unzip("downloads/csv_hva.zip", exdir = "downloads/")

pva <- read_csv("downloads/psam_p51.csv")
hva <- read_csv("downloads/psam_h51.csv")


# filter to cville region pumas ----
# these contain the six localities of the
# thomas jefferson planning district:
# Charlottesville, Albemarle, Louisa, Nelson, Fluvanna, Greene
# Buckingham is in puma 51-08300 (Soutside Planning District Commision & Commonwealth Regional Council)
#   so is excluded from this analysis
pcvl <- pva %>% 
  filter(PUMA20 %in% c("54001", "10901") |
           PUMA10 %in% c("51090", "51089"))

hcvl <- hva %>% 
  filter(PUMA20 %in% c("54001", "10901") |
           PUMA10 %in% c("51090", "51089"))


# save ----
write_csv(pcvl, "data/cvl_acspums_person.csv")
write_csv(hcvl, "data/cvl_acspums_household.csv")

