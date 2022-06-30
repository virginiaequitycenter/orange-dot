# Orange dot report shape file 
# Lee LeBoeuf
# 2021-06-29

## Pulling a shape file for the Orange Dot report based on the 2020 census tracts 
## including the following localities Albemarle (003), Buckingham (029), Fluvanna (065),
##  Greene (079), Louisa (109), Nelson (125), and Charlottesville (540)

library(tidyverse)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# cville region ---
regionfips <- c("540", "003", "065", "079", "109", "125", "029")

# county polygons
cville_tracts <- tracts(state = "51", year = 2020)
cville_tracts <- cville_tracts %>% 
  filter(COUNTYFP %in% regionfips)

# save for R
saveRDS(cville_tracts, file = "cvilleODreport_tracts.RDS")
