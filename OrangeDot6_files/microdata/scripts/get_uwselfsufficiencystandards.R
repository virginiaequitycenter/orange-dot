# University of Washington, Center for Women's Welfare, Self Sufficiency Standards
# https://selfsufficiencystandard.org/
# 2021 Dataset
# Downloaded on 2024-03-07 mpc
#
# Virginia: https://selfsufficiencystandard.org/virginia/
# Documentation: https://www.census.gov/programs-surveys/acs/microdata/documentation.html


# setup ----
library(tidyverse)
library(readxl)


# download ----
va_link <- "https://selfsufficiencystandard.org/wp-content/uploads/2021/11/VA2021_all_families.xlsx"

download.file(va_link, "downloads/uw_selfsufficiencystandard_va.xlsx")

# read data ----
va_sss <- read_excel("downloads/uw_selfsufficiencystandard_va.xlsx",
                     sheet = "SSS", skip = 8)

cvl_region <- c("Albemarle County", "Charlottesville city",
                "Fluvanna County", "Greene County",
                "Louisa County", "Nelson County")

cvl_sss <- va_sss %>% 
  filter(famcode %in% cvl_region) %>% 
  select(-tablenum) %>% 
  pivot_longer(-famcode) %>%
  pivot_wider(id_cols = name, names_from="famcode", values_from="value") %>%
  rename(family_type=name)


# save ----
write_csv(cvl_sss, "data/cvlregion_uwselfsufficiencystandards.csv")


# family_type identifies number of people in family of each type
# a = adult
# i = infant
# p = preschooler
# s = school-age
# t = teenager
# we will need to decide how to simplify this
#   e.g., use average standards within families with same number of adults and children
#   or choose only a small number of family types to apply, or something else
# and how to combine across localities (city, counties) as we cannot 
# link acs respondents to their localities
#   e.g., generate population-weighted average across localities,
#   just use highest locality standards, something else?


# citation note ----
# how to cite, from their webpage
# All Self-Sufficiency Standard data that has been produced by the Center for Women’s Welfare is publicly available. 
# When using the data, please credit the Self-Sufficiency Standard at the Center for Women’s Welfare, University of Washington.