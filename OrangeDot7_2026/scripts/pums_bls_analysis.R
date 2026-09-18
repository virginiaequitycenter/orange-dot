# Microdata Analysis
#   Workers by hours worked
#   Workers by wage bins 

# Libraries ----
library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(stringi)
library(survey)

# Set WD ----
setwd(here("OrangeDot7_2026"))


# PUMS Analysis ----
# Data ---- 
hh <- read_csv("data/pums_household_processed.csv")
per <- read_csv("data/pums_person_processed.csv")
bls <- read_csv("data/bls_occupation_wages.csv")

# Employment analysis ----
## Workers by hours ----
# wkhp (per), usual hours worked per week
# bins based on likelhood of benefits acces
# https://www.bls.gov/opub/mlr/2015/article/the-relationship-between-access-to-benefits-and-weekly-work-hours.htm
workers_by_hours <- per %>% 
  filter(!is.na(below_budget_finc) & !is.na(esr) & esr != 6) %>% 
  mutate(hours_bin = case_when(
    wkhp >= 40 ~ "40 or more",
    wkhp >= 35 & wkhp < 40 ~ "35 to 39",
    wkhp >= 30 & wkhp < 35 ~ "30 to 34",
    wkhp >= 20 & wkhp < 30 ~ "20 to 29",
    wkhp > 0 & wkhp < 20 ~ "Less than 20",
    esr == 3 ~ "Unemployed"),
    hours_bin = factor(hours_bin, 
                       levels = c("40 or more", "35 to 39", "30 to 34", "20 to 29", 
                                  "Less than 20", "Unemployed"))) %>% 
  group_by(below_budget_finc) %>% 
  count(hours_bin, wt = pwgtp, name = "count") %>% 
  group_by(below_budget_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(workers_by_hours, file = "data/pums_workers_by_hours.csv")

## Workers by wage bins ----
# per: occp/socp = occupation codes 
# median wages of occupation: https://www.bls.gov/oes/current/oes_nat.htm#11-0000
#   Lots of non-matched (socp ending with X, but also others...)
#   Strategy: for socp in pums that don't match socp in bls 
#   truncate to first four digits and use overall occupation category code/wages
bls <- bls %>% 
  mutate(socp = as.character(soc_code))

per_wage <- per %>% 
  left_join(bls, by = "socp")

sum(!is.na(per_wage$socp)) # 7969 occupation codes
sum(!is.na(per_wage$soc_code)) # 4591 occupation codes matched to bls data

per_socp_matched <- per_wage %>% 
  filter(!is.na(soc_code))

per_socp_unmatched <- per_wage %>% 
  filter(is.na(soc_code))   
  select(-c(names(bls)[1:23]))

bls_overall <- bls %>% 
  mutate(socp2 = str_sub(socp, 1,2),
         socp4 = str_sub(socp, 1,4),
         soc_code = as.numeric(soc_code)) 
  # select(-socp)

per_socp_unmatched <- per_socp_unmatched %>% 
  mutate(socp2 = str_sub(socp, 1,2),
         socp4 = str_sub(socp, 1,4))

per_socp_unmatched_test <- per_socp_unmatched %>% 
  left_join(bls_overall, by = join_by(socp))

sum(!is.na(per_socp_unmatched_test$socp)) # 3378 occupation codes
sum(!is.na(per_socp_unmatched_test$soc_code)) # 0 occupation codes matched to bls data

per_socp_unmatched_test %>% 
  filter(is.na(soc_code)) %>%
  select(socp, socp2, soc_code, median_wage_bin) %>% 
  view()
# unmatched are military and unemployed
per_socp_unmatched %>% 
  select(esr, soc_code, socp2, median_wage_bin_label) %>% view()


per_bls <- bind_rows(per_socp_matched, per_socp_unmatched_test)

sum(!is.na(per_bls$socp)) # 7969 occupation codes
sum(!is.na(per_bls$soc_code)) # 4591 occupation codes matched to bls data

workers_by_medianwages <- per_wage %>% 
  filter(!is.na(below_budget_finc) & esr %in% c(1,2)) %>% 
  mutate(median_wage_bin_label = factor(median_wage_bin_label,
                                        levels = c("28320-38540", "38580-49300", "49420-59790",
                                                   "60010-74100", "74110-100930", "101210-317900"))) %>%
  group_by(below_budget_finc) %>% 
  count(median_wage_bin_label, wt = pwgtp, name = "count") %>% 
  group_by(below_budget_finc) %>% 
  mutate(percent = count/sum(count)*100)
# NOTE: this is not what the individuals represented earn
#       but the range for what the jobs they hold typically pay

write_csv(workers_by_medianwages, file = "data/pums_workers_by_medianwages.csv")

