# Orange Dot 6.0 Microdata Analysis
# Analysis of families below economic self-sufficiency thresholds
# Generate data for inclusion in report
# 2024-10-02 mpc


# Estimates to generate
#   Families by (householder) age bins
#   Families by race categories
#   Families by housing tenure (rent/own)
#   Families by shelter burden
#   People by disability status
#   Workers by hours worked
#   Workers by wage bins 
#   Workers (or families?) by income bins
# List of family composition counts


# Libraries ----
library(tidyverse)
library(survey)


# Data ---- 
hh <- read_csv("data/household_processed.csv")
per <- read_csv("data/person_processed.csv")
bls <- read_csv("data/occupation_wages.csv")
bls_overall <- read_csv("data/occupation_wages_catchallcategories.csv")


# Families by age bins ----
# hhldragep (hh)
families_by_age <- hh %>% 
  filter(!is.na(below_ess_finc)) %>% 
  mutate(hhldrage_bin = case_when(
    hhldragep < 25 ~ "Under 25",
    hhldragep >= 25 & hhldragep < 35 ~ "25 to 34",
    hhldragep >= 35 & hhldragep < 45 ~ "35 to 44",
    hhldragep >= 45 & hhldragep < 55 ~ "45 to 54",
    hhldragep >= 55 & hhldragep < 65 ~ "55 to 64",
    hhldragep >= 65 ~ "65 and over"
    ),
    hhldrage_bin = factor(hhldrage_bin, 
                          levels = c("Under 25", "25 to 34", "35 to 44",
                                     "45 to 54", "55 to 64", "65 and over"))) %>% 
  group_by(below_ess_finc) %>% 
  count(hhldrage_bin, wt = wgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(families_by_age, file = "data/families_by_age.csv")


# Families by race/ethnicity ----
# hhldrrac1p, hhldrhisp (hh)
families_by_race <- hh %>% 
  filter(!is.na(below_ess_finc)) %>% 
  mutate(hhldrhisp_bin = ifelse(hhldrhisp == "01", "Non-Hispanic", "Hispanic"),
         hhldr_race_ethn = case_when(
           hhldrrac1p == 1 & hhldrhisp_bin == "Non-Hispanic" ~ "White",
           hhldrrac1p == 2 ~ "Black",
           hhldrrac1p == 6 ~ "Asian",
           hhldrrac1p == 3 | hhldrrac1p == 4 | hhldrrac1p == 5 | hhldrrac1p == 7 ~ "AI/AN/NH/PI",
           hhldrrac1p == 9 & hhldrhisp_bin == "Non-Hispanic" ~ "Multiracial",
           hhldrrac1p == 8 & hhldrhisp_bin == "Non-Hispanic" ~ "Some Other Race",
           hhldrhisp_bin == "Hispanic" & hhldrrac1p == 1 | hhldrrac1p == 8 | hhldrrac1p == 9 ~ "Hispanic"
  ),
  hhldr_race_ethn = fct_infreq(hhldr_race_ethn)) %>% 
  group_by(below_ess_finc) %>% 
  count(hhldr_race_ethn, wt = wgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(families_by_race, file = "data/families_by_race.csv")


# Housing ----
## Families by rent/own ----
## ten (hh): 1 = owned with morgtage, 2 = owned free and clear, 3 = rented, 4 = occupied w/o payment
families_by_tenure <- hh %>% 
  filter(!is.na(below_ess_finc)) %>% 
  mutate(own_rent = ifelse(ten == 1 | ten == 2, "Own", "Rent")) %>% 
  group_by(below_ess_finc) %>% 
  count(own_rent, wt = wgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(families_by_tenure, file = "data/families_by_tenure.csv")


## Familiesy by shelter burdened ----
# income/shelter costs
# less than 30, 30-49, 50 and more (not burdened, burdened, severely burdened)
# grpip (hh) (grntp/hincp), ocpip (hh) (smoc/hincp)
# underestimates costs for mobile home owners, see
# https://www.huduser.gov/portal/periodicals/cityscpe/vol10num2/ch8.pdf

# # check understanding
# hh %>% 
#   filter(!is.na(grntp)) %>% 
#   mutate(rent_percent = ((grntp*12)/hincp)*100,
#          rent_percent = round(rent_percent,0),
#          rent_percent = ifelse(rent_percent > 100, 101, rent_percent),
#          match = ifelse(grpip == rent_percent, "yes", "no")) %>% 
#   select(grntp, fincp, hincp, grpip, rent_percent, match) %>% 
#   ggplot() +
#   geom_point(aes(x = grpip, y = rent_percent))
# 
# hh %>% 
#   filter(!is.na(smocp)) %>% 
#   mutate(own_percent = ((smocp*12)/hincp)*100,
#          own_percent = round(own_percent,0),
#          own_percent = ifelse(own_percent > 100, 101, own_percent),
#          match = ifelse(ocpip == own_percent, "yes", "no")) %>%
#   select(smocp, fincp, hincp, ocpip, own_percent, match) %>% 
#   view()

# missing ocpip among owners
hh %>% 
  filter(!is.na(below_ess_finc)) %>% 
  filter(is.na(ocpip)) %>% 
  filter(ten == 1 | ten == 2) %>% 
  select(hincp, fincp, ten, smocp, ocpip) %>% 
  view()
# due to 0 or negative incomes: assign to 101?

# missing grpip among renters
hh %>% 
  filter(!is.na(below_ess_finc)) %>% 
  filter(is.na(grpip)) %>% 
  filter(ten == 3 | ten == 4) %>% 
  select(hincp, fincp, ten, grntp, grpip) %>% 
  view()
# if ten = 3, due to 0 income: assign to 101
# if ten = 4, due to 0 rent/costs? assign to 0

families_by_burden <- hh %>% 
  filter(!is.na(below_ess_finc)) %>% 
  mutate(rent_pip = ifelse(ten == 4, 0, grpip),
         rent_pip = ifelse(ten == 3 & is.na(grpip), 101, rent_pip),
         own_pip = ifelse(ten == 1 & is.na(ocpip), 101, ocpip),
         own_pip = ifelse(ten == 2 & is.na(ocpip), 101, own_pip),
         own_rent = ifelse(ten == 1 | ten == 2, "Own", "Rent"),
         shelter_pip = ifelse(own_rent == "Rent", rent_pip, own_pip),
         shelter_burden = case_when(
           shelter_pip < 30 ~ "Not Burdened",
           shelter_pip >= 30 & shelter_pip < 50 ~ "Burdened",
           shelter_pip >= 50 ~ "Severely Burdened"
           ),
         shelter_burden = factor(shelter_burden, 
                                 levels = c("Not Burdened", "Burdened", "Severely Burdened"))) %>% 
  group_by(below_ess_finc) %>% 
  count(shelter_burden, wt = wgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(families_by_burden, file = "data/families_by_burden.csv")


# People by disability status ----
# dis (per) 
people_by_disability <- per %>% 
  filter(!is.na(below_ess_finc)) %>% 
  group_by(below_ess_finc, dis) %>% 
  count(wt = pwgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(people_by_disability, file = "data/people_by_disability.csv")


# ddrs, dear, deye, dout, dphy, drem [self-care, hearing, vision, independent living, ambulatory, cognitive]
per %>% 
  filter(!is.na(below_ess_finc) & !is.na(drem)) %>% 
  group_by(below_ess_finc, drem) %>% 
  count(wt = pwgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)


# Employment ----
## Workers by hours ----
# wkhp (per), usual hours worked per week
# bins based on likelhood of benefits acces
# https://www.bls.gov/opub/mlr/2015/article/the-relationship-between-access-to-benefits-and-weekly-work-hours.htm
workers_by_hours <- per %>% 
  filter(!is.na(below_ess_finc) & !is.na(esr) & esr != 6) %>% 
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
  group_by(below_ess_finc) %>% 
  count(hours_bin, wt = pwgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)

write_csv(workers_by_hours, file = "data/workers_by_hours.csv")


## Workers by wage bins ----
# per: occp/socp = occupation codes 
# median wages of occupation: https://www.bls.gov/oes/current/oes_nat.htm#11-0000
#   Lots of non-matched (socp ending with X, but also others...)
#   Strategy: for socp in pums that don't match socp in bls 
#   truncate to first four digits and use overall occupation category code/wages
bls <- bls %>% 
  mutate(socp = as.character(soc_code))

per <- per %>% 
  left_join(bls, by = "socp")

sum(!is.na(per$socp)) # 7691 occupation codes
sum(!is.na(per$soc_code)) # 4341 occupation codes matched to bls data

per_socp_matched <- per %>% 
  filter(!is.na(soc_code))

per_socp_unmatched <- per %>% 
  filter(is.na(soc_code)) %>% 
  select(-c(names(bls)[1:11]))

bls_overall <- bls_overall %>% 
  mutate(socp = as.character(soc_code),
         socp2 = str_sub(socp, 1,2),
         soc_code = as.numeric(soc_code)) %>% 
  select(-socp)

per_socp_unmatched <- per_socp_unmatched %>% 
  mutate(socp2 = str_sub(socp, 1,2))

per_socp_unmatched <- per_socp_unmatched %>% 
  left_join(bls_overall, by = "socp2")

sum(!is.na(per_socp_unmatched$socp)) # 3350 occupation codes
sum(!is.na(per_socp_unmatched$soc_code)) # 3322 occupation codes matched to bls data

per_socp_unmatched %>% 
  filter(is.na(soc_code)) %>% 
  select(socp, socp2, soc_code, median_wage_bin) %>% 
  view()
# unmatched are military and unemployed
per_socp_unmatched %>% 
  select(esr, soc_code, socp2, median_wage_bin_label) %>% view()


per_bls <- bind_rows(per_socp_matched, per_socp_unmatched)

sum(!is.na(per_bls$socp)) # 7691 occupation codes
sum(!is.na(per_bls$soc_code)) # 7663 occupation codes matched to bls data

workers_by_medianwages <- per_bls %>% 
  filter(!is.na(below_ess_finc) & esr %in% c(1,2)) %>% 
  mutate(median_wage_bin_label = factor(median_wage_bin_label, 
                     levels = c("25610-37760", "37860-48320", "48340-60600",
                                "60640-86950", "88050-238990"))) %>% 
  group_by(below_ess_finc) %>% 
  count(median_wage_bin_label, wt = pwgtp, name = "count") %>% 
  group_by(below_ess_finc) %>% 
  mutate(percent = count/sum(count)*100)
# NOTE: this is not what the individuals represented earn
#       but the range for what the jobs they hold typically pay

write_csv(workers_by_medianwages, file = "data/workers_by_medianwages.csv")


# see also (future reference)
# https://www.urban.org/data-tools/black-women-precarious-gig-work?&utm_source=urban_ea&utm_campaign=unstable_work_black_women&utm_id=workforce&utm_content=general&engaged&utm_term=workforce
# https://www.urban.org/sites/default/files/2023-09/Job-quality-and-race-and-gender-equity.pdf

# Family by incomes ---- 
families_below_es_by_incomes <- hh %>% 
  filter(!is.na(below_ess_finc) & !is.na(fincp) & below_ess_finc == "Yes") %>% 
  mutate(income_bins = case_when(
    fincp < 10000 ~ "$0 to $9,999",
    fincp >= 10000 & fincp < 20000 ~ "$10,000 to $19,999",
    fincp >= 20000 & fincp < 30000 ~ "$20,000 to $29,999",
    fincp >= 30000 & fincp < 40000 ~ "$30,000 to $39,999",
    fincp >= 40000 & fincp < 50000 ~ "$40,000 to $49,999",
    fincp >= 50000  ~ "$50,000+",
  ),
  income_bins = factor(income_bins, 
                        levels = c("$0 to $9,999", "$10,000 to $19,999", "$20,000 to $29,999",
                                   "$30,000 to $39,999", "$40,000 to $49,999", "$50,000+"))) %>% 
  count(income_bins, wt = wgtp, name = "count") %>% 
  mutate(percent = count/sum(count)*100)

write_csv(families_below_es_by_incomes, file = "data/families_below_es_by_incomes.csv")


# Family Compositions ----
# among below income sufficiency
family_compositions_below_ess <- hh %>% 
  filter(below_ess_finc == "Yes") %>%
  mutate(
    num_adults = as.integer(str_extract(family_type, "(?<=a)\\d+")),
    num_infants = as.integer(str_extract(family_type, "(?<=i)\\d+")),
    num_preschoolers = as.integer(str_extract(family_type, "(?<=p)\\d+")),
    num_school_age = as.integer(str_extract(family_type, "(?<=s)\\d+")),
    num_teenagers = as.integer(str_extract(family_type, "(?<=t)\\d+")),
    num_other = as.integer(str_extract(family_type, "(?<=c)\\d+"))
  ) %>%
  replace_na(list(num_adults = 0, num_infants = 0, num_preschoolers = 0,
                  num_school_age = 0, num_teenagers = 0, num_other = 0)) %>%
  mutate(
    num_children = num_infants + num_preschoolers + num_school_age + num_teenagers + num_other
  ) %>% 
  group_by(num_adults, num_children) %>% 
  count(family_type, wt = wgtp, name = "count") %>% 
  arrange(desc(count)) 

write_csv(family_compositions_below_ess, file = "data/family_compositions_below_ess.csv")
