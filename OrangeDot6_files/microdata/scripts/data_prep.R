# Orange Dot 6.0 Microdata Analysis
# Identify families below economic self-sufficiency thresholds
# 2024-09-20 mpc
# Initial work done in conjunction with Public Interest Data Lab students
# Dylan, Sarah, Ander, Victoria, Sarah, Joshua, Sophia, Kayleigh, Sydnee


# Libraries ----
library(tidyverse)
library(janitor)

# Data ---- 
household <- read_csv("data/cvl_acspums_household.csv") %>% clean_names()
person <- read_csv("data/cvl_acspums_person.csv") %>% clean_names()
selfsuff <- read_csv("data/cvlregion_uwselfsufficiencystandards.csv")


# Prep self-sufficiency data ----
# Create Region-specific ESS across family types
#   includes A1-A3 and children by age up to 6 children
#   A1-A3 and children combined beyond 6 children
#   A4-A10 and children combined beyond 3 adults
region_ess <- selfsuff %>% 
  rowwise() %>% 
  mutate(median_sss = median(c_across(where(is.numeric)))) %>% 
  select(family_type, median_sss)

# # add number of adults and children by age group from the 'family_type' string as alternative
# region_ess <- region_ess %>%
#   mutate(
#     num_adults = as.integer(str_extract(family_type, "(?<=a)\\d+")),
#     num_infants = as.integer(str_extract(family_type, "(?<=i)\\d+")),
#     num_preschoolers = as.integer(str_extract(family_type, "(?<=p)\\d+")),
#     num_school_age = as.integer(str_extract(family_type, "(?<=s)\\d+")),
#     num_teenagers = as.integer(str_extract(family_type, "(?<=t)\\d+")),
#     num_other = as.integer(str_extract(family_type, "(?<=c)\\d+"))
#   ) %>%
#   replace_na(list(num_adults = 0, num_infants = 0, num_preschoolers = 0, 
#                   num_school_age = 0, num_teenagers = 0, num_other = 0)) %>%
#   mutate(
#     num_children = num_infants + num_preschoolers + num_school_age + num_teenagers + num_other
#   )


# Prep pums data ---- 
## Housing records ----
# # remove records that aren't housing units (institutional and group quarters)
# # remove records that are vacant housing units
household_reduced <- household %>% 
  filter(typehugq == 1, is.na(vacs)) 

## Person records ----
# remove person records that aren't related to household/reference person
# relationship is roommate (34), other nonrelative (36)
# theoretically, this should reduce the number of adults in household unit
person_reduced <- person %>% 
  filter(!(relshipp %in% c(34, 36))) 
# what does NA on relshipp mean?

# create family composition variable 
# to match self-sufficiency variable

# assign age group
person_reduced <- person_reduced %>% 
  mutate(age_type = case_when(agep <= 2 ~ 'i',
                              agep > 2 & agep <= 5 ~ 'p',
                              agep > 5 & agep <= 12 ~ 's',
                              agep > 12 & agep < 18 ~ 't',
                              agep >= 18 ~ 'a'
  ))  

# Create binary variables for each type 
person_reduced <- person_reduced %>% 
  mutate(i = case_when(age_type == 'i' ~ 1, TRUE ~ 0),
         p = case_when(age_type == 'p' ~ 1, TRUE ~ 0),
         a = case_when(age_type == 'a' ~ 1, TRUE ~ 0),
         s = case_when(age_type == 's' ~ 1, TRUE ~ 0),
         t = case_when(age_type == 't' ~ 1, TRUE ~ 0)
  ) 

# Collapse by serial number
collapsed_person_reduced <- person_reduced %>%
  group_by(serialno) %>% 
  summarize(a = sum(a),
            i = sum(i),
            p = sum(p),
            s = sum(s),
            t = sum(t)
  ) %>% 
  mutate(c = i+p+s+t, # for cases with more than 6 children
         # create family type conditional on # adults and # children
         family_type = case_when(
           a < 4 & c < 7 ~ paste0("a", a, "i", i, "p", p, "s", s, "t", t),
           a >= 4 ~ paste0("a", a, "c", c),
           c >= 7 ~ paste0("a", a, "c", c),
           a == 11 ~ paste0("a10", "c", c)
         )) %>% 
  select(serialno, family_type)

## Merge family type with household records ----
household_type <- household_reduced %>% 
  left_join(collapsed_person_reduced, by = "serialno")


## Merge self-sufficiency and pums data ---- 
household_type <- household_type %>% 
  left_join(region_ess, by = "family_type")

## Identify families/households below self-sufficiency ----
household_type <- household_type %>%
  mutate(
    income = ifelse(!is.na(fincp), fincp, hincp),
    below_ess_inc = case_when(
      income >= median_sss ~ "No",
      income < median_sss ~ "Yes",
      TRUE ~ NA_character_), 
    below_ess_finc = case_when(
      fincp >= median_sss ~ "No", 
      fincp < median_sss ~ "Yes",
      TRUE ~ NA_character_),
    below_35_finc = case_when(
      fincp >= 35000 ~ "No",
      fincp < 35000 ~ "Yes",
      TRUE ~ NA_character_)
    )

count(household_type, below_ess_inc)
count(household_type, below_ess_finc)
count(household_type, below_35_finc)

## Join below_ess to person records ----
person_type <- person_reduced %>% 
  left_join(household_type %>% 
              select(serialno, income, below_ess_inc, below_ess_finc, below_35_finc),
            by = "serialno")

count(person_type, below_ess_inc)
count(person_type, below_ess_finc)
count(person_type, below_35_finc)


# Save data ----
write_csv(household_type, file = "data/household_processed.csv")
write_csv(person_type, file = "data/person_processed.csv")
