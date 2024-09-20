####################################################
# ORANGE DOT REPROT
####################################################
# Acquire ACS data
# Last updated: 06/08/2022
# Metrics used in Orange dot report: 
# * Total population
# * Number and percent of families who fall into the following income bands (disaggregated by race):
#         * $0-9,999; $10,000-14,999; $15,000-24,999; $25,000-34,999
#         * Under $35K, $35-100K, Over $100K
# * Median HH Income for entire region and for each tract
# * Median family income for the entire region and for each tract
# * Percent and number of adults >25 years old without a HS diploma
# * Percent that receive public assistance/benefits (whole region) 
# * Percent of households with access to a vehicle (whole region)
# * Percent who rent and own their household
# * Percent interested in home ownership (whole region) -- HAVE NOT FOUND IN ACS
# * Percent with a criminal history (whole region) -- HAVE NOT FOUND IN ACS
#
# Based on: ACS 2020
# Pulling data for the following Localities: Albemarle (003), Buckingham (029), Fluvanna (065),
# Greene (079), Louisa (109), Nelson (125), and Charlottesville (540)

# Naming convention throughout: 
# columns that end in E are estimates -- some are counts, other are percents
# columns that begin with perc_ are percentages 
# columns that end in M are margins of error 
# columns with HHI are household income
# columns with Fam are family income

####################################################
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull data, reduce to variables of interest
# 3. Combine data 
# 4. Save
####################################################

# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables #########################

# Load libraries
library(tidyverse)
library(tidycensus)

# Census api key
# census_api_key("", instAll = TRUE, Overwrite = TRUE) # add key

# Variable view helper
# acs_var <- load_variables(2020, "acs5", cache = TRUE)

# Variables of interest -

# Pull by variables 
##  - Total population -- B01003_001
##  - Median HH Income -- B19013__001
##    B19013A_001 -- White alone householder
##    B19013B_001 -- Black alone householder
##  - Percent renting -- B25070_001/B25002_001 * 100
##  - Percent owner-occupied housing units -- B25003_002/B25002_001 * 100
##  - Number of households who receive cash public assistance/SNAP benefits -- B19058_002
##  - Less than high school diploma, population 25 and Over B06009_002
##  - Population 25 and Over B06009_001
##  - Percent households with access to a car -- B25044_002 (owners) + B25044_009 (renters) / B25044_001 (total) *100 
##  - Median family income -- B19119_001

# Pull by table 
## B19101 Total family income (number of families in different income bins)
    ## B19101A -- White alone
    ## B19101B -- Black alone

## ----------------------------------------------------------------------------------------------------

# ....................................................
# 2. Define localities, variables, pull tables #########################

# List of desired localities by FIPS
region <- c("003", "540", "065", "079", "109", "125", "029")

# 2. Get Data
# variables: define variable list
varlist_s = c("B01003_001", # total population
              "B19013_001", # median household income
              "B19013A_001", # median household income -- White householder
              "B19013B_001", # median household income -- Black householder
              "B25002_001", # total housing units
              "B25003_002", # owner-occupied housing units
              "B25070_001", # All renters
              "B19058_002", # receive snap 
              "B06009_002", # Less than HS diploma, population >=25
              "B06009_001", # Population >=25
              "B27011_008", # unemployed
              "B25044_002", # owner-occupied households with car access
              "B25044_009", # renter-occupied households with car access 
              "B19119_001") # median family income
              
## Pull variables listed above
tract_data_s <- get_acs(geography = "tract",
                        variables = varlist_s,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2020, 
                        output = "wide")

## Renaming variables 
names(tract_data_s) = c("GEOID", "NAME",
                        "totalpopE", "totalpopM",
                        "medianHHIE", "medianHHIM",
                        "medianHHIWhiteE", "medianHHIWhiteM",
                        "medianHHIBlackE", "medianHHIBlackM",
                        "AllhseE", "AllhseM",
                        "ownoccE", "ownoccM",
                        "rentAllE", "rentAllM",
                        "snapE", "snapM",
                        "noHSE", "noHSM",
                        "pop25olderE", "pop25olderM",
                        "unempE", "unempM",
                        "ownnerCarAccessE", "ownnerCarAccessM",
                        "renterCarAccessE", "renterCarAccessM",
                        "medianFamIncomeE", "medianFamIncomeM")

## Calculating percent of population 25 and older without HS diploma
tract_data_s <- tract_data_s %>% 
  mutate(perc_noHSE = round((noHSE/pop25olderE)*100,1),
         perc_noHSM = moe_prop(noHSE, pop25olderE, noHSM, pop25olderM),
         perc_noHSM = round(perc_noHSM*100, 1)) %>%
  dplyr::select(-pop25olderE, -pop25olderM)

## Pulling whole tables and then combing
## Calculating percentages and margins of error when applicable

####### Family income --------------

# Income bands in ACS key:
# _001 = total, _002 = <10k, _003 = 10-14,999k, _004 = 15-19,999k
# _005 = 20-24,999k, _006 = 25-29,999k, _007 = 30-34,999k, 
# _008 = 35-39,999k, _009 = 40-44,999k, _010 = 45-49,999k,
# _011 = 50-59,999k, _012 = 60-74,999k, _013 = 75-99,999k, 
# _014 = 100-124,999k, _015 = 125-149,999, _016 = 150-199,999k, 
# _017 = 200+k

# Income bands of interest for Orange Dot:
# $0-9,999; $10,000-14,999; $15,000-24,999; $25,000-34,999
# Under $35K, $35-100K, Over $100K

# Family income -- All families #########################
tract_AllFam <- get_acs(geography = "tract", 
                       table = "B19101", # pulls income data for all families 
                       state = "VA", 
                       county = region, 
                       survey = "acs5",
                       year = 2020)

# total All families 
AllFamT <- tract_AllFam %>% 
  filter(variable == "B19101_001") %>% 
  group_by(GEOID, NAME) %>% 
  rename(AllFamE = estimate,
         AllFamM = moe) %>% 
  dplyr::select(-variable)

# All families making <10k
tract_AllFamUnder10k <- tract_AllFam %>% 
  filter(variable == "B19101_002") %>% 
  rename(AllFamUnder10kE = estimate,
         AllFamUnder10kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_AllFamUnder10kE = round(AllFamUnder10kE / AllFamT$AllFamE * 100, 2),
         perc_AllFamUnder10kM = round(moe_prop(AllFamUnder10kE, AllFamT$AllFamE, 
                                              AllFamUnder10kM, AllFamT$AllFamM), 2))

# All families making 10-14,999k
tract_AllFamBtw10_14k <- tract_AllFam %>% 
  filter(variable == "B19101_003") %>% 
  rename(AllFamBtw10_14kE = estimate,
         AllFamBtw10_14kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_AllFamBtw10_14kE = round(AllFamBtw10_14kE / AllFamT$AllFamE * 100, 2),
         perc_AllFamBtw10_14kM = round(moe_prop(AllFamBtw10_14kE, AllFamT$AllFamE, 
                                            AllFamBtw10_14kM, AllFamT$AllFamM), 2))

# All families making 15-24,999k
tract_AllFamBtw15_24k <- tract_AllFam %>% 
  filter(variable %in% c("B19101_004", "B19101_005")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(AllFamBtw15_24kE = sum(estimate),
            AllFamBtw15_24kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_AllFamBtw15_24k$perc_AllFamBtw15_24kE <- round(tract_AllFamBtw15_24k$AllFamBtw15_24kE / AllFamT$AllFamE * 100, 2)
tract_AllFamBtw15_24k$perc_AllFamBtw15_24kM <- round(moe_prop(tract_AllFamBtw15_24k$AllFamBtw15_24kE, AllFamT$AllFamE, 
                                                      tract_AllFamBtw15_24k$AllFamBtw15_24kM, AllFamT$AllFamM), 2)

# All families making 25-34,999k
tract_AllFamBtw25_34k <- tract_AllFam %>% 
  filter(variable %in% c("B19101_006", "B19101_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(AllFamBtw25_34kE = sum(estimate),
            AllFamBtw25_34kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_AllFamBtw25_34k$perc_AllFamBtw25_34kE <- round(tract_AllFamBtw25_34k$AllFamBtw25_34kE / AllFamT$AllFamE * 100, 2)
tract_AllFamBtw25_34k$perc_AllFamBtw25_34kM <- round(moe_prop(tract_AllFamBtw25_34k$AllFamBtw25_34kE, AllFamT$AllFamE, 
                                                      tract_AllFamBtw25_34k$AllFamBtw25_34kM, AllFamT$AllFamM), 2)

# All families making Under 35k
tract_AllFamUnder35k <- tract_AllFam %>% 
  filter(variable %in% c("B19101_002", "B19101_003", "B19101_004",
                         "B19101_005", "B19101_006", "B19101_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(AllFamUnder35kE = sum(estimate),
            AllFamUnder35kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_AllFamUnder35k$perc_AllFamUnder35kE <- round(tract_AllFamUnder35k$AllFamUnder35kE / AllFamT$AllFamE * 100, 2)
tract_AllFamUnder35k$perc_AllFamUnder35kM <- round(moe_prop(tract_AllFamUnder35k$AllFamUnder35kE, AllFamT$AllFamE, 
                                                          tract_AllFamUnder35k$AllFamUnder35kM, AllFamT$AllFamM), 2)
# All families making 35-59,999k 
tract_AllFamBtw35_59k <- tract_AllFam %>% 
  filter(variable %in% c("B19101_008", "B19101_009", "B19101_010",
                         "B19101_011")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(AllFamBtw35_59kE = sum(estimate),
            AllFamBtw35_59kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_AllFamBtw35_59k$perc_AllFamBtw35_59kE <- round(tract_AllFamBtw35_59k$AllFamBtw35_59kE / AllFamT$AllFamE * 100, 2)
tract_AllFamBtw35_59k$perc_AllFamBtw35_59kM <- round(moe_prop(tract_AllFamBtw35_59k$AllFamBtw35_59kE, AllFamT$AllFamE, 
                                                              tract_AllFamBtw35_59k$AllFamBtw35_59kM, AllFamT$AllFamM), 2)

# All families making 60-100k 
tract_AllFamBtw60_100k <- tract_AllFam %>% 
  filter(variable %in% c("B19101_012", "B19101_013")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(AllFamBtw60_100kE = sum(estimate),
            AllFamBtw60_100kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_AllFamBtw60_100k$perc_AllFamBtw60_100kE <- round(tract_AllFamBtw60_100k$AllFamBtw60_100kE / AllFamT$AllFamE * 100, 2)
tract_AllFamBtw60_100k$perc_AllFamBtw60_100kM <- round(moe_prop(tract_AllFamBtw60_100k$AllFamBtw60_100kE, AllFamT$AllFamE, 
                                                              tract_AllFamBtw60_100k$AllFamBtw60_100kM, AllFamT$AllFamM), 2)

# All families making Over 100k 
tract_AllFamOver100 <- tract_AllFam %>% 
  filter(variable %in% c("B19101_014", "B19101_015", "B19101_016",
                         "B19101_017")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(AllFamOver100kE = sum(estimate),
            AllFamOver100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_AllFamOver100$perc_AllFamOver100kE <- round(tract_AllFamOver100$AllFamOver100kE / AllFamT$AllFamE * 100, 2)
tract_AllFamOver100$perc_AllFamOver100kM <- round(moe_prop(tract_AllFamOver100$AllFamOver100kE, AllFamT$AllFamE, 
                                                         tract_AllFamOver100$AllFamOver100kM, AllFamT$AllFamM), 2)




# Family income -- White families  #########################
tract_White <- get_acs(geography = "tract", 
                       table = "B19101A", # pulls income data for all white families 
                       state = "VA", 
                       county = region, 
                       survey = "acs5",
                       year = 2020)

# total White families 
tract_WhiteAll <- tract_White %>% 
  filter(variable == "B19101A_001") %>% 
  group_by(GEOID, NAME) %>% 
  rename(AllWhiteFamE = estimate,
            AllWhiteFamM = moe) %>% 
  dplyr::select(-variable)

# White families making <10k
tract_WhiteFamUnder10k <- tract_White %>% 
  filter(variable == "B19101A_002") %>% 
  rename(WhiteFamUnder10kE = estimate,
         WhiteFamUnder10kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_WhiteFamUnder10kE = round(WhiteFamUnder10kE / tract_WhiteAll$AllWhiteFamE * 100, 2),
         perc_WhiteFamUnder10kM = round(moe_prop(WhiteFamUnder10kE, tract_WhiteAll$AllWhiteFamE, 
                                              WhiteFamUnder10kM, tract_WhiteAll$AllWhiteFamM), 2))

# White families making 10-14,999k
tract_WhiteFamBtw10_14k <- tract_White %>% 
  filter(variable == "B19101A_003") %>% 
  rename(WhiteFamBtw10_14kE = estimate,
         WhiteFamBtw10_14kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_WhiteFamBtw10_14kE = round(WhiteFamBtw10_14kE / tract_WhiteAll$AllWhiteFamE * 100, 2),
         perc_WhiteFamBtw10_14kM = round(moe_prop(WhiteFamBtw10_14kE, tract_WhiteAll$AllWhiteFamE, 
                                              WhiteFamBtw10_14kM, tract_WhiteAll$AllWhiteFamM), 2))

# White families making 15-24,999k
tract_WhiteFamBtw15_24k <- tract_White %>% 
  filter(variable %in% c("B19101A_004", "B19101A_005")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(WhiteFamBtw15_24kE = sum(estimate),
            WhiteFamBtw15_24kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_WhiteFamBtw15_24k$perc_WhiteFamBtw15_24kE <- round(tract_WhiteFamBtw15_24k$WhiteFamBtw15_24kE / tract_WhiteAll$AllWhiteFamE * 100, 2)
tract_WhiteFamBtw15_24k$perc_WhiteFamBtw15_24kM <- round(moe_prop(tract_WhiteFamBtw15_24k$WhiteFamBtw15_24kE, tract_WhiteAll$AllWhiteFamE, 
                                                      tract_WhiteFamBtw15_24k$WhiteFamBtw15_24kM, tract_WhiteAll$AllWhiteFamM), 2)

# White families making 25-34,999k
tract_WhiteFamBtw25_34k <- tract_White %>% 
  filter(variable %in% c("B19101A_006", "B19101A_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(WhiteFamBtw25_34kE = sum(estimate),
            WhiteFamBtw25_34kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_WhiteFamBtw25_34k$perc_WhiteFamBtw25_34kM <- round(tract_WhiteFamBtw25_34k$WhiteFamBtw25_34kE / tract_WhiteAll$AllWhiteFamE * 100, 2)
tract_WhiteFamBtw25_34k$perc_WhiteFamBtw25_34kM <- round(moe_prop(tract_WhiteFamBtw25_34k$WhiteFamBtw25_34kE, tract_WhiteAll$AllWhiteFamE, 
                                                      tract_WhiteFamBtw25_34k$WhiteFamBtw25_34kM, tract_WhiteAll$AllWhiteFamM), 2)

# White families making Under 35k
tract_WhiteFamUnder35k <- tract_White %>% 
  filter(variable %in% c("B19101A_002", "B19101A_003", "B19101A_004",
                         "B19101A_005", "B19101A_006", "B19101A_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(WhiteFamUnder35kE = sum(estimate),
            WhiteFamUnder35kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_WhiteFamUnder35k$perc_WhiteFamUnder35kE <- round(tract_WhiteFamUnder35k$WhiteFamUnder35kE / tract_WhiteAll$AllWhiteFamE * 100, 2)
tract_WhiteFamUnder35k$perc_WhiteFamUnder35kM <- round(moe_prop(tract_WhiteFamUnder35k$WhiteFamUnder35kE, tract_WhiteAll$AllWhiteFamE, 
                                                          tract_WhiteFamUnder35k$WhiteFamUnder35kM, tract_WhiteAll$AllWhiteFamM), 2)
# White families making 35-59,999k 
tract_WhiteFamBtw35_59k <- tract_White %>% 
  filter(variable %in% c("B19101A_008", "B19101A_009", "B19101A_010",
                         "B19101A_011")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(WhiteFamBtw35_59kE = sum(estimate),
            WhiteFamBtw35_59kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_WhiteFamBtw35_59k$perc_WhiteFamBtw35_59kE <- round(tract_WhiteFamBtw35_59k$WhiteFamBtw35_59kE / tract_WhiteAll$AllWhiteFamE * 100, 2)
tract_WhiteFamBtw35_59k$perc_WhiteFamBtw35_59kM <- round(moe_prop(tract_WhiteFamBtw35_59k$WhiteFamBtw35_59kE, tract_WhiteAll$AllWhiteFamE, 
                                                              tract_WhiteFamBtw35_59k$WhiteFamBtw35_59kM, tract_WhiteAll$AllWhiteFamM), 2)

# White families making 60-100k 
tract_WhiteFamBtw60_100k <- tract_White %>% 
  filter(variable %in% c("B19101A_012", "B19101A_013")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(WhiteFamBtw60_100kE = sum(estimate),
            WhiteFamBtw60_100kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_WhiteFamBtw60_100k$perc_WhiteFamBtw60_100kE <- round(tract_WhiteFamBtw60_100k$WhiteFamBtw60_100kE / tract_WhiteAll$AllWhiteFamE * 100, 2)
tract_WhiteFamBtw60_100k$perc_WhiteFamBtw60_100kM <- round(moe_prop(tract_WhiteFamBtw60_100k$WhiteFamBtw60_100kE, tract_WhiteAll$AllWhiteFamE, 
                                                                tract_WhiteFamBtw60_100k$WhiteFamBtw60_100kM, tract_WhiteAll$AllWhiteFamM), 2)

# White families making Over 100k 
tract_WhiteFamOver100 <- tract_White %>% 
  filter(variable %in% c("B19101A_014", "B19101A_015", "B19101A_016",
                         "B19101A_017")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(WhiteFamOver100kE = sum(estimate),
            WhiteFamOver100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_WhiteFamOver100$perc_WhiteFamOver100kE <- round(tract_WhiteFamOver100$WhiteFamOver100kE / tract_WhiteAll$AllWhiteFamE * 100, 2)
tract_WhiteFamOver100$perc_WhiteFamOver100kM <- round(moe_prop(tract_WhiteFamOver100$WhiteFamOver100kE, tract_WhiteAll$AllWhiteFamE, 
                                                        tract_WhiteFamOver100$WhiteFamOver100kM, tract_WhiteAll$AllWhiteFamM), 2)




# Family income -- Black families  #########################
tract_Black <- get_acs(geography = "tract", 
                            table = "B19101B", # pulls income data for all Black families 
                            state = "VA", 
                            county = region, 
                            survey = "acs5",
                            year = 2020)

# total Black families 
tract_BlackAll <- tract_Black %>% 
  filter(variable == "B19101B_001") %>% 
  group_by(GEOID, NAME) %>% 
  rename(AllBlackFamE = estimate,
         AllBlackFamM = moe) %>% 
  dplyr::select(-variable)

# Black families making <10k
tract_BlackFamUnder10k <- tract_Black %>% 
  filter(variable == "B19101B_002") %>% 
  rename(BlackFamUnder10kE = estimate,
         BlackFamUnder10kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_BlackFamUnder10kE = round(BlackFamUnder10kE / tract_BlackAll$AllBlackFamE * 100, 2),
         perc_BlackFamUnder10kM = round(moe_prop(BlackFamUnder10kE, tract_BlackAll$AllBlackFamE, 
                                              BlackFamUnder10kM, tract_BlackAll$AllBlackFamM), 2))

# Black families making 10-14,999k
tract_BlackFamBtw10_14k <- tract_Black %>% 
  filter(variable == "B19101B_003") %>% 
  rename(BlackFamBtw10_14kE = estimate,
         BlackFamBtw10_14kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_BlackFamBtw10_14kE = round(BlackFamBtw10_14kE / tract_BlackAll$AllBlackFamE * 100, 2),
         perc_BlackFamBtw10_14kM = round(moe_prop(BlackFamBtw10_14kE, tract_BlackAll$AllBlackFamE, 
                                            BlackFamBtw10_14kM, tract_BlackAll$AllBlackFamM), 2))

# Black families making 15-24,999k
tract_BlackFamBtw15_24k <- tract_Black %>% 
  filter(variable %in% c("B19101B_004", "B19101B_005")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(BlackFamBtw15_24kE = sum(estimate),
            BlackFamBtw15_24kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_BlackFamBtw15_24k$perc_BlackFamBtw15_24kE <- round(tract_BlackFamBtw15_24k$BlackFamBtw15_24kE / tract_BlackAll$AllBlackFamE * 100, 2)
tract_BlackFamBtw15_24k$perc_BlackFamBtw15_24kM <- round(moe_prop(tract_BlackFamBtw15_24k$BlackFamBtw15_24kE, tract_BlackAll$AllBlackFamE, 
                                                      tract_BlackFamBtw15_24k$BlackFamBtw15_24kM, tract_BlackAll$AllBlackFamM), 2)

# Black families making 25-34,999k
tract_BlackFamBtw25_34k <- tract_Black %>% 
  filter(variable %in% c("B19101B_006", "B19101B_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(BlackFamBtw25_34kE = sum(estimate),
            BlackFamBtw25_34kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_BlackFamBtw25_34k$perc_BlackFamBtw25_34kE <- round(tract_BlackFamBtw25_34k$BlackFamBtw25_34kE / tract_BlackAll$AllBlackFamE * 100, 2)
tract_BlackFamBtw25_34k$perc_BlackFamBtw25_34kM <- round(moe_prop(tract_BlackFamBtw25_34k$BlackFamBtw25_34kE, tract_BlackAll$AllBlackFamE, 
                                                      tract_BlackFamBtw25_34k$BlackFamBtw25_34kM, tract_BlackAll$AllBlackFamM), 2)

# Black families making Under 35k
tract_BlackFamUnder35k <- tract_Black %>% 
  filter(variable %in% c("B19101B_002", "B19101B_003", "B19101B_004",
                         "B19101B_005", "B19101B_006", "B19101B_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(BlackFamUnder35kE = sum(estimate),
            BlackFamUnder35kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_BlackFamUnder35k$perc_BlackFamUnder35kE <- round(tract_BlackFamUnder35k$BlackFamUnder35kE / tract_BlackAll$AllBlackFamE * 100, 2)
tract_BlackFamUnder35k$perc_BlackFamUnder35kM <- round(moe_prop(tract_BlackFamUnder35k$BlackFamUnder35kE, tract_BlackAll$AllBlackFamE, 
                                                          tract_BlackFamUnder35k$BlackFamUnder35kM, tract_BlackAll$AllBlackFamM), 2)
# Black families making 35-59,999k 
tract_BlackFamBtw35_59k <- tract_Black %>% 
  filter(variable %in% c("B19101B_008", "B19101B_009", "B19101B_010",
                         "B19101B_011")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(BlackFamBtw35_59kE = sum(estimate),
            BlackFamBtw35_59kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_BlackFamBtw35_59k$perc_BlackFamBtw35_59kE <- round(tract_BlackFamBtw35_59k$BlackFamBtw35_59kE / tract_BlackAll$AllBlackFamE * 100, 2)
tract_BlackFamBtw35_59k$perc_BlackFamBtw35_59kM <- round(moe_prop(tract_BlackFamBtw35_59k$BlackFamBtw35_59kE, tract_BlackAll$AllBlackFamE, 
                                                                  tract_BlackFamBtw35_59k$BlackFamBtw35_59kM, tract_BlackAll$AllBlackFamM), 2)

# Black families making 60-100k 
tract_BlackFamBtw60_100k <- tract_Black %>% 
  filter(variable %in% c("B19101B_012", "B19101B_013")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(BlackFamBtw60_100kE = sum(estimate),
            BlackFamBtw60_100kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_BlackFamBtw60_100k$perc_BlackFamBtw60_100kE <- round(tract_BlackFamBtw60_100k$BlackFamBtw60_100kE / tract_BlackAll$AllBlackFamE * 100, 2)
tract_BlackFamBtw60_100k$perc_BlackFamBtw60_100kM <- round(moe_prop(tract_BlackFamBtw60_100k$BlackFamBtw60_100kE, tract_BlackAll$AllBlackFamE, 
                                                                    tract_BlackFamBtw60_100k$BlackFamBtw60_100kM, tract_BlackAll$AllBlackFamM), 2)

# Black families making Over 100k 
tract_BlackFamOver100 <- tract_Black %>% 
  filter(variable %in% c("B19101B_014", "B19101B_015", "B19101B_016",
                         "B19101B_017")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(BlackFamOver100kE = sum(estimate),
            BlackFamOver100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_BlackFamOver100$perc_BlackFamOver100kE <- round(tract_BlackFamOver100$BlackFamOver100kE / tract_BlackAll$AllBlackFamE * 100, 2)
tract_BlackFamOver100$perc_BlackFamOver100kM <- round(moe_prop(tract_BlackFamOver100$BlackFamOver100kE, tract_BlackAll$AllBlackFamE, 
                                                         tract_BlackFamOver100$BlackFamOver100kM, tract_BlackAll$AllBlackFamM), 2)

# ....................................................
# 3. Combining All data frames #########################

tract_data <- tract_data_s %>%
  left_join(AllFamT) %>%
  left_join(tract_AllFamUnder10k) %>%
  left_join(tract_AllFamBtw10_14k) %>%
  left_join(tract_AllFamBtw15_24k) %>%
  left_join(tract_AllFamBtw25_34k) %>%
  left_join(tract_AllFamUnder35k) %>%
  left_join(tract_AllFamBtw35_59k) %>%
  left_join(tract_AllFamBtw60_100k) %>%
  left_join(tract_AllFamOver100) %>%
  left_join(tract_WhiteAll) %>%
  left_join(tract_WhiteFamUnder10k) %>%
  left_join(tract_WhiteFamBtw10_14k) %>%
  left_join(tract_WhiteFamBtw15_24k) %>%
  left_join(tract_WhiteFamBtw25_34k) %>%
  left_join(tract_WhiteFamUnder35k) %>%
  left_join(tract_WhiteFamBtw35_59k) %>%
  left_join(tract_WhiteFamBtw60_100k) %>%
  left_join(tract_WhiteFamOver100) %>%
  left_join(tract_BlackAll) %>%
  left_join(tract_BlackFamUnder10k) %>%
  left_join(tract_BlackFamBtw10_14k) %>%
  left_join(tract_BlackFamBtw15_24k) %>%
  left_join(tract_BlackFamBtw25_34k) %>%
  left_join(tract_BlackFamUnder35k) %>%
  left_join(tract_BlackFamBtw35_59k) %>%
  left_join(tract_BlackFamBtw60_100k) %>%
  left_join(tract_BlackFamOver100)
  

tract_data <- tract_data %>% 
  mutate(year = "2020") 

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

tract_data$locality <- as.character(tract_data$locality)

tract_data$countyName <- recode(tract_data$locality, "540" = "Charlottesville", "029" = "Buckingham", "065" = "Fluvanna",
         "003" = "Albemarle", "109" = "Louisa", "125" = "Nelson", "079" = "Greene")

# ....................................................
# 4. Save #########################
write.csv(tract_data, "acs_tract_orangedot.csv", row.names = F) 










