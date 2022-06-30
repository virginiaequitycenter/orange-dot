####################################################
# ORANGE DOT REPROT
####################################################
# Acquire ACS data
# Last updated: 05/25/2022
# Metrics used in Orange dot report: 
# * Total population
# * Number and percent of families who fall into the following income bands (disaggregated by race):
#         * $0-9,999; $10,000-14,999; $15,000-24,999; $25,000-34,999
#         * under $35K, $35-100K, over $100K
# * Median HH Income for entire region and for each tract
# * Percent and number of adults >25 years old without a HS diploma
# * Percent of families making < $35,000 disaggregated by race
# * Percent with a criminal history (whole region) -- HAVE NOT FOUND IN ACS
# * Percent that receive public assistance/benefits (whole region) 
# * Percent with access to a vehicle (whole region) -- HAVE NOT FOUND IN ACS
# * Percent who rent, are homeless, live with family/friends, own (whole region)
# * Percent interested in home ownership (whole region) -- HAVE NOT FOUND IN ACS
#
# Based on: ACS 2020
# Pulling data for the following Localities: Albemarle (003), Buckingham (029), Fluvanna (065),
# Greene (079), Louisa (109), Nelson (125), and Charlottesville (540)

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
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
# acs_var <- load_variables(2020, "acs5", cache = TRUE)

# Variables of interest -

# Pull by variables 
##  - Total population -- B01003_001
##  - Median HH Income -- B19013__001
##    B19013A_001 -- White alone householder
##    B19013B_001 -- Black alone householder
##    B19013G_001 -- Two or more races householder
##    B19013I_001 -- Hispanic or Latino householder
##  - Percent renting -- B25070_001/B25002_001 * 100
##  - Percent home owners -- B25003_002/B25002_001 * 100
##  - Number of households who receive cash public assistance/SNAP benefits -- B19058_002
##  - Less than high school diploma, population 25 and over B06009_002

# Pull by table 
## B19101 Total family income (number of families in different income bins)
    ## B19101A -- White alone
    ## B19101B -- Black alone
    ## B19101G -- Two or more races 
    ## B19101I -- Hispanic or Latino

## Notes that will likely get deleted:--------------------------------------------------
## B19001 for all the household incomes by race, in case interested in that instead of family
## S1903 MEDIAN INCOME IN THE PAST 12 MONTHS (by race)
## current orange dot report has family income by race 
## Median household income -- B19013_001

# Micro data ?
## FINCP -- Family income
## HINCP -- Household income
## JWTRNS -- Means of transportation to work 
## ----------------------------------------------------------------------------------------------------

# ....................................................
# 2. Define localities, variables, pull tables #########################

# List of desired localities by FIPS
region <- c("003", "540", "065", "079", "109", "125", "029")

# 2. Get Data
# variables: define variable list
varlist_s = c("B01003_001", # total population
              "B19013_001", # median household income
              "B19013A_001", # median household income -- white householder
              "B19013B_001", # median household income -- black householder
              "B25002_001", # total housing units
              "B25003_002", # owner-occupied housing units
              "B25070_001", # all renters
              "B19058_002", # receive snap 
              "B06009_002", # Less than HS diploma, population >=25
              "B27011_008") # unemployed

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
                        "medianHHIwhiteE", "medianHHIwhiteM",
                        "medianHHIblackE", "medianHHIblackM",
                        "allhseE", "allhseM",
                        "ownoccE", "ownoccM",
                        "rentallE", "rentallM",
                        "snapE", "snapM",
                        "noHSE", "noHSM",
                        "unempE", "unempM")

## Pulling whole tables and then combing

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
# under $35K, $35-100K, over $100K

# Family income -- all families #########################
tract_hhincall <- get_acs(geography = "tract", 
                       table = "B19101", 
                       state = "VA", 
                       county = region, 
                       survey = "acs5",
                       year = 2020)

# total all families 
hhincallT <- tract_hhincall %>% 
  filter(variable == "B19101_001") %>% 
  group_by(GEOID, NAME) %>% 
  rename(allFamE = estimate,
         allFamM = moe) %>% 
  dplyr::select(-variable)

# all families making <10k
tract_allunder10k <- tract_hhincall %>% 
  filter(variable == "B19101_002") %>% 
  rename(allunder10kE = estimate,
         allunder10kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_allunder10kE = round(allunder10kE / hhincallT$allFamE * 100, 2),
         perc_allunder10kM = round(moe_prop(allunder10kE, hhincallT$allFamE, 
                                              allunder10kM, hhincallT$allFamM), 2))

# all families making 10-14,999k
tract_all10_14k <- tract_hhincall %>% 
  filter(variable == "B19101_003") %>% 
  rename(all10_14kE = estimate,
         all10_14kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_all10_14kE = round(all10_14kE / hhincallT$allFamE * 100, 2),
         perc_all10_14kM = round(moe_prop(all10_14kE, hhincallT$allFamE, 
                                            all10_14kM, hhincallT$allFamM), 2))

# all families making 15-24,999k
tract_all15_24k <- tract_hhincall %>% 
  filter(variable %in% c("B19101_004", "B19101_005")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(all15_24kE = sum(estimate),
            all15_24kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_all15_24k$perc_all15_24kE <- round(tract_all15_24k$all15_24kE / hhincallT$allFamE * 100, 2)
tract_all15_24k$perc_all15_24kM <- round(moe_prop(tract_all15_24k$all15_24kE, hhincallT$allFamE, 
                                                      tract_all15_24k$all15_24kM, hhincallT$allFamM), 2)

# all families making 25-34,999k
tract_all25_34k <- tract_hhincall %>% 
  filter(variable %in% c("B19101_006", "B19101_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(all25_34kE = sum(estimate),
            all25_34kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_all25_34k$perc_all25_34kE <- round(tract_all25_34k$all25_34kE / hhincallT$allFamE * 100, 2)
tract_all25_34k$perc_all25_34kM <- round(moe_prop(tract_all25_34k$all25_34kE, hhincallT$allFamE, 
                                                      tract_all25_34k$all25_34kM, hhincallT$allFamM), 2)

# all families making under 35k
tract_allunder35k <- tract_hhincall %>% 
  filter(variable %in% c("B19101_002", "B19101_003", "B19101_004",
                         "B19101_005", "B19101_006", "B19101_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(allunder35kE = sum(estimate),
            allunder35kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_allunder35k$perc_allunder35kE <- round(tract_allunder35k$allunder35kE / hhincallT$allFamE * 100, 2)
tract_allunder35k$perc_allunder35kM <- round(moe_prop(tract_allunder35k$allunder35kE, hhincallT$allFamE, 
                                                          tract_allunder35k$allunder35kM, hhincallT$allFamM), 2)
# all families making 35-100k 
tract_all35_100 <- tract_hhincall %>% 
  filter(variable %in% c("B19101_008", "B19101_009", "B19101_010",
                         "B19101_011", "B19101_012", "B19101_013")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(all35_100kE = sum(estimate),
            all35_100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_all35_100$perc_all35_100kE <- round(tract_all35_100$all35_100kE / hhincallT$allFamE * 100, 2)
tract_all35_100$perc_all35_100kM <- round(moe_prop(tract_all35_100$all35_100kE, hhincallT$allFamE, 
                                                       tract_all35_100$all35_100kM, hhincallT$allFamM), 2)

# all families making over 100k 
tract_allover100 <- tract_hhincall %>% 
  filter(variable %in% c("B19101_014", "B19101_015", "B19101_016",
                         "B19101_017")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(allover100kE = sum(estimate),
            allover100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_allover100$perc_allover100kE <- round(tract_allover100$allover100kE / hhincallT$allFamE * 100, 2)
tract_allover100$perc_allover100kM <- round(moe_prop(tract_allover100$allover100kE, hhincallT$allFamE, 
                                                         tract_allover100$allover100kM, hhincallT$allFamM), 2)




# Family income -- white families  #########################
tract_hhincwhite <- get_acs(geography = "tract", 
                       table = "B19101A", 
                       state = "VA", 
                       county = region, 
                       survey = "acs5",
                       year = 2020)

# total white families 
tract_hhincwhiteall <- tract_hhincwhite %>% 
  filter(variable == "B19101A_001") %>% 
  group_by(GEOID, NAME) %>% 
  rename(allwhiteFamE = estimate,
            allwhiteFamM = moe) %>% 
  dplyr::select(-variable)

# white families making <10k
tract_whiteunder10k <- tract_hhincwhite %>% 
  filter(variable == "B19101A_002") %>% 
  rename(whiteunder10kE = estimate,
         whiteunder10kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_whiteunder10kE = round(whiteunder10kE / tract_hhincwhiteall$allwhiteFamE * 100, 2),
         perc_whiteunder10kM = round(moe_prop(whiteunder10kE, tract_hhincwhiteall$allwhiteFamE, 
                                              whiteunder10kM, tract_hhincwhiteall$allwhiteFamM), 2))

# white families making 10-14,999k
tract_white10_14k <- tract_hhincwhite %>% 
  filter(variable == "B19101A_003") %>% 
  rename(white10_14kE = estimate,
         white10_14kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_white10_14kE = round(white10_14kE / tract_hhincwhiteall$allwhiteFamE * 100, 2),
         perc_white10_14kM = round(moe_prop(white10_14kE, tract_hhincwhiteall$allwhiteFamE, 
                                              white10_14kM, tract_hhincwhiteall$allwhiteFamM), 2))

# white families making 15-24,999k
tract_white15_24k <- tract_hhincwhite %>% 
  filter(variable %in% c("B19101A_004", "B19101A_005")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(white15_24kE = sum(estimate),
            white15_24kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_white15_24k$perc_white15_24kE <- round(tract_white15_24k$white15_24kE / tract_hhincwhiteall$allwhiteFamE * 100, 2)
tract_white15_24k$perc_white15_24kM <- round(moe_prop(tract_white15_24k$white15_24kE, tract_hhincwhiteall$allwhiteFamE, 
                                                      tract_white15_24k$white15_24kM, tract_hhincwhiteall$allwhiteFamM), 2)

# white families making 25-34,999k
tract_white25_34k <- tract_hhincwhite %>% 
  filter(variable %in% c("B19101A_006", "B19101A_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(white25_34kE = sum(estimate),
            white25_34kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_white25_34k$perc_white25_34kE <- round(tract_white25_34k$white25_34kE / tract_hhincwhiteall$allwhiteFamE * 100, 2)
tract_white25_34k$perc_white25_34kM <- round(moe_prop(tract_white25_34k$white25_34kE, tract_hhincwhiteall$allwhiteFamE, 
                                                      tract_white25_34k$white25_34kM, tract_hhincwhiteall$allwhiteFamM), 2)

# white families making under 35k
tract_whiteunder35k <- tract_hhincwhite %>% 
  filter(variable %in% c("B19101A_002", "B19101A_003", "B19101A_004",
                         "B19101A_005", "B19101A_006", "B19101A_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(whiteunder35kE = sum(estimate),
            whiteunder35kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_whiteunder35k$perc_whiteunder35kE <- round(tract_whiteunder35k$whiteunder35kE / tract_hhincwhiteall$allwhiteFamE * 100, 2)
tract_whiteunder35k$perc_whiteunder35kM <- round(moe_prop(tract_whiteunder35k$whiteunder35kE, tract_hhincwhiteall$allwhiteFamE, 
                                                          tract_whiteunder35k$whiteunder35kM, tract_hhincwhiteall$allwhiteFamM), 2)
# white families making 35-100k 
tract_white35_100 <- tract_hhincwhite %>% 
  filter(variable %in% c("B19101A_008", "B19101A_009", "B19101A_010",
                         "B19101A_011", "B19101A_012", "B19101A_013")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(white35_100kE = sum(estimate),
            white35_100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_white35_100$perc_white35_100kE <- round(tract_white35_100$white35_100kE / tract_hhincwhiteall$allwhiteFamE * 100, 2)
tract_white35_100$perc_white35_100kM <- round(moe_prop(tract_white35_100$white35_100kE, tract_hhincwhiteall$allwhiteFamE, 
                                                       tract_white35_100$white35_100kM, tract_hhincwhiteall$allwhiteFamM), 2)

# white families making over 100k 
tract_whiteover100 <- tract_hhincwhite %>% 
  filter(variable %in% c("B19101A_014", "B19101A_015", "B19101A_016",
                         "B19101A_017")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(whiteover100kE = sum(estimate),
            whiteover100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_whiteover100$perc_whiteover100kE <- round(tract_whiteover100$whiteover100kE / tract_hhincwhiteall$allwhiteFamE * 100, 2)
tract_whiteover100$perc_whiteover100kM <- round(moe_prop(tract_whiteover100$whiteover100kE, tract_hhincwhiteall$allwhiteFamE, 
                                                        tract_whiteover100$whiteover100kM, tract_hhincwhiteall$allwhiteFamM), 2)




# Family income -- black families  #########################
tract_hhincblack <- get_acs(geography = "tract", 
                            table = "B19101B", 
                            state = "VA", 
                            county = region, 
                            survey = "acs5",
                            year = 2020)

# total black families 
tract_hhincblackall <- tract_hhincblack %>% 
  filter(variable == "B19101B_001") %>% 
  group_by(GEOID, NAME) %>% 
  rename(allblackFamE = estimate,
         allblackFamM = moe) %>% 
  dplyr::select(-variable)

# black families making <10k
tract_blackunder10k <- tract_hhincblack %>% 
  filter(variable == "B19101B_002") %>% 
  rename(blackunder10kE = estimate,
         blackunder10kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_blackunder10kE = round(blackunder10kE / tract_hhincblackall$allblackFamE * 100, 2),
         perc_blackunder10kM = round(moe_prop(blackunder10kE, tract_hhincblackall$allblackFamE, 
                                              blackunder10kM, tract_hhincblackall$allblackFamM), 2))

# black families making 10-14,999k
tract_black10_14k <- tract_hhincblack %>% 
  filter(variable == "B19101B_003") %>% 
  rename(black10_14kE = estimate,
         black10_14kM = moe) %>% 
  dplyr::select(-variable) %>%
  mutate(perc_black10_14kE = round(black10_14kE / tract_hhincblackall$allblackFamE * 100, 2),
         perc_black10_14kM = round(moe_prop(black10_14kE, tract_hhincblackall$allblackFamE, 
                                            black10_14kM, tract_hhincblackall$allblackFamM), 2))

# black families making 15-24,999k
tract_black15_24k <- tract_hhincblack %>% 
  filter(variable %in% c("B19101B_004", "B19101B_005")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(black15_24kE = sum(estimate),
            black15_24kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_black15_24k$perc_black15_24kE <- round(tract_black15_24k$black15_24kE / tract_hhincblackall$allblackFamE * 100, 2)
tract_black15_24k$perc_black15_24kM <- round(moe_prop(tract_black15_24k$black15_24kE, tract_hhincblackall$allblackFamE, 
                                                      tract_black15_24k$black15_24kM, tract_hhincblackall$allblackFamM), 2)

# black families making 25-34,999k
tract_black25_34k <- tract_hhincblack %>% 
  filter(variable %in% c("B19101B_006", "B19101B_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(black25_34kE = sum(estimate),
            black25_34kM = round(moe_sum(moe = moe, estimate = estimate), 2)) 
tract_black25_34k$perc_black25_34kE <- round(tract_black25_34k$black25_34kE / tract_hhincblackall$allblackFamE * 100, 2)
tract_black25_34k$perc_black25_34kM <- round(moe_prop(tract_black25_34k$black25_34kE, tract_hhincblackall$allblackFamE, 
                                                      tract_black25_34k$black25_34kM, tract_hhincblackall$allblackFamM), 2)

# black families making under 35k
tract_blackunder35k <- tract_hhincblack %>% 
  filter(variable %in% c("B19101B_002", "B19101B_003", "B19101B_004",
                         "B19101B_005", "B19101B_006", "B19101B_007")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(blackunder35kE = sum(estimate),
            blackunder35kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_blackunder35k$perc_blackunder35kE <- round(tract_blackunder35k$blackunder35kE / tract_hhincblackall$allblackFamE * 100, 2)
tract_blackunder35k$perc_blackunder35kM <- round(moe_prop(tract_blackunder35k$blackunder35kE, tract_hhincblackall$allblackFamE, 
                                                          tract_blackunder35k$blackunder35kM, tract_hhincblackall$allblackFamM), 2)
# black families making 35-100k 
tract_black35_100 <- tract_hhincblack %>% 
  filter(variable %in% c("B19101B_008", "B19101B_009", "B19101B_010",
                         "B19101B_011", "B19101B_012", "B19101B_013")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(black35_100kE = sum(estimate),
            black35_100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_black35_100$perc_black35_100kE <- round(tract_black35_100$black35_100kE / tract_hhincblackall$allblackFamE * 100, 2)
tract_black35_100$perc_black35_100kM <- round(moe_prop(tract_black35_100$black35_100kE, tract_hhincblackall$allblackFamE, 
                                                       tract_black35_100$black35_100kM, tract_hhincblackall$allblackFamM), 2)

# black families making over 100k 
tract_blackover100 <- tract_hhincblack %>% 
  filter(variable %in% c("B19101B_014", "B19101B_015", "B19101B_016",
                         "B19101B_017")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(blackover100kE = sum(estimate),
            blackover100kM = round(moe_sum(moe = moe, estimate = estimate), 2))
tract_blackover100$perc_blackover100kE <- round(tract_blackover100$blackover100kE / tract_hhincblackall$allblackFamE * 100, 2)
tract_blackover100$perc_blackover100kM <- round(moe_prop(tract_blackover100$blackover100kE, tract_hhincblackall$allblackFamE, 
                                                         tract_blackover100$blackover100kM, tract_hhincblackall$allblackFamM), 2)

# ....................................................
# 3. Combining all data frames #########################

tract_data <- tract_data_s %>%
  left_join(hhincallT) %>%
  left_join(tract_allunder10k) %>%
  left_join(tract_all10_14k) %>%
  left_join(tract_all15_24k) %>%
  left_join(tract_all25_34k) %>%
  left_join(tract_allunder35k) %>%
  left_join(tract_all35_100) %>%
  left_join(tract_allover100) %>%
  left_join(tract_hhincwhiteall) %>%
  left_join(tract_whiteunder10k) %>%
  left_join(tract_white10_14k) %>%
  left_join(tract_white15_24k) %>%
  left_join(tract_white25_34k) %>%
  left_join(tract_whiteunder35k) %>%
  left_join(tract_white35_100) %>%
  left_join(tract_whiteover100) %>%
  left_join(tract_hhincblackall) %>%
  left_join(tract_blackunder10k) %>%
  left_join(tract_black10_14k) %>%
  left_join(tract_black15_24k) %>%
  left_join(tract_black25_34k) %>%
  left_join(tract_blackunder35k) %>%
  left_join(tract_black35_100) %>%
  left_join(tract_blackover100)
  

tract_data <- tract_data %>% 
  mutate(year = "2020") 

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

tract_data$locality <- as.character(tract_data$locality)

tract_data$countyName <- recode(tract_data$locality, "003" = "Charlottesville", "029" = "Buckingham", "065" = "Fluvanna",
         "540" = "Albemarle", "109" = "Louisa", "125" = "Nelson", "079" = "Greene")

# ....................................................
# 4. Save #########################
write.csv(tract_data, "acs_tract_orangedot.csv", row.names = F) 










