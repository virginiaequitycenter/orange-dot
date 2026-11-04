# Orange Dot
# Exploration of trends over time
# Was not used in 7.0

library(tidyverse)
library(readxl)
library(tidycensus)
library(janitor)

# Costs ----
# #rent ----
### HUD Fair-Market Rent ----
# https://www.huduser.gov/portal/datasets/fmr.html#history
url <- "https://www.huduser.gov/portal/datasets/FMR/FMR_2Bed_1983_2027.csv"
fmr <- read_csv(url)
fmr <- read_csv("~/Downloads/FMR_2Bed_1983_2027.csv")
fmr_va <- fmr |> 
  filter(state == "51")
fmr_cvl <- fmr_va |> 
  filter(areaname27 == "Charlottesville, VA MSA") |> 
  select(fips, county, areaname27, name, fmr27_2,
         fmr26_2, fmr25_2, fmr24_2, fmr23_2, fmr22_2,
         fmr21_2, fmr20_2, fmr19_2, fmr18_2, fmr17_2,
         fmr16_2, fmr15_2, fmr14_2, fmr13_2, fmr12_2,
         fmr11_2, fmr10_2, fmr09_2, fmr08_2, fmr07_2,
         fmr06_2, fmr05_2, fmr04_2, fmr03_2, fmr02_2,
         fmr01_2, fmr00_2) |> 
  pivot_longer(fmr27_2:fmr00_2, 
               names_to = "fmr_year", values_to = "fmr2") |> 
  mutate(year = str_remove(fmr_year, "fmr"),
         year = str_remove(year, "_2"))

fmr_cvl |> 
  # same value for all counties in MSA
  filter(name == "Charlottesville city") |> 
ggplot(aes(x = year, y = fmr2)) +
  geom_point() + 
  geom_line(group = 1)

### Zillow ----
# https://www.zillow.com/research/data/
# ZORI Multifamily residence time series
zori <- read_csv("~/Downloads/Metro_zori_uc_mfr_sm_sa_month.csv")
zori_va <- zori |> 
  filter(StateName == "VA")
zori_cvl <- zori_va |> 
  filter(RegionName == "Charlottesville, VA") |> 
  select(-c(RegionID, SizeRank, RegionType)) |> 
  pivot_longer(cols = -c(RegionName, StateName), 
               names_to = "date",
               values_to = "zori") |> 
  mutate(date = ymd(date))

ggplot(zori_cvl, aes(x = date, y = zori)) +
  geom_point() +
  geom_line()

zori_cvl_year <- zori_cvl |> 
  mutate(year = year(date)) |> 
  group_by(year) |> 
  summarize(zori_average = mean(zori))

ggplot(zori_cvl_year, aes(x = year, y = zori_average)) +
  geom_point() +
  geom_line()

fmr_cvl |> 
  # same value for all counties in MSA
  filter(name == "Charlottesville city",
         as.numeric(year) > 14,
         as.numeric(year) < 27) |> 
  ggplot(aes(x = year, y = fmr2)) +
  geom_point() + 
  geom_line(group = 1)

## child care? ----
# https://www.dol.gov/agencies/wb/topics/featured-childcare
cc <- read_excel("~/Downloads/NDCP2022-state-level-estimates-and-rankings.xlsx") |> 
  filter(STATE_NAME == "Virginia") |> 
  clean_names()

ggplot(cc, aes(x = studyyear, y = median_infant_price, color = type)) +
  geom_point() +
  geom_line()

# https://fred.stlouisfed.org/tags/series?t=cpi%3Bday+care%3Bmonthly?

# # save work ----
# save.image("trends.Rdata")
# load("trends.Rdata")


# Income/Earnings ----
## Median family income ----
# ACS, Cville msa
years <- c(2009,2010,2011,2012,2013,2014,
           2015,2016,2017,2018,2019,2021,
           2022,2023,2024)

med_faminc <- map_dfr(years, 
  ~ get_acs(
  geography = "cbsa",
  variables = "B19113_001",
  year = .x,
  survey = "acs1"
) |> mutate(year = .x))

cvl_med_faminc <- med_faminc |> 
  filter(GEOID == "16820")

ggplot(cvl_med_faminc, aes(x = year, y = estimate)) +
  geom_point() +
  geom_line()

## Median earnings ----
med_earn <- map_dfr(years, 
                      ~ get_acs(
                        geography = "cbsa",
                        variables = "B20002_001",
                        year = .x,
                        survey = "acs1"
                      ) |> mutate(year = .x))

cvl_med_earn <- med_earn |> 
  filter(GEOID == "16820")

ggplot(cvl_med_earn, aes(x = year, y = estimate/12)) +
  geom_point() +
  geom_line()

## Minimum wage ----
# https://fred.stlouisfed.org/series/STTMINWGVA
min_va <- read_csv("~/Downloads/STTMINWGVA.csv")


## Wages ----
# https://www.bls.gov/oes/tables.htm - 10th, 25th, median, 75th, 90th percentiles over time
# May 2025: https://www.bls.gov/oes/special-requests/oesm25all.zip
m25 <- read_excel("~/Downloads/oesm25all/all_data_M_2025.xlsx")
cvl25 <- m25 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2025")
# May 2024: https://www.bls.gov/oes/special-requests/oesm24all.zip
m24 <- read_excel("~/Downloads/oesm24all/all_data_M_2024.xlsx")
cvl24 <- m24 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2024")
# May 2023: https://www.bls.gov/oes/special-requests/oesm23all.zip
m23 <- read_excel("~/Downloads/oesm23all/all_data_M_2023.xlsx")
cvl23 <- m23 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2023")
# May 2022: https://www.bls.gov/oes/special-requests/oesm22all.zip
m22 <- read_excel("~/Downloads/oesm22all/all_data_M_2022.xlsx")
cvl22 <- m22 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2022")
# May 2021: https://www.bls.gov/oes/special-requests/oesm21all.zip
m21 <- read_excel("~/Downloads/oesm21all/all_data_M_2021.xlsx")
cvl21 <- m21 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2021")
# May 2020: https://www.bls.gov/oes/special-requests/oesm20all.zip
m20 <- read_excel("~/Downloads/oesm20all/all_data_M_2020.xlsx")
cvl20 <- m20 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2020")
# May 2019: https://www.bls.gov/oes/special-requests/oesm19all.zip
m19 <- read_excel("~/Downloads/oesm19all/all_data_M_2019.xlsx")
cvl19 <- m19 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2019")
# May 2018: https://www.bls.gov/oes/special-requests/oesm18all.zip
m18 <- read_excel("~/Downloads/oesm18all/all_data_M_2018.xlsx")
cvl18 <- m18 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2018")
# May 2017: https://www.bls.gov/oes/special-requests/oesm17all.zip
m17 <- read_excel("~/Downloads/oesm17all/all_data_M_2017.xlsx")
cvl17 <- m17 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2017")
# May 2016: https://www.bls.gov/oes/special-requests/oesm16all.zip
m16 <- read_excel("~/Downloads/oesm16all/all_data_M_2016.xlsx")
cvl16 <- m16 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2016")
# May 2015: https://www.bls.gov/oes/special-requests/oesm15all.zip
m15 <- read_excel("~/Downloads/oesm15all/all_data_M_2015.xlsx")
cvl15 <- m15 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2015")
# May 2014: https://www.bls.gov/oes/special-requests/oesm14all.zip
m14 <- read_excel("~/Downloads/all_data_M_2014.xlsx")
cvl14 <- m14 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2014")
# May 2013: https://www.bls.gov/oes/special-requests/oesm13all.zip
m13 <- read_excel("~/Downloads/oes_data_2013.xlsx")
cvl13 <- m13 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2013")
# May 2012: https://www.bls.gov/oes/special-requests/oesm12all.zip
m12 <- read_excel("~/Downloads/all_oes_data_2012.xlsx")
cvl12 <- m12 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2012")
# May 2011: https://www.bls.gov/oes/special-requests/oesm11all.zip
m11 <- read_excel("~/Downloads/oes_data_2011.xlsx")
cvl11 <- m11 |> 
  clean_names() |> 
  filter(area == "16820", occ_code == "00-0000") |> 
  select(area_title, occ_title, tot_emp, starts_with("a_")) |> 
  mutate(time = "May 2011")

rm(m25,m24,m23,m22,m21,m20,m19,m18,m17,m16,m15,m14,m13,m12,m11)
# cvl14, cvl20: tot_emp is character
cvl14 <- cvl14 |> mutate(tot_emp = as.numeric(tot_emp))
cvl20 <- cvl20 |> mutate(tot_emp = as.numeric(tot_emp))

cvl_oes <- bind_rows(cvl25,cvl24,cvl23,cvl22,cvl21,cvl20,cvl19,
                     cvl18,cvl17,cvl16,cvl15,cvl14,cvl13,cvl12,cvl11) |> 
  mutate(year = str_sub(time, 5,8))

ggplot(cvl_oes, aes(x = year, y = tot_emp)) +
  geom_point() + 
  geom_line(aes(group = 1))

ggplot(cvl_oes, aes(x = year, y = as.numeric(a_pct25)/12)) +
  geom_point() + 
  geom_line(aes(group = 1))

rm(cvl11,cvl12,cvl13,cvl14,cvl15,cvl16,cvl17,cvl18,cvl19,cvl20,cvl21,cvl22,cvl23,cvl24,cvl25)

cvl_oes |> 
  select(area_title, a_pct10:a_pct90, year) |> 
  pivot_longer(cols = a_pct10:a_pct90, names_to = "percentile", values_to = "income") |> 
  ggplot(aes(x = year, y = as.numeric(income)/12, group = percentile, color = percentile)) +
  geom_line()

# explore further?
# https://www.bls.gov/regions/mid-atlantic/news-release/countyemploymentandwages_virginia.htm


# # save work ----
# save.image("trends.Rdata")
# load("trends.Rdata")

# combine measures ----
fmr_cvl_final <- fmr_cvl |> 
  filter(county == "003")  |> 
  select(areaname27, fmr = fmr2, year) |> 
  mutate(year = paste0("20", year),
         year = as.numeric(year)) |> 
  filter(year >= 2011, year < 2027)

zori_cvl_annual <- zori_cvl_year |> 
  select(year, zori = zori_average)

cvl_medearn_acs <- cvl_med_earn |> 
  select(name = NAME, med_earn_acs = estimate, year)

minwage_va <- min_va |> 
  mutate(year = year(observation_date),
         min_monthly_wage = (STTMINWGVA *2080)/12) |> 
  filter(year >= 2011) |> 
  select(year, min_monthly_wage)

cvl_oes_final <- cvl_oes |> 
  select(area_title, a_pct10:a_pct90, year) |> 
  mutate(year = as.numeric(year))

combined <- cvl_oes_final |> 
  left_join(cvl_medearn_acs, by = "year") |> 
  left_join(minwage_va, by = "year") |> 
  left_join(fmr_cvl_final, by = "year") |> 
  left_join(zori_cvl_annual, by = "year") 


# save trends data file ----
write_csv(combined, "trends.csv")
combined <- read_csv("scripts/trends.csv")

# from https://www.bls.gov/oes/tables.htm: a_pct10 - a_pct90 
# from acs: med_earn_acs
# from https://fred.stlouisfed.org/series/STTMINWGVA: min_monthly_wage
# from https://www.huduser.gov/portal/datasets/fmr.html: fmr (fair market rate for 2-BR apt)
# from https://www.zillow.com/research/data/: zori (observed rent index, multifamily residence)

# explore ----
# do fmr/zori track closely
combined |> 
  select(areaname27, year, fmr, zori) |> 
  pivot_longer(fmr:zori, names_to = "source", values_to = "rent") |> 
  ggplot(aes(x = year, y = rent, group = source, color = source)) +
  geom_point() +
  geom_line()

# do acs median earnings/bls median wages track closely
combined |> 
  select(areaname27, year, a_median, med_earn_acs) |> 
  mutate(a_median = as.numeric(a_median)/12,
         med_earn_acs = med_earn_acs/12) |> 
  pivot_longer(a_median:med_earn_acs, names_to = "source", values_to = "rent") |> 
  ggplot(aes(x = year, y = rent, group = source, color = source)) +
  geom_point() +
  geom_line()

# zori and earnings: try median, percentiles
combined |> 
  select(areaname27, year, zori, a_pct10, a_pct90) |> 
  mutate(a_pct10 = as.numeric(a_pct10)/12,
         a_pct90 = as.numeric(a_pct90)/12) |> 
  pivot_longer(zori:a_pct90, names_to = "trend", values_to = "dollars") |> 
  ggplot(aes(x = year, y = dollars, group = trend, color = trend)) +
  geom_point() +
  geom_line()

combined |> 
  select(areaname27, year, zori, a_pct25, a_pct75) |> 
  mutate(a_pct25 = as.numeric(a_pct25)/12,
         a_pct75 = as.numeric(a_pct75)/12) |> 
  pivot_longer(zori:a_pct75, names_to = "trend", values_to = "dollars") |> 
  filter(year >= 2015) |> 
  ggplot(aes(x = year, y = dollars, group = trend, color = trend)) +
  geom_point() +
  geom_line()

combined |> 
  select(areaname27, year, zori, a_median, min_monthly_wage) |> 
  mutate(a_median = as.numeric(a_median)/12) |> 
  pivot_longer(zori:min_monthly_wage, names_to = "trend", values_to = "dollars") |> 
  filter(year >= 2015) |> 
  ggplot(aes(x = year, y = dollars, group = trend, color = trend)) +
  geom_point() +
  geom_line()

# curious about 10 year rate of increase in each series
# a_pct10, 2025-2015: 30680-18940 = 11740 (978.33), 62%
# a_pct25, 2025-2015: 37860-24770 = 13090 (1090.83), 53%
# a_pct50, 2025-2015: 55180-37660 = 17520 (1460), 47%
# a_pct75, 2025-2015: 85600-59990 = 25610 (2134.17), 43%
# a_pct90, 2025-2015: 137730-91930 = 45800 (3816.67), 50%
# fmr, 2025-2015: 1729-1038 = 691, 67%
# zori, 2025-2015: 1809.698-1081.443 = 728.255, 67%
# median earnings, 2024-2014: 47123-30248 = 16875 (1406.25), 56%

