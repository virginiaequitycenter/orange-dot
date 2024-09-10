####################################################
# ORANGE DOT REPROT
####################################################
# Acquire ACS data summaries
# For Number and Percent of families nder 35K over time
# 2011-2020
# Last updated: 09/29/2022

library(tidyverse)
library(tidycensus)
library(ggthemes)

# local fips
# albemarle, buckingham, fluvanna, greene, louisa, nelson, cville
localfips <- c("003", "029", "065", "079", "109", "125", "540")

# .................................
# Get community data ----
# Family income (B19101)
local_all <-
  map_df(2009:2020,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101",
           survey = "acs5",
           summary_var = "B19101_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )


# .................................
# Wrangle data ----
local_fsw <- local_all %>% 
  filter(variable != "B19101_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101_002",
                               "$10,000 - $14,999" = "B19101_003",
                               "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
                               "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_fsw_sum <- local_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1))

# .................................
# Generate figures ----
loc_per <- local_fsw_sum %>% 
  filter(income != "$35,000 or more") %>% 
  group_by(year) %>% 
  summarize(families = sum(families),
            total = first(total)) %>% 
  mutate(percent_families = round(families/total*100, 1)) %>% 
  ggplot(aes(x = year, y = percent_families, label = families)) +
  geom_line() +
  annotate("text", x = 2020, y = 16, label = "14.1%", color = "#88419d") +
  coord_cartesian(xlim = c(2009, 2020), 
                  clip = 'off') +   
  scale_y_continuous(name = "% of Families Struggling", limits = c(0,30)) +
  scale_x_continuous(name = "", breaks = seq(2009, 2020, 1)) +
  theme_clean()
loc_per

loc_num <- local_fsw_sum %>% 
  filter(income != "$35,000 or more") %>% 
  ggplot(aes(x = year, y = families, fill = income, label = percent)) +
  geom_area() +
  scale_x_continuous(name = "", breaks = seq(2011, 2020, 1)) +
  scale_fill_brewer(name = "Income Range", palette = "BuPu") +
  labs(y = "# of Families Struggling") +
  theme_minimal() 
loc_num

# .................................
# Calculating multiple thresholds ----
local_fsw_35 <- local_all %>% 
  filter(variable != "B19101_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101_002",
                               "$10,000 - $14,999" = "B19101_003",
                               "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
                               "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_fsw_40 <- local_all %>% 
  filter(variable != "B19101_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101_002",
                               "$10,000 - $14,999" = "B19101_003",
                               "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
                               "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
                               "35,000 - $39,999" = c("B19101_008"),
                               other_level = "$40,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_fsw_45 <- local_all %>% 
  filter(variable != "B19101_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101_002",
                               "$10,000 - $14,999" = "B19101_003",
                               "$15,000 - $24,999" = c("B19101_004", "B19101_005"),
                               "$25,000 - $34,999" = c("B19101_006", "B19101_007"),
                               "35,000 - $44,999" = c("B19101_008", "B19101_009"),
                               other_level = "$45,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

# summing
local_fsw_sum_35 <- local_fsw_35 %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         threshold = "35K")

local_fsw_sum_40 <- local_fsw_40 %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         threshold = "40K")

local_fsw_sum_45 <- local_fsw_45 %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         threshold = "45K")

# .................................
# Joining multiple thresholds ----
local_fsw_threshold <- bind_rows(
  local_fsw_sum_35 %>% 
    select(year, income, families, total, threshold) %>% 
    filter(income != "$35,000 or more") %>% 
    group_by(year) %>% 
    summarize(families = sum(families),
              total = first(total),
              threshold = first(threshold)) %>% 
    mutate(percent = round(families/total*100, 1)), 
  local_fsw_sum_40 %>% 
    select(year, income, families, total, threshold) %>% 
    filter(income != "$40,000 or more") %>% 
    group_by(year) %>% 
    summarize(families = sum(families),
              total = first(total),
              threshold = first(threshold)) %>% 
    mutate(percent = round(families/total*100, 1)), 
  local_fsw_sum_45 %>% 
    select(year, income, families, total, threshold) %>% 
    filter(income != "$45,000 or more") %>% 
    group_by(year) %>% 
    summarize(families = sum(families),
              total = first(total),
              threshold = first(threshold)) %>% 
    mutate(percent = round(families/total*100, 1))
  )

# .................................
# Visualizing multiple thresholds ----
ggplot(local_fsw_threshold, aes(x = year, y = percent, color = threshold)) +
  geom_line() +
  scale_x_continuous(name = "", breaks = seq(2009, 2020, 1)) +
  scale_y_continuous(limits = c(0,30))

ggplot(local_fsw_threshold, aes(x = year, y = families, color = threshold)) +
  geom_line() +
  scale_x_continuous(name = "", breaks = seq(2009, 2020, 1))


# .................................
# Comparing by race ----
# Charlottesville region localities, by race, ACS 5-year estimates 
local_black <-
  map_df(2009:2020,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101B",
           survey = "acs5",
           summary_var = "B19101B_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

local_white <-
  map_df(2009:2020,
         ~ get_acs(
           year = .x,
           state = "VA",
           geography = "county",
           county = localfips,
           table = "B19101H",
           survey = "acs5",
           summary_var = "B19101H_001",
           cache = TRUE
         ) %>% 
           mutate(year = .x)
  )

# Prep data
# Black
local_black_fsw <- local_black %>% 
  filter(variable != "B19101B_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101B_002",
                               "$10,000 - $14,999" = "B19101B_003",
                               "$15,000 - $24,999" = c("B19101B_004", "B19101B_005"),
                               "$25,000 - $34,999" = c("B19101B_006", "B19101B_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_black_fsw_sum <- local_black_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "Black")

# White
local_white_fsw <- local_white %>% 
  filter(variable != "B19101H_001") %>% 
  mutate(income = variable,
         income = fct_collapse(income,
                               "$0 - $9,999" = "B19101H_002",
                               "$10,000 - $14,999" = "B19101H_003",
                               "$15,000 - $24,999" = c("B19101H_004", "B19101H_005"),
                               "$25,000 - $34,999" = c("B19101H_006", "B19101H_007"),
                               other_level = "$35,000 or more")) %>%
  group_by(year, NAME, income) %>%
  summarize(families = sum(estimate),
            total = first(summary_est)) %>%
  mutate(percent = round(families/total*100,1))

local_white_fsw_sum <- local_white_fsw %>% 
  group_by(year, income) %>% 
  summarize(families = sum(families),
            total = sum(total)) %>% 
  mutate(percent = round(families/total*100,1),
         race = "White")
