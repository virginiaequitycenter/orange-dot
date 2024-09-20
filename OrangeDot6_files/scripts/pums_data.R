# PUMS Data

# Load packages ----
library(here)
library(tidyverse)
library(tidycensus)
library(tigris)
library(rcartocolor)

# Sandbox

# PUMA 2020
puma_codes <- c("10901", # Thomas Jefferson Planning District Commission (South & East) PUMA
                "54001", # Thomas Jefferson Planning District Commission (North) PUMA
                "08300" # Southside Planning District Commission & Commonwealth Regional Council PUMA
                )

pums_vars_2022 <- pums_variables %>% 
  filter(year == 2022, survey == "acs1")

pums_vars_2022 <- pums_vars_2022 %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "housing")

va_pums_recoded <- get_pums(
  variables = c("PUMA", "FINCP", "HINCP"),
  state = "VA",
  survey = "acs1",
  year = 2022,
  recode = TRUE
) 

va_pums_fincp <- get_pums(
  variables = c("PUMA", "FINCP"),
  state = "VA",
  survey = "acs1",
  year = 2022,
  recode = TRUE
) 

va_fincp <- va_pums_fincp %>%
  group_by(ST, PUMA) %>%
  summarize(
    total_pop = sum(PWGTP),
    pct_in_pov = sum(PWGTP[FINCP < 60000]) / total_pop
  )


va_pumas <- map("VA", tigris::pumas, class = "sf", cb = TRUE, year = 2020) %>% 
  reduce(rbind)

va_pumas %>%
  left_join(va_fincp, by = c("STATEFP20" = "ST", "PUMACE20" = "PUMA")) %>%
  filter(PUMACE20 %in% puma_codes) %>% 
  ggplot(aes(fill = pct_in_pov)) +
  geom_sf() +
  scale_fill_stepsn(colors = carto_pal(5, "Teal"),
                    labels = scales::label_percent(1),
                    n.breaks = 5,
                    guide = guide_colourbar(title = "",
                                            title.position = "top",
                                            direction = "vertical",
                                            title.hjust = 0.5,
                                            # nbin = 6,
                                            # theme = theme(legend.key.height  = unit(0.5, "lines"),
                                            #               legend.key.width = unit(12, "lines"),
                                            #               text = element_text(size = 11)) 
                    )
  )+
  # scale_fill_viridis_b(
  #   name = NULL,
  #   option = "magma",
  #   labels = scales::label_percent(1)
  # ) +
  labs(title = "Percentage of population w/ family income <50,000") +
  theme_void()
