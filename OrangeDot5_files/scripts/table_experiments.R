# table experimentation
# 2022-09-08
# mpc

library(tidyverse)
library(reactable)
library(flextable)

# ACS data
tractdat <- read_csv("data/acs_tract_orangedot.csv")
tractdat$GEOID <- as.character(tractdat$GEOID)
countydat <- read_csv("data/acs_county_orangedot.csv")

highlightcolor <- ("#7991BD") # FDE725FF

# reactable version
incomebins <- data_frame(Income = list("$0 - $9,999", "$10,000 - $14,999", "$15,000 - $24,999", "$25,000 - $34,999"),
                         Number = list(countydat[8,]$AllFamUnder10kE, countydat[8,]$AllFamBtw10_14kE, countydat[8,]$AllFamBtw15_24kE, countydat[8,]$AllFamBtw25_34kE),
                         Percent = list(countydat[8,]$perc_AllFamUnder10kE, countydat[8,]$perc_AllFamBtw10_14kE, countydat[8,]$perc_AllFamBtw15_24kE, countydat[8,]$perc_AllFamBtw25_34kE))

reactable(incomebins, defaultSorted = c("Income"),
          highlight = TRUE,
          bordered = TRUE,
          rowStyle = function(index) {
            if (incomebins[index, "Income"] == "$15,000 - $24,999" | incomebins[index, "Income"] == "$25,000 - $34,999") {
              list(background = highlightcolor, color = "white")}
          }
)


# flextable
incomebins_tot <- tibble(Group = c(1,1,2,2,3),
                             Income = c("$0 - $9,999", "$10,000 - $14,999", "$15,000 - $24,999", "$25,000 - $34,999", "Total"),
                         Number = c(countydat[8,]$AllFamUnder10kE, countydat[8,]$AllFamBtw10_14kE, countydat[8,]$AllFamBtw15_24kE, countydat[8,]$AllFamBtw25_34kE, countydat[8,]$AllFamUnder35kE))

incomebins_tot <- incomebins_tot %>% 
  mutate(Percent = round((Number/countydat[8,]$AllFamUnder35kE)*100, 0)) %>% 
  group_by(Group) %>% 
  mutate(Cumulative = sum(Percent)) %>% 
  ungroup() %>% 
  select(-Group)

flextable(incomebins_tot) %>% 
  merge_v() %>% 
  merge_h() %>% 
  width(width = 5) %>% 
  align(align = "center", part = "all") %>% 
  bg(i = ~ Percent > 29 & Percent < 100, bg = highlightcolor) %>% 
  color(i = ~Percent > 29 & Percent < 100, color = "white") %>% 
  colformat_num(j = c("Percent", "Cumulative"), suffix = "%")

# also huxtable: https://hughjonesd.github.io/huxtable/
