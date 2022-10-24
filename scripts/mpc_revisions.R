library(scales)
library(LaCroixColoR)
library(flextable)
library(officer)
library(patchwork)
# library(plotly)
library(leaflet)
library(tidyverse)
library(ggpubr)

countydat <- read_csv("data/acs_county_orangedot.csv")
countydat <- countydat %>% arrange(countyName)

show_col(lacroix_palette("Apricot", type = "continuous", n = 10))
show_col(lacroix_palette("Apricot", type = "discrete"))
show_col(lacroix_palette("Orange", type = "continuous", n = 15))
show_col(lacroix_palette("Orange", type = "discrete"))

# possible substitute for code chunk 3
income_break <- countydat %>% 
  filter(countyName != "Whole Region") %>% 
  select(AllFamUnder10kE, AllFamBtw10_14kE,
         AllFamBtw15_24kE, AllFamBtw25_34kE, AllFamUnder35kE) %>% 
  pivot_longer(cols = everything(), names_to = "Income", values_to = "Number") %>% 
  group_by(Income) %>% 
  summarize(Number = sum(Number)) %>% 
  mutate(Income = factor(Income, 
                         levels = c("AllFamUnder10kE", "AllFamBtw10_14kE",
                                    "AllFamBtw15_24kE", "AllFamBtw25_34kE",
                                    "AllFamUnder35kE"),
                         labels = c("$0 - $9,999", "$10,000 - $14,999",
                                    "$15,000 - $24,999", "$25,000 - $34,999",
                                    "Total"))) %>% 
  arrange(Income) %>% mutate(group = c(1,1,2,2,3))

totfam <- as_vector(income_break[5,2])

income_break <- income_break %>% 
  mutate(Percent = round((Number/totfam)*100, 0)) %>% 
  group_by(group) %>% 
  mutate(Cumulative = sum(Percent)) %>% 
  ungroup() %>% 
  select(-group)

highlightcolor <- "#EE6100"
lowlightcolor <- "#FFAD0A"
  
flextable(income_break) %>% 
  merge_v() %>% 
  merge_h() %>% 
  width(width = 5) %>% 
  align(align = "center", part = "all") %>% 
  bg(j = "Cumulative", bg = lowlightcolor) %>%
  bg(j = "Cumulative", i = ~ Cumulative > 70, bg = highlightcolor) %>% 
  colformat_num(j = c("Percent", "Cumulative"), suffix = "%") %>% 
  border_inner_h(border = fp_border(color="gray", width=1))

# code chunks 4 and 5
## figure 1
cnty_race_long <- countydat %>% 
  filter(countyName != "Whole Region") %>% 
  select(countyName, count_AllFamUnder35kE = AllFamUnder35kE, 
         count_BlackFamUnder35kE = BlackFamUnder35kE, count_WhiteFamUnder35kE = WhiteFamUnder35kE,
         perc_AllFamUnder35kE, perc_WhiteFamUnder35kE, perc_BlackFamUnder35kE) %>% 
  pivot_longer(-countyName, names_to = c(".value", "fam"), names_sep = "_") %>% 
  mutate(fam = factor(fam, levels = c("AllFamUnder35kE", "BlackFamUnder35kE", "WhiteFamUnder35kE"),
                        labels = c("All families", "Black families", "White families")))

numplot <- ggplot(cnty_race_long, aes(y = count, 
                           x = fct_reorder(countyName, count), 
                           fill = fct_rev(fam))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = comma(count)),
            position = position_dodge(width=0.9), hjust = -0.2, size = 2.5) + 
  labs(title = "Number of Families", x = "", y = "") + 
  scale_fill_manual(values=c("#7473A6", "#1BB6AF", "#FFAD0A"), 
                    name = element_blank()) + 
  scale_y_continuous(labels = label_comma(),
                     limits = c(0,2800)) +
  annotate("text", x = "Greene", y = 2000, color = "gray50",
           label = "9,413 families\n in region") +
  theme_minimal() +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom")

allperc <- as_vector(countydat %>% filter(countyName == "Whole Region") %>% 
                       select(perc_AllFamUnder35kE))

percplot <- ggplot(cnty_race_long, aes(y = perc,
                           x = fct_reorder(countyName, perc), 
                           fill = fct_rev(fam))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = percent(perc, scale = 1, accuracy = 0.1)),
            position = position_dodge(width=0.9), hjust = -0.25, size = 2.5) + 
  labs(title = "Percent of Families", x = "", y = "") + 
  scale_fill_manual(values=c("#7473A6", "#1BB6AF", "#FFAD0A"), 
                    name = element_blank()) + 
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(0,53)) +
  geom_hline(yintercept = allperc, color = "gray50", linetype = "dashed") +
  annotate("curve", y = 30, x = 1, yend = 14.5, xend = 1.7, 
           curvature = -0.1, arrow = arrow(length = unit(2, "mm")),  color = "gray50") +
  annotate("text", x = 1.1, y = 30.5, size = 3, color = "gray50",
           label = "Percent in region (14%)", hjust = "left") +
  theme_minimal() +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom")

numplot + percplot + 
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

# code chunk 7
incomebinsAC <- tibble(Income = c("$0 - $9,999", "$10,000 - $14,999", "$15,000 - $24,999", "$25,000 - $34,999", "Total"),
                       Number = c(albemarle$AllFamUnder10kE, albemarle$AllFamBtw10_14kE, albemarle$AllFamBtw15_24kE, albemarle$AllFamBtw25_34kE, sum(albemarle$AllFamUnder10kE, albemarle$AllFamBtw10_14kE, albemarle$AllFamBtw15_24kE, albemarle$AllFamBtw25_34kE)),
                       "of whole county" = c(albemarle$perc_AllFamUnder10kE, albemarle$perc_AllFamBtw10_14kE, albemarle$perc_AllFamBtw15_24kE, albemarle$perc_AllFamBtw25_34kE, sum(albemarle$perc_AllFamUnder10kE, albemarle$perc_AllFamBtw10_14kE, albemarle$perc_AllFamBtw15_24kE, albemarle$perc_AllFamBtw25_34kE)),
                       "of families making < 35K in county" =
                         c(round(albemarle$AllFamUnder10kE/albemarle$AllFamUnder35kE * 100, 2),
                           round(albemarle$AllFamBtw10_14kE/albemarle$AllFamUnder35kE * 100, 2),
                           round(albemarle$AllFamBtw15_24kE/albemarle$AllFamUnder35kE * 100, 2),
                           round(albemarle$AllFamBtw25_34kE/albemarle$AllFamUnder35kE * 100, 2),
                           100))

flextable(incomebinsAC) %>% 
  width(width = 5) %>%
  align(align = "center", part = "all") %>% 
  bg(i = ~Number > 2000, bg = highlightcolor) %>%
  color(i = ~Number > 2000, color = "black") 

# locality specific
albemarle <- countydat %>% filter(countyName == "Albemarle")

income_break_ac <- albemarle %>% 
  select(AllFamUnder10kE, AllFamBtw10_14kE,
         AllFamBtw15_24kE, AllFamBtw25_34kE, AllFamUnder35kE) %>% 
  pivot_longer(cols = everything(), names_to = "Income", values_to = "Number") %>% 
  group_by(Income) %>% 
  summarize(Number = sum(Number)) %>% 
  mutate(Income = factor(Income, 
                         levels = c("AllFamUnder10kE", "AllFamBtw10_14kE",
                                    "AllFamBtw15_24kE", "AllFamBtw25_34kE",
                                    "AllFamUnder35kE"),
                         labels = c("$0 - $9,999", "$10,000 - $14,999",
                                    "$15,000 - $24,999", "$25,000 - $34,999",
                                    "Total"))) %>% 
  arrange(Income) %>% mutate(group = c(1,1,2,2,3))

totfam <- as_vector(income_break_ac[5,2])

income_break_ac <- income_break_ac %>% 
  mutate(Percent = round((Number/totfam)*100, 0)) %>% 
  group_by(group) %>% 
  mutate(Cumulative = sum(Percent)) %>% 
  ungroup() %>% 
  select(-group)

flextable(income_break_ac) %>% 
  merge_v() %>% 
  merge_h() %>% 
  width(width = 5) %>% 
  align(align = "center", part = "all") %>% 
  bg(j = "Cumulative", bg = lowlightcolor) %>%
  bg(j = "Cumulative", i = ~ Cumulative > 70, bg = highlightcolor) %>% 
  colformat_num(j = c("Percent", "Cumulative"), suffix = "%") %>% 
  border_inner_h(border = fp_border(color="gray", width=1))

# code chunk 8
alb_race_long <- albemarle %>% 
  select(perc_AllFamUnder35kE, perc_WhiteFamUnder35kE, perc_BlackFamUnder35kE,
         perc_AllFamBtw35_59kE, perc_WhiteFamBtw35_59kE, perc_BlackFamBtw35_59kE,
         perc_AllFamBtw60_100kE, perc_WhiteFamBtw60_100kE, perc_BlackFamBtw60_100kE,
         perc_AllFamOver100kE, perc_WhiteFamOver100kE, perc_BlackFamOver100kE) %>% 
  pivot_longer(cols = everything(), names_to = c("fam", "inc"), 
               names_pattern = "perc_(AllFam|BlackFam|WhiteFam)(.+)", 
               values_to = "Percent") %>% 
  mutate(inc = factor(inc, levels = c("Under35kE", "Btw35_59kE", "Btw60_100kE", "Over100kE"),
                      labels = c("Earning Under $35K", "Earning $35K-$59,999K", "Earning $60-100k","Earning Over $100K")),
         fam = factor(fam, levels = c("AllFam", "BlackFam", "WhiteFam"),
                      labels = c("All families", "Black families", "White families")))

ggplot(alb_race_long, aes(y = Percent,
                           x = inc, 
                           fill = fam)) + 
  geom_col(position="dodge") +
  labs(title = "Percent of Ablemarle Families", x = "", y = "") + 
  geom_text(aes(label = percent(Percent, scale = 1, accuracy = 1)),
            position = position_dodge(width=0.9), vjust = -0.25, 
            size = 2.5) + 
  scale_fill_manual(values=c("#FFAD0A", "#1BB6AF", "#7473A6"), 
                    name = element_blank()) + 
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # annotate("text", x = 1.1, y = 30.5, size = 3, color = "gray50",
  #          label = "Percent in region (14%)", hjust = "left") +
  theme_minimal() +
  #coord_flip() +
  #guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom")

ggplot(alb_race_long, aes(y = Percent,
                          x = fam, 
                          fill = inc)) + 
  geom_col(position="dodge") +
  labs(title = "Percent of Ablemarle Families", x = "", y = "") + 
  geom_text(aes(label = percent(Percent, scale = 1, accuracy = 1)),
            position = position_dodge(width=0.9), vjust = -0.25, 
            size = 2.5) + 
  # scale_fill_manual(values=c("#FFAD0A", "#1BB6AF", "#7473A6"), 
  #                   name = element_blank()) + 
  scale_y_continuous(labels = label_percent(scale = 1)) +
  # annotate("text", x = 1.1, y = 30.5, size = 3, color = "gray50",
  #          label = "Percent in region (14%)", hjust = "left") +
  theme_minimal() +
  #coord_flip() +
  #guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.position = "bottom")

#ggplotly(p)

# map
ACmap <- geofull %>%
  filter(countyName == "Albemarle")

# pal <- colorBin(palette = c("#D72000", "#FFAD0A", "#1BB6AF", "#EE6100", "#9093A2", "#132157"), domain = ACmap$medianFamIncomeE)
# show_col(lacroix_palette("Apricot", type = "continuous", n = 20))
pal_vector <- lacroix_palette("Apricot", type = "continuous", n = 20)[5:14]
pal_vector <- lacroix_palette("Orange", type = "continuous", n = 10)[1:10]
# pal_vector <- c("#EE6400","#F37903","#F78C05","#FCA108","#E7AD1B","#ABB046","#6FB272","#33B59D","#2DB0AC","#4CA7A9")

pal <- colorBin(pal_vector, 
                domain = geofull$medianFamIncomeE)

palb <- colorNumeric(pal_vector, 
                domain = geofull$medianFamIncomeE)

# pal <- colorQuantile(pal_vector,
#                      domain = ACmap$medianFamIncomeE, n = 5)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = ACmap,
              fillColor = ~pal(medianFamIncomeE),
              # fillColor = ~lacroix_palette("Apricot", type = "continuous"),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              smoothFactor = 0.3,
              highlight = highlightOptions(
                weight = 1, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Median family income: ", ACmap$medianFamIncomeE)) %>% 
  addLegend("bottomright", pal = pal, values = ACmap$medianFamIncomeE, 
            title = "Median family income", opacity = 0.7)

# map 2
# pal <- colorBin(palette = c("#D72000", "#FFAD0A", "#1BB6AF", "#EE6100", "#9093A2", "#132157"), domain = ACmap$perc_AllFamUnder35kE)

# show_col(lacroix_palette("Apricot", type = "continuous", n = 10))
pal2_vector <- lacroix_palette("Apricot", type = "continuous", n = 10)[2:7]
pal2_vector <- lacroix_palette("Orange", type = "continuous", n = 6)[1:6]
# pal_vector <- c("#E34400","#EF6901","#F99306","#CCAF2E","#4DB48A","#41AAAA")

pal2 <- colorBin(pal2_vector, 
                domain = geofull$perc_AllFamUnder35kE,
                reverse = TRUE)
pal3 <- colorNumeric(pal2_vector, 
                 domain = geofull$perc_AllFamUnder35kE,
                 reverse = TRUE)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = ACmap,
              fillColor = ~pal3(perc_AllFamUnder35kE),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              smoothFactor = 0.3,
              highlight = highlightOptions(
                weight = 1, fillOpacity = 0.8, bringToFront = T
              ),
              popup = paste0("Percent of families: ", ACmap$perc_AllFamUnder35kE, "%")) %>% 
  addLegend("bottomright", pal = pal3, values = ACmap$perc_AllFamUnder35kE, 
            title = "Percent of families <br> making < $35,000", opacity = 0.7)

# code chunk 47
hs_long <- countydat %>% 
  filter(countyName != "Whole Region") %>% 
  select(countyName, noHSE, perc_noHSE) %>% 
  pivot_longer(-countyName, names_to = "status", values_to = "value") %>% 
  mutate(status = factor(status, levels = c("perc_noHSE", "noHSE"),
                         labels = c("Percent No HS Degree", "Number No HS Degree")),
         countyName = factor(countyName))

ggplot(hs_long, aes(x = countyName, y = value)) +
  geom_col() +
  facet_wrap(~status, scales = "free_y", ncol = 1) +
  labs(x = "", y = "") +
  geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.5, size=2.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Or separately and then patch
hs_long <- countydat %>% 
  filter(countyName != "Whole Region") %>% 
  select(countyName, noHSE, perc_noHSE) %>% 
  pivot_longer(-countyName, names_to = "status", values_to = "value") %>% 
  mutate(status = factor(status, levels = c("perc_noHSE", "noHSE"))) 

hsnum <- hs_long %>% filter(status == "noHSE") %>% 
  ggplot(aes(x = countyName, y = value)) +
  geom_col() +
  labs(title = "Number", x = "", y = "") +
  geom_text(aes(label=comma(value)), 
            position=position_dodge(width=0.9), vjust=-0.5, size=2.5) +
  scale_y_continuous(labels = label_comma(), limits = c(0,6000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hsper <- hs_long %>% filter(status == "perc_noHSE") %>% 
  ggplot(aes(x = countyName, y = value)) +
  geom_col() +
  labs(title = "Percent", x = "", y = "") +
  geom_text(aes(label=percent(value, scale = 1, accuracy = 1)), 
            position=position_dodge(width=0.9), vjust=-0.5, size=4) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0,20)) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

hsper / hsnum +  plot_annotation(title = "Adults Over 25 without a High School Diploma")

# lollipop version
hsnum_pop <- hs_long %>% filter(status == "noHSE") %>% 
  mutate(countyName = fct_rev(countyName)) %>% 
  ggplot(aes(x = value, y = countyName)) +
  geom_segment(aes(x=0, xend=value, y=countyName, yend=countyName), 
               color = "grey") + 
  geom_point(size = 8, color = "deepskyblue4") +
  labs(title = "Number", x = "", y = "") +
  geom_text(aes(label=comma(value)), 
            position=position_dodge(width=0.9), 
            vjust=0.25, size=2.5, color = "white") +
  scale_x_continuous(labels = label_comma()) +
  theme_minimal()

hsperc_pop <- hs_long %>% filter(status == "perc_noHSE") %>% 
  mutate(countyName = fct_rev(countyName)) %>% 
  ggplot(aes(x = value, y = countyName)) +
  geom_segment(aes(x=0, xend=value, y=countyName, yend=countyName), 
               color = "grey") + 
  geom_point(size = 8, color = "deepskyblue4") +
  labs(title = "Percent", x = "", y = "") +
  geom_text(aes(label=percent(value, scale = 1)), 
            position=position_dodge(width=0.9), 
            vjust=0.25, size=2.5, color = "white") +
  scale_x_continuous(labels = label_percent()) +
  theme_minimal() +
  theme(axis.text.y = element_blank())

hsnum_pop + hsperc_pop +plot_annotation(title = "Adults Over 25 without a High School Diploma")


# pie chart
library(ggforce)

piedf <- countydat %>% 
  filter(countyName == "Whole Region") %>% 
  select(AllFamE, AllFamUnder35kE) %>% 
  mutate(AllFamOver35kE = AllFamE - AllFamUnder35kE) %>% 
  select(-AllFamE) %>% 
  pivot_longer(everything(), names_to = "inc", values_to = "num") %>% 
  mutate(percent = round((num/sum(num))*100,0),
         ypos = cumsum(percent) - 0.5*percent,
         inc = factor(inc, levels = c("AllFamOver35kE", "AllFamUnder35kE"),
                      labels = c("Self-Sufficient\n 86%", "Not\n Self-Sufficient\n 14%")))

piedf %>%
  mutate(focus = ifelse(inc == "AllFamUnder35kE", -0.1, 0)) %>%
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 0,
               amount = num, fill = inc, explode = focus),
               stat = "pie") +
  theme_no_axes()


ggplot(piedf, aes(x = 1, y = percent, fill = inc)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, x = 1.15, label = inc),
            color = "white", size=4) +
  guides(fill = "none") +
  theme_void()

library(waffle)

ggplot(piedf, aes(fill = inc, percent, values = percent)) +
  geom_waffle(n_rows = 5, color = "white", flip = FALSE) +
  coord_equal() +
  scale_fill_manual(values = c("#969696", "#009bda", "#c7d4b6", "#97b5cf"),
                    name = "") +
  theme_void() +
  theme(legend.position = "none")

wafdf1 <- c(86, 14)
names(wafdf1) <- c("Earn Family\nSufficient Wages", "Do Not Earn Family\n-Sufficient Wages")
w1 <- waffle(wafdf1, rows = 5, size = 0.5,
       colors = c("gray", "deepskyblue3"),
       legend_pos = "left", flip = TRUE, reverse = TRUE)

wafdf2 <- c(73, 27)
names(wafdf2) <- c("Family-Sufficient Wages\n within Reach", "Family-Sufficient Wages\n not in Reach")
w2 <- waffle(wafdf2, rows = 5, size = 0.5,
       colors = c("deepskyblue4", "deepskyblue"),
       legend_pos = "right", flip = TRUE, reverse = TRUE)

w1 + w2

