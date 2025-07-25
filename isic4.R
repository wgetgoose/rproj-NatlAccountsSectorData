library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(stringr)
library(readxl)
library(openxlsx)
library(rlang)
library(forcats)
setwd("~/Desktop")
rev_4 <- read_excel("ISIC4_tidied.xlsx")
rev_4_targets <- c("Agriculture", "Industry", "Construction", "Transportation", "Hotel and Resturant", "Financial", "Real Estate", "Other: Public", "Other: Private")
#Revision 4 tidying
hotel_filter_rev_4 <- rev_4 %>%
  filter(!is.na(`Hotel and Resturant`)) %>% 
  filter(`Hotel and Resturants` < 5 & `Hotel and Resturants` > -5)

ggplot(data = hotel_filter_rev_4, aes(y = `Hotel and Resturants`, x = Year))+
  geom_col()+
  facet_wrap(~`Country`)+
  theme_light()+
  labs(title = "relative growth contribution of the hotel and resturant industry")
#regressions attempts (didnt work)
ggplot(data = rev_4, aes(x = `Hotel and Resturants`, y = Transportation))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  scale_x_continuous(limits = c(-5, 5))+
  scale_y_continuous(limits = c(-5, 5))

ggplot(data = ISIC4_growth_tided, aes(x = `Hotel and Resturants`, y = Construction))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  scale_x_continuous(limits = c(-5, 5))+
  scale_y_continuous(limits = c(-5, 5))
transportation_reg <- lm(Transportation ~ `Hotel and Resturants`, data = ISIC4_growth_tided, method = "qr")
construction_reg <- lm(Construction ~ `Hotel and Resturants`, data = ISIC4_growth_tided, method = "qr")
#big sector visualization for revision 4 of SNAS
country_avgs <- rev_4 %>%
  group_by(`Country or Area`) %>% 
  summarize(across(all_of(rev_4_targets), ~ mean(.x, na.rm = T)), .groups = "drop") %>% 
  mutate(sum = rowSums(across(all_of(rev_4_targets)), na.rm = T))

write.xlsx(country_avgs, "isic4_country_averages.xlsx")

sector_long <- country_avgs %>% 
  select(-sum) %>% 
  pivot_longer(-`Country or Area`, names_to = "Sector", values_to = "Value")

ggplot(sector_long, aes(x = fct_relevel(Sector, rev_4_targets), y = Value, fill = fct_relevel(Sector, rev_4_targets))) +
  geom_col() +
  facet_wrap(~ `Country or Area`, scales = "free_y") +
  theme_minimal() +
  labs(title = "Relative Sector Growth Contributions by Country",
       x = "Sector",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Spectral", name = "Sector")+
  theme(legend.position = "right")
  #legend(y = Sector, fill = Sector)


