library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(stringr)
library(readxl)
library(openxlsx)
sids_sectors <- read_excel("wb-gdp-component-current-price.xlsx")
un_gdp_components <- read_excel("un-gdp-components.xlsx")
#create year col
long <- sids_sectors %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(year))
#breakout series into discrete columns
wide <- long %>%
  pivot_wider(
    id_cols = c(`Country Name`, `Country Code`, year),
    names_from = `Series Name`,
    values_from = value)

un_join <- wide %>% 
  rename(country = `Country Name`) %>% 
  select(`country`, deflator, deflator_linked, year) %>% 
  filter(year > 1969)

un_components_deflator <- left_join(un_gdp_components, un_join)
#deflating
deflated <- un_components_deflator %>% 
  filter(!is.na(deflator)) %>%
  filter(!is.na(imports)) %>%
  filter(!is.na(exports)) %>%
  filter(!is.na(consumption)) %>%
  filter(!is.na(investment)) %>%
  filter(!is.na(government)) %>%
  mutate(exports = (exports*deflator)/100) %>% 
  mutate(imports = (imports*deflator)/100) %>% 
  mutate(consumption = (consumption*deflator)/100) %>% 
  mutate(government = (government*deflator)/100) %>% 
  mutate(investment = (investment*deflator)/100) %>% 
  mutate(gdp = (gdp*deflator)/100) %>%
  select(-deflator&-deflator_linked)
#computing change in vars
net_sids <- deflated %>%
  mutate(delta_exports = exports - lag(exports)) %>% 
  mutate(delta_imports = imports - lag(imports)) %>% 
  mutate(delta_consumption = consumption - lag(consumption)) %>% 
  mutate(delta_investment = investment - lag(investment)) %>% 
  mutate(delta_government = government - lag(government)) %>% 
  mutate(delta_gdp = gdp - lag(gdp))
    
#growth contribution of each sector
prop <- net_sids %>% 
  mutate(net_exports = delta_exports-delta_imports) %>% 
  mutate(delta_net_exports = net_exports/lag(gdp)) %>% 
  mutate(delta_consumption = delta_consumption/lag(gdp)) %>% 
  mutate(delta_government = delta_government/lag(gdp)) %>% 
  mutate(delta_investment = delta_investment/lag(gdp)) %>% 
  mutate(delta_gdp = delta_gdp/lag(gdp))

#tidying  
prop_tidy <- prop %>% 
  select(c("country", "year", "delta_gdp", "delta_net_exports", "delta_consumption", "delta_government", "delta_investment")) %>% 
  mutate(sum = delta_net_exports + delta_consumption + delta_government + delta_investment) %>% 
  mutate(dif = delta_gdp-sum)

#proportion of growth
frac <- prop_tidy %>% 
  mutate(percent_gdp = delta_gdp/delta_gdp) %>% 
  mutate(percent_consumption = delta_consumption/delta_gdp) %>% 
  mutate(percent_investment = delta_investment/delta_gdp) %>% 
  mutate(percent_government = delta_government/delta_gdp) %>% 
  mutate(percent_net_exports = delta_net_exports/delta_gdp) %>%  
  select(c("country", "year", "percent_gdp", "percent_net_exports", 
           "percent_consumption", "percent_government", "percent_investment")) %>% 
  mutate(component_sum = percent_consumption + percent_investment + percent_government + percent_net_exports) %>% 
  filter(year > 1970)

#tests/checks
frac %>% 
  filter(component_sum <= 1.1 & component_sum >= 0.9) %>% 
  ggplot(aes(x = component_sum))+
  geom_histogram()

#periodization
`70-90` <- frac %>% 
  filter(year > 1969 & year < 1991) %>% 
  group_by(country) %>% 
  drop_na() %>% 
  summarize(percent_consumption = mean(percent_consumption), percent_net_exports = mean(percent_net_exports),
            percent_government = mean(percent_government), percent_investment = mean(percent_investment)) %>% 
  mutate(sum = percent_consumption + percent_net_exports + percent_government + percent_investment)
pre_2008 <- frac %>% 
  filter(year > 1991 & year < 2008) %>% 
  group_by(country) %>% 
  drop_na() %>% 
  summarize(percent_consumption = mean(percent_consumption), percent_net_exports = mean(percent_net_exports),
            percent_government = mean(percent_government), percent_investment = mean(percent_investment)) %>% 
  mutate(sum = percent_consumption + percent_net_exports + percent_government + percent_investment)
post_2008 <- frac %>% 
  filter(year > 2008) %>% 
  group_by(country) %>% 
  drop_na() %>% 
  summarize(percent_consumption = mean(percent_consumption), percent_net_exports = mean(percent_net_exports),
            percent_government = mean(percent_government), percent_investment = mean(percent_investment)) %>% 
  mutate(sum = percent_consumption + percent_net_exports + percent_government + percent_investment)
write.xlsx(`70-90`, "UN-19**-1990-deflated.xlsx")
write.xlsx(pre_2008, "UN-1991-2007-deflated.xlsx")
write.xlsx(post_2008, "UN-2009-2024-deflated.xlsx")

