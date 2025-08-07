library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(rlang)
library(forcats)
library(broom)
SIDS <- c("American Samoa", "Antigua and Barbuda", "Haiti", "St. Kitts and Nevis", "Bahamas, The", "Jamaica", "St. Lucia", "Barbados", "Kiribati", "St. Vincent and the Grenadines", "Belize", "Maldives", "Seychelles", "Cabo Verde", "Marshall Islands", "Solomon Islands", "Comoros", "Micronesia, Fed. Sts.", "Suriname", "Cook Islands", "Mauritius", "Timor-Leste", "Cuba", "Nauru", "Tonga", "Dominica", "Niue", "Trinidad and Tobago", "Dominican Republic", "Palau", "Tuvalu", "Fiji", "Papua New Guinea", "Vanuatu", "Grenada", "Samoa", "Guinea-Bissau", "São Tomé and Príncipe", "Guyana", "Singapore")
#inbound tourism as share of exports
tourism_export_share <- read_excel("UNWTO_SIDS complementary_indicators.xlsx", 
                                                  sheet = "tidying export prop")
#write.xlsx(UNWTO_SIDS_complementary_indicators, "Export Share for SIDS (1995-2022).xlsx")

export_share <- UNWTO_SIDS_complementary_indicators %>% 
  filter(COUNTRY %in% c("Maldives", "Mauritius", "Dominican Republic", "Fiji", "Jamaica"))
#write.xlsx(export_share, "Export Share for Cases (1995-2022).xlsx")
export_share <- export_share %>% 
  mutate(sum = rowSums(across(2:29))) %>% 
  mutate(`tourism share of exports` = sum/28) %>% 
  select(COUNTRY, `tourism share of exports`)
#write.xlsx(export_share, "Average Export Share for Cases (1995-2022).xlsx")

#inbound tourism over current account credits
#tourism_current_account <- read_excel("UNWTO_SIDS complementary_indicators.xlsx", 
                                                  sheet = "current account prop", skip = 1)
#write.xlsx(tourism_current_account, "Inbound Tourism Over Current Account Credits (1995-2022).xlsx")
#tourism_current_account <- read_excel("Inbound Tourism Over Current Account Credits (1995-2022).xlsx")


current_account_share <- tourism_current_account %>% 
  filter(COUNTRY %in% c("Maldives", "Mauritius", "Dominican Republic", "Fiji", "Jamaica"))
write.xlsx(current_account_share, "Tourism Share of Current Account Credits for Cases (1995-2022).xlsx")
current_account_share <- current_account_share %>% 
  mutate(sum = rowSums(across(2:29))) %>% 
  mutate(`tourism share of current account credit` = sum/28) %>% 
  select(COUNTRY, `tourism share of current account credit`)
#write.xlsx(export_share, "Average Current Account Credit Share for Cases (1995-2022).xlsx")

#current account vs. change in number of beds
SIDS_current_account <- API_BN_CAB_XOKA_CD_DS2_en_excel_v2_21690 %>% 
  filter(`Country Name` %in% SIDS) %>% 
  rename(Country = `Country Name`, Series = `Indicator Name`)
SIDS_current_account <- SIDS_current_account %>% 
  pivot_longer(cols = 5:40, names_to = "Year", values_to = "ca_balance") %>% 
  select(Country, Year, ca_balance)

#Occupancy tidying
Rooms <- UNWTO_accomadation_data %>% 
  filter(Series == "Number of rooms")
Rooms <- Rooms %>% 
  pivot_longer(cols = 4:31, names_to = "Year", values_to = "rooms") %>% 
  select(Country, Year, rooms)
#change in rooms
#Rooms <- Rooms %>% 
#  group_by(Country) %>% 
#  mutate(delta_rooms = rooms - lag(rooms)) %>% 
#  select(-rooms)

#joining rooms and current account
rooms_current_account <- left_join(SIDS_current_account, Rooms) %>% arrange(Country, Year)

#adding hospitality relative growth contribution and inbound tourism expenditure/ current account credits
ISIC4_tidied <- ISIC4_tidied %>% 
  mutate( Year = as.character(Year)) %>% 
  select(`Country or Area`, Year, `Hotel and Resturant`) %>% 
  rename(Country = `Country or Area`, Hospitality = `Hotel and Resturant`)
  
rooms_current_account <- left_join(rooms_current_account, ISIC4_tidied)

ISIC_Jamacia <- ISIC_Jamacia %>% 
  mutate(Year = as.character(Year)) %>% 
  select(`Country or Area`, Year, `Hotels and restaurants`) %>% 
  rename(Country = `Country or Area`, Hotel = `Hotels and restaurants`)

rooms_current_account <- left_join(rooms_current_account, ISIC4_tidied)
#current accout credits share from UNWTO
Current_Account_Credits_Share_for_SIDS_1995_2022_ <- read_excel("Current Account Credits Share for SIDS (1995-2022).xlsx")
temp <- Current_Account_Credits_Share_for_SIDS_1995_2022_ %>% 
  pivot_longer(cols = 2:29, names_to = "Year", values_to = "tourism_over_ca_credit") %>% 
  rename(Country = COUNTRY)
rooms_current_account <- left_join(rooms_current_account, temp)
#UNCTAD product concentration index
conc_1 <- US.ConcentDiversIndices_20250730_194556 %>% 
  select(-ends_with("Footnote")) %>% 
  select(-ends_with("MissingValue"))
conc_2 <- conc_1 %>% 
  pivot_longer(cols = starts_with("X"), values_to = "Concentration Index", names_to = "Year", names_prefix = "X") %>% 
  rename(Country = Economy_Label)
conc_2$Year <- str_remove(conc_2$Year, "_Concentration_Index_Value")
rooms_current_account <- left_join(rooms_current_account, conc_2)

write.xlsx(rooms_current_account, "current_account_tourism_regressions.xlsx")
rooms_current_account <- read_excel("current_account_tourism_regressions.xlsx")

rooms_current_account <- rooms_current_account %>% 
  mutate(delta_rooms = rooms-lag(rooms)) %>% 
  mutate(rooms = (delta_rooms/lag(rooms)*100)) %>% 
  select(-delta_rooms)
  

#linear regression
results <- rooms_current_account %>% 
  filter(!is.na(ca_balance), !is.na(rooms)) %>%   # drop NAs
  nest_by(Country) %>% 
  mutate(model = list(lm(ca_balance ~ rooms, data = data)),
         tidied = list(tidy(model))) %>% 
  unnest(tidied) %>% 
  select(Country, term, estimate, std.error, statistic, p.value)
coef <- results %>% 
  select(Country, term, estimate, std.error, statistic, p.value) %>% 
  filter(term == "rooms") %>% 
  arrange(p.value)

#graphing of that regression  
rm_sing <- rooms_current_account %>% 
  filter(Country != "Singapore")
ggplot(data = rm_sing, aes(x = rooms, y = ca_balance))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~Country)
#furhter graphing
ggplot(data = rooms_current_account, aes(x = tourism_over_ca_credit, y = rooms))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~Country)+
  xlab("Inbound Tourism Expenditure over Current Account Credits")+
  ylab("Annual Percentage Change in Number of Hotel Rooms")

plot_tourism_vs_rooms(rooms_current_account, c("Barbados", "Belize", "Cabo Verde", "Maldives", "Fiji", "Mauritius", 
                                               "Comoros", "Dominica", "Dominican Republic", "Fiji", "Grenada", "Guyana", "Jamaica", 
                                               "Kiribati", "Maldvies", 
                                               "Mauritius", "Palau", "Signapore", "Solomon Islands", "Timor-Leste", "Seychelles"))


plot_tourism_vs_rooms <- function(data, countries, width = 7, height = 5) {
  # Loop through each country
  for (country in countries) {
    
    # Filter for one country
    country_data <- data %>% 
      filter(Country == country)
    
    # Create plot
    p <- ggplot(country_data, aes(x = tourism_over_ca_credit, y = rooms)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      xlab("Inbound Tourism Expenditure over Current Account Credits") +
      ylab("Annual Percentage Change in Number of Hotel Rooms") +
      ggtitle(paste("Tourism vs Rooms in", country))
    
    # Define filename
    file_name <- paste0("tourism_vs_rooms_", gsub(" ", "_", country), ".png")
    
    # Save plot
    ggsave(filename = file_name, plot = p, width = width, height = height)
  }
}

  