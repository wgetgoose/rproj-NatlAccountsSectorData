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
final <- read_excel("current_account_tourism_regressions.xlsx")

#inbound tourism as share of exports

UNWTO_accomadation_data <- read_excel("UNWTO_accomadation_data.xlsx", sheet = "tidying")

Rooms <- UNWTO_accomadation_data %>% 
  filter(Series == "Number of rooms")
Rooms <- Rooms %>% 
  pivot_longer(cols = 4:31, names_to = "Year", values_to = "rooms") %>% 
  select(Country, Year, rooms)
#join
final <- final %>% 
  select(-rooms)
final <- left_join(final, Rooms) %>% arrange(Country, Year)

#inbound tourism over current account credits
current_account_share <- read_excel("UNWTO_SIDS complementary_indicators_TIDY.xlsx", 
                                                                               sheet = "6.11", skip = 1)
current_account_share <- current_account_share %>% 
  pivot_longer(cols = 2:29, names_to = "Year", values_to = "tourism_over_ca_credit") %>% 
  rename(Country = COUNTRY)
#join
final <- final %>% 
  select(-tourism_over_ca_credit)
final <- left_join(final, current_account_share)

#current account balance
SIDS_current_account <- read_excel("SIDS current account balance (World Bank).xls", 
                                                       skip = 3)
SIDS_current_account <- SIDS_current_account %>% 
  filter(`Country Name` %in% SIDS) %>% 
  rename(Country = `Country Name`, Series = `Indicator Name`)
SIDS_current_account <- SIDS_current_account %>% 
  pivot_longer(cols = 5:69, names_to = "Year", values_to = "ca_balance") %>% 
  select(Country, Year, ca_balance)
#join
final <- final %>% 
  select(-ca_balance)
final <- left_join(final, SIDS_current_account) %>% arrange(Country, Year)


#hospitality relative growth contribution and inbound tourism expenditure/ current account credits
ISIC4_tidied <- read_excel("~/Desktop/RA/rproj-NatlAccountsSectorData/Peter/Sectoral Analysis (ISIC)/ISIC4/ISIC4_tidied.xlsx")
ISIC4_tidied <- ISIC4_tidied %>% 
  mutate( Year = as.character(Year)) %>% 
  select(`Country or Area`, Year, `Hospitality`) %>% 
  rename(Country = `Country or Area`)

ISIC_Jamacia <- read_excel("~/Desktop/RA/rproj-NatlAccountsSectorData/Peter/Sectoral Analysis (ISIC)/ISIC3/ISIC Jamacia.xlsx")
ISIC_Jamacia <- ISIC_Jamacia %>% 
  mutate(Year = as.character(Year)) %>% 
  select(`Country or Area`, Year, `Hotels and restaurants`) %>% 
  rename(Country = `Country or Area`, Hospitality = `Hotels and restaurants`)

ISIC <- full_join(ISIC4_tidied, ISIC_Jamacia)

final <- final %>% 
  select(-Hospitality)
final <- left_join(final, ISIC)

#UNCTAD product concentration index
US_ConcentDiversIndices_20250730_194556 <- read_csv("concentration index (UNCTAD)/US.ConcentDiversIndices_20250730_194556.csv")
conc_1 <- US_ConcentDiversIndices_20250730_194556 %>% 
  select(-ends_with("Footnote")) %>% 
  select(-ends_with("MissingValue"))
conc_2 <- conc_1 %>% 
  pivot_longer(cols = 2:31, values_to = "Concentration Index", names_to = "Year") %>% 
  rename(Country = Economy_Label)
conc_2$Year <- str_remove(conc_2$Year, "_Concentration_Index_Value")
final <- final %>% 
  select(-`Concentration Index`)
final <- left_join(final, conc_2)

write.xlsx(final, "current_account_tourism_regressions.xlsx")
rooms_current_account <- read_excel("current_account_tourism_regressions.xlsx")

rooms_current_account <- rooms_current_account %>% 
  mutate(delta_rooms = rooms-lag(rooms)) %>% 
  mutate(rooms = (delta_rooms/lag(rooms)*100)) %>% 
  select(-delta_rooms)
  

#linear regression
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
  xlab("Inbound Tourism Expenditure over Current Account Credits")+
  ylab("Annual Percentage Change in Number of Hotel Rooms")

plot_tourism_vs_rooms(rooms_current_account, c("Barbados", "Belize", "Cabo Verde", "Maldives", "Fiji", "Mauritius", 
                                               "Comoros", "Dominica", "Dominican Republic", "Fiji", "Grenada", "Guyana", "Jamaica", 
                                               "Kiribati", "Maldvies", 
                                               "Mauritius", "Palau", "Singapore", "Solomon Islands", "Timor-Leste", "Seychelles"))

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
      ggtitle(paste(country))
    
    # Define filename
    file_name <- paste0("tourism_vs_rooms_", gsub(" ", "_", country), ".png")
    
    # Save plot
    ggsave(filename = file_name, plot = p, width = width, height = height)
  }
}
rooms_current_account %>% 
  group_by(Country) %>% 
  summarize(Hospitality = mean(Hospitality, na.rm = T)) %>% 
  View()
  