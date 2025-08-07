library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(rlang)
library(forcats)
rev_4 <- read_excel("ISIC4_tidied.xlsx")
rev_4_targets <- c("Primary", "Industry", "Construction", "Transportation", "Hospitality", "Financial", "Real Estate", "Other: Public", "Other: Private")

#big sector visualization for revision 4 of SNAS
country_avgs <- rev_4 %>%
  group_by(`Country or Area`) %>% 
  summarize(across(all_of(rev_4_targets), ~ mean(.x, na.rm = T)), .groups = "drop") %>% 
  mutate(sum = rowSums(across(all_of(rev_4_targets)), na.rm = T))

country_avgs_2005_2022 <- rev_4 %>%
  group_by(`Country or Area`) %>% 
  filter(Year > 2004 & Year < 2023) %>% 
  summarize(across(all_of(rev_4_targets), ~ mean(.x, na.rm = T)), .groups = "drop") %>% 
  mutate(sum = rowSums(across(all_of(rev_4_targets)), na.rm = T))

write.xlsx(country_avgs_2005_2022, "isic4_country_averages (2005-2022).xlsx")

sector_long <- country_avgs %>% 
  select(-sum) %>% 
  pivot_longer(-`Country or Area`, names_to = "Sector", values_to = "Value")
sector_long_2005_2022 <- country_avgs_2005_2022 %>% 
  select(-sum) %>% 
  pivot_longer(-`Country or Area`, names_to = "Sector", values_to = "Value")

ggplot(data = sector_long_2005_2022, aes(x = fct_relevel(Sector, rev_4_targets), y = (Value*100), fill = fct_relevel(Sector, rev_4_targets))) +
  geom_col() +
  facet_wrap(~ `Country or Area`, scales = "free_y") +
  theme_minimal() +
  labs(x = "Sector",
       y = "Value (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Spectral", name = "Sector")+
  theme(legend.position = "right")

#individual graphs for the Madlives, Mauritius and the Seychelles
sector_long %>% 
  filter(`Country or Area` == "Maldives") %>% 
  ggplot(aes(x = fct_relevel(Sector, rev_4_targets), y = (Value*100), fill = fct_relevel(Sector, rev_4_targets))) +
  geom_col() +
  theme_minimal() +
  labs(x = "Sector",
       y = "Value (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Spectral", name = "Sector")+
  theme(legend.position = "right")

sector_long %>% 
  filter(`Country or Area` == "Mauritius") %>% 
  ggplot(aes(x = fct_relevel(Sector, rev_4_targets), y = (Value*100), fill = fct_relevel(Sector, rev_4_targets))) +
  geom_col() +
  theme_minimal() +
  labs(x = "Sector",
       y = "Value (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Spectral", name = "Sector")+
  theme(legend.position = "right")

sector_long %>% 
  filter(`Country or Area` == "Seychelles") %>% 
  ggplot(aes(x = fct_relevel(Sector, rev_4_targets), y = (Value*100), fill = fct_relevel(Sector, rev_4_targets))) +
  geom_col() +
  theme_minimal() +
  labs( x = "Sector",
       y = "Value (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Spectral", name = "Sector")+
  theme(legend.position = "right")

avail <- rev_4 %>% 
  group_by(`Country or Area`) %>% 
  summarise(min(Year), max(Year))

