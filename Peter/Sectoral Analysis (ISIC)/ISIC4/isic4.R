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


