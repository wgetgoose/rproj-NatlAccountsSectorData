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
tourism_current_account <- read_excel("UNWTO_SIDS complementary_indicators.xlsx", 
                                                  sheet = "current account prop", skip = 1)
#write.xlsx(tourism_current_account, "Inbound Tourism Over Current Account Credits (1995-2022).xlsx")

current_account_share <- tourism_current_account %>% 
  filter(COUNTRY %in% c("Maldives", "Mauritius", "Dominican Republic", "Fiji", "Jamaica"))
write.xlsx(current_account_share, "Tourism Share of Current Account Credits for Cases (1995-2022).xlsx")
current_account_share <- current_account_share %>% 
  mutate(sum = rowSums(across(2:29))) %>% 
  mutate(`tourism share of current account credit` = sum/28) %>% 
  select(COUNTRY, `tourism share of current account credit`)
write.xlsx(export_share, "Average Current Account Credit Share for Cases (1995-2022).xlsx")
