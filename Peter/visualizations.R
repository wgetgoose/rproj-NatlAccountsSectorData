library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(stringr)
library(readxl)
library(openxlsx)
#case selection aides
period_1 <- read_excel("UN-19**-1990-deflated.xlsx")
period_2 <- read_excel("UN-1991-2007-deflated.xlsx")
period_3 <- read_excel("UN-2009-2024-deflated.xlsx")

period_1 <- period_1 %>% 
  rowwise() %>% 
  mutate(nuc = list(names(across(2:5))), row_max = nuc[which.max(across(2:5))]) %>% 
  ungroup() %>% 
  select(-nuc)
period_2 <- period_2 %>% 
  rowwise() %>% 
  mutate(nuc = list(names(across(2:5))), row_max = nuc[which.max(across(2:5))]) %>% 
  ungroup() %>% 
  select(-nuc)
period_3<- period_3 %>% 
  rowwise() %>% 
  mutate(nuc = list(names(across(2:5))), row_max = nuc[which.max(across(2:5))]) %>% 
  ungroup() %>% 
  select(-nuc)

ggplot(data = period_1, aes(y = row_max, fill = row_max))+
  geom_bar()+
  facet_wrap(~region)
ggplot(data = period_2, aes(y = row_max, fill = row_max))+
  geom_bar()+
  facet_wrap(~region)
ggplot(data = period_3, aes(y = row_max, fill = row_max))+
  geom_bar()+
  facet_wrap(~region)

write.xlsx(period_1, "UN-19**-1990-deflated.xlsx")
write.xlsx(period_2, "UN-1991-2007-deflated.xlsx")
write.xlsx(period_3, "UN-2009-2024-deflated.xlsx")



#garbage code from trying to graph SNAS rev 3
isic3_long <- ISIC3_growth %>% 
  select(-Currency) %>% 
  select(-`sector_sums`) %>% 
  pivot_longer(
    cols = 3:15, names_to = "sector", values_to = "value") %>% 
  group_by(`Country or Area`, sector) %>% 
  summarize(value = mean(value))

ggplot(data = isic3_long, aes(y = sector, x = value)) + 
  geom_col()+
  facet_wrap(~`Country or Area`)

ggplot(data = ISIC3_growth, aes(x = Year, y = H))+
  geom_col()+
  facet_wrap(~`Country or Area`)

H <- ISIC3_growth %>% 
  group_by(`Country or Area`) %>% 
  summarize(H = mean(H))

ggplot(H, aes(x= H, y = `Country or Area`))+
  geom_col()