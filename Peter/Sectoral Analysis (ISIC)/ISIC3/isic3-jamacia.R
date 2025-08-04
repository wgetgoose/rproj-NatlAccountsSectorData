library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(rlang)
library(forcats)
rev_3 <- read_excel("Table2.1_ISIC3_ValueAddedCurrPrices_1970_2023.xlsx")

#JAMACIA CASE STUDY

rev_3_jam <- rev_3 %>% 
  filter(`SNA System` == 1993) %>% 
  filter(`Country or Area` == "Jamaica") %>% 
  select(-`Item`) %>% 
  select(-Currency) %>% 
  select(-Series) %>% 
  select(-`Value Footnotes`)

jam_wide <- pivot_wider(data = rev_3_jam, names_from = "SNA93 Item Code", values_from = "Value")
jam_tidy <- jam_wide %>% 
  select(`Country or Area`, Year, `A+B`, C, D, E, `F`, G, H, `60-63`, `64`, J, K , 
         L, `M+N+O`, P, `P.119`, `D.21-D.31`, `D.21`, `B.1*g`, `B.1g`) %>% 
  mutate(sum = rowSums(across(`A+B`:P), na.rm = T))

jam_final <- jam_tidy %>% 
  mutate(Primary = `A+B` + C) %>% 
  rename(Manufacturing = D,`Electricity, gas, water supply` = E,Construction = `F`,`Hotels and restaurants` = H,
         Transportation = `60-63`,Communications = `64`,`Financial Intermediation` = J, 
         `Real Estate` = K,`Gross Value Added` = `B.1g`) %>% 
  mutate(`Other: Private` = rowSums(across(c(G, P)), na.rm = T)) %>% 
  mutate(`Other: Public` = L + `M+N+O`) %>% 
  select(-c("A+B", "C", "G", "P", "L", "M+N+O"))


#graphing for jamacia
target_sna_codes <- c("Primary", "Manufacturing", "Electricity, gas, water supply", "Construction",
                      "Hospitality", "Transportation", "Communications",
                      "Financial Intermediation", "Real Estate", "Other: Private", "Other: Public")
Jamaica_growth <- read_excel("ISIC Jamacia.xlsx")
isic3_jamacia <- Jamaica_growth %>% 
  filter(Year != 1998) %>% 
  select(-c("P.119", "D.21-D.31", "D.21", "B.1*g")) %>% 
  summarize(Primary = mean(Primary), Manufacturing = mean(Manufacturing), `Electricity, gas, water supply` = mean(`Electricity, gas, water supply`),
            Construction = mean(Construction), Transportation = mean(Transportation), `Hotels and restaurants` = mean(`Hotels and restaurants`),
            Communications = mean(Communications), `Financial Intermediation` = mean(`Financial Intermediation`),
            `Real Estate` = mean(`Real Estate`), `Other: commercial` = mean(`Other: commercial`), `Other: public` = mean(`Other: public`)) %>% 
  mutate(sum = Primary + Manufacturing + `Electricity, gas, water supply` + Construction + Transportation + `Hotels and restaurants` + Communications +
           `Financial Intermediation` + `Real Estate` + `Other: commercial`+ `Other: public`) %>% 
  rename(`Other: Private` = `Other: commercial`, `Other: Public` = `Other: public`, Hospitality = `Hotels and restaurants`)
write.xlsx(isic3_jamacia, "ISIC3 Jamacia average.xlsx")
ISIC3_Jamacia_average <- read_excel("ISIC3 Jamacia average.xlsx")

jam_long <- ISIC3_Jamacia_average %>% 
  select(-sum) %>% 
  pivot_longer(cols = 1:11, names_to = "Sector", values_to = "Value")


ggplot(jam_long, aes(x = fct_relevel(Sector, target_sna_codes), y = Value, fill = fct_relevel(Sector, target_sna_codes))) +
  geom_col() +
  theme_minimal() +
  labs(title = "Relative Sector Growth Contributions by Country",
       x = "Sector",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Spectral", name = "Sector")+
  theme(legend.position = "right")
