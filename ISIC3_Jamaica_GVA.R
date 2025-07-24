library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# prompts user to open excel file
Jamaica_orig <- read_excel(file.choose())

# For Jamaica: Categories D, E, F, H, 60-63 (Transportation), 64 (Communications), 
#                         J, K 
target_sna_codes <- c("Manufacturing", "Electricity, gas, water supply", "Construction",
                      "Hotels and restaurants", "Transportation", "Communications",
                      "Financial Intermediation", "Real Estate")

Jamaica_growth <- Jamaica_orig %>%
  # Calculate relative growth values, deltaSector/deltaGVA
  group_by(`Country or Area`) %>%
  arrange(`Year`) %>%
  mutate (
    change_in_GVA = `Gross Value Added` - lag(`Gross Value Added`, n = 1),
    across(
      .cols = all_of(target_sna_codes),
      .fns = ~ (. - lag(., n = 1)) / change_in_GVA 
    )
  ) %>%
  ungroup() %>%
  
  # get rid of temp variable
  select(-change_in_GVA) %>%
  arrange(`Country or Area`, `Year`)

# Write to file
write.xlsx(Jamaica_growth, file.choose())