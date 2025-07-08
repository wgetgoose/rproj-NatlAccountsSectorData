library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# fix definite this path later
ISIC3_orig <- read_excel("data.un.org data/National Accounts Official Country Data/Table2.1 ValueAddCurrPrices ISIC3/MODIFIED_ValueAddedCurrentPrices_1970_2023/Table2.1_ValueAddedCurrPrices_1970_2023.xlsx")

# Only use the highest series available and only 1993 SNA system
ISIC3 <- ISIC3_orig %>%
  filter(`SNA System` == 1993) %>%
  group_by(`Country or Area`,`Year`,`SNA93 Item Code`) %>%
  slice_max(order_by = Series, n = 1, with_ties = FALSE) %>%
  ungroup()

# Remove unused cols 
ISIC3$`Item` <- NULL
ISIC3$`Value Footnotes` <- NULL
ISIC3$Series <- NULL
ISIC3$`SNA System` <- NULL

# Only take the SNA93 codes we care about, for now
# !! PROBLEM !! Some countries only report "J+K", not "J" and "K" isolated. Same for G+H
target_sna_codes <- c("A+B", "C", "D", "E", "F", "G", "H", "I", "J",
                      "K", "L", "M+N+O", "P", "B.1g")

# Move SNA93 Item Codes into Columns instead of Rows in new DF
ISIC3_wider <- ISIC3 %>%
  filter(`SNA93 Item Code` %in% target_sna_codes) %>%
  pivot_wider(
    names_from = `SNA93 Item Code`,
    values_from = `Value` 
  )

# Calculate Relative Growth Values in new DF
ISIC3_growth <- ISIC3_wider %>%
  # Tabulate TVA by summing together all sectors
  rowwise() %>%
  mutate (
    total_value_added = sum(across(c(`A+B`, `C`, `D`, `E`, `F`, `G`, `H`, `I`, `J`, `K`, `L`, `M+N+O`, `P`)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  
  # Calculate relative growth values, deltaSector/deltaTVA
  group_by(`Country or Area`) %>%
  arrange(`Year`) %>%
  mutate (
    change_in_TVA = `total_value_added` - lag(`total_value_added`, n = 1),
    across(
      .cols = c(`A+B`, `C`, `D`, `E`, `F`, `G`, `H`, `I`, `J`, `K`, `L`, `M+N+O`, `P`),
      .fns = ~ (. - lag(., n = 1)) / change_in_TVA
    )
  ) %>%
  ungroup() %>%
  
  # get rid of temp variable
  select(-change_in_TVA) %>%
  
  # Sum all the sectors to see if they add to 100%
  rowwise() %>%
  mutate (
    sector_sums = sum(across(c(`A+B`, `C`, `D`, `E`, `F`, `G`, `H`, `I`, `J`, `K`, `L`, `M+N+O`, `P`)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  arrange(`Country or Area`, `Year`)

# Delete reported GVA and calculated TVA from output 
ISIC3_growth$B.1g <- NULL
ISIC3_growth$total_value_added <- NULL

# Filter out base years, A+B is NA in a base year
ISIC3_growth <- ISIC3_growth %>% filter(!is.na(`A+B`))

# Write to file
write.xlsx(ISIC3_growth, "ISIC3_growth.xlsx")