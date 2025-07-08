library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# yeah fix this path later
ISIC4_orig <- read_excel("data.un.org data/National Accounts Official Country Data/Table2.1 ValueAddCurrPrices ISIC3/MODIFIED_ValueAddedCurrentPrices_1970_2023/Table2.1_ValueAddedCurrPrices_1970_2023.xlsx")
# Only use the highest series available
ISIC4 <- ISIC4_orig %>%
  filter(`SNA System` == 2008) %>%
  group_by(`Country or Area`,`Year`,`SNA93 Item Code`) %>%
  slice_max(order_by = Series, n = 1, with_ties = FALSE) %>%
  ungroup()

# Remove unused cols 
ISIC4$`Item` <- NULL
ISIC4$`Value Footnotes` <- NULL
ISIC4$Series <- NULL
ISIC4$`SNA System` <- NULL

# Only take the SNA93 codes we care about, for now
target_sna_codes <- c("A+B", "C", "D", "E", "F", "G", "H", "I", "J",
                      "K", "L", "M+N+O", "P", "B.1g")

# Move SNA93 Item Codes into Columns instead of Rows in new DF
ISIC4_wider <- ISIC3 %>%
  filter(`SNA93 Item Code` %in% target_sna_codes) %>%
  pivot_wider(
    names_from = `SNA93 Item Code`,
    values_from = `Value` 
  )

# Calculate Relative Growth Values in new DF
ISIC3_growth <- ISIC3_wider %>%
  group_by(`Country or Area`) %>%
  arrange(`Year`) %>%
  mutate (
    change_in_GVA = `B.1g` - lag(`B.1g`, n = 1),
    across(
      .cols = c(`A+B`, `C`, `D`, `E`, `F`, `G`, `H`, `I`, `J`, `K`, `L`, `M+N+O`, `P`),
      .fns = ~ (. - lag(., n = 1)) / change_in_GVA 
    )
  ) %>%
  # re-sorts the data by country and year ascending
  arrange(`Country or Area`, `Year`) %>%
  # get rid of temp variable
  select(-change_in_GVA) %>%
  ungroup()
# we don't need reported GVA for this DF 
ISIC3_growth$B.1g <- NULL
ISIC3_growth <- ISIC3_growth %>% filter(!is.na(`A+B`))

write.xlsx(ISIC3_growth, "ISIC3_growth.xlsx")