library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# prompts user to open excel file
ISIC3_orig <- read_excel(file.choose())

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
target_sna_codes <- c("A+B", "A", "B", "C", "D", "E", "F", "G", "H", "G+H", "I", "J",
                      "K", "J+K", "L", "M", "N", "O", "M+N+O", "P")

# Move SNA93 Item Codes into Columns instead of Rows in new DF
ISIC3_wider <- ISIC3 %>%
  filter(`SNA93 Item Code` %in% target_sna_codes) %>%
  pivot_wider(
    names_from = `SNA93 Item Code`,
    values_from = `Value` 
  )

ISIC3_filter_for_na <- ISIC3_wider %>%
  group_by(`Country or Area`, `Year`) %>%
  filter(if_any(all_of(target_sna_codes), is.na)) %>%
  select_if(~ any(is.na(.)))

# SNA codes we're going to need to remove due to potential double counting later
removal_sna_codes <- c("A","B","G","H","M","N","O")

ISIC3_growth <- ISIC3_wider %>%
  # Tabulate TVA by summing together all sectors and removing double counting
  rowwise() %>%
  mutate (
    # should this sum equation be a function later? yes
    total_value_added = sum(across(all_of(target_sna_codes)), na.rm=TRUE)
    - sum(across(all_of(removal_sna_codes)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  
  # Calculate relative growth values, deltaSector/deltaTVA
  group_by(`Country or Area`) %>%
  arrange(`Year`) %>%
  mutate (
    change_in_TVA = `total_value_added` - lag(`total_value_added`, n = 1),
    across(
      .cols = all_of(target_sna_codes),
      .fns = ~ (. - lag(., n = 1)) / change_in_TVA
    )
  ) %>%
  ungroup() %>%
  
  # get rid of temp variable
  select(-change_in_TVA) %>%
  
  # tabulate sum of all relative growth (%) values. should equal 1 (100%) 
  rowwise() %>%
  mutate (
    # should this sum equation be a function later? yes
    sector_sums = sum(across(all_of(target_sna_codes)), na.rm=TRUE) 
    - sum(across(all_of(removal_sna_codes)), na.rm=TRUE)
  ) %>%
  ungroup() %>%
  arrange(`Country or Area`, `Year`)

# Delete calculated TVA from output 
ISIC3_growth$total_value_added <- NULL

# Filter out base years. because all countries report category A+B, just filter out the blank A+B years
ISIC3_growth <- ISIC3_growth %>% filter(!is.na(`A+B`))



# Write to file
write.xlsx(ISIC3_growth, file.choose())