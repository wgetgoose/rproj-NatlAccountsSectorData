library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

# prompts user to open excel file
ISIC4_orig <- read_excel(file.choose())

# Only use the highest series available
ISIC4 <- ISIC4_orig %>%
  # uhh should we slice SNA system?? let's just see
  filter(`SNA System` == 2008) %>%
  group_by(`Country or Area`,`Year`,`SNA93 Item Code`) %>%
  slice_max(order_by = Series, n = 1, with_ties = FALSE) %>%
  ungroup()

# Remove unused cols 
ISIC4$`Item` <- NULL
ISIC4$`Value Footnotes` <- NULL
ISIC4$`Series` <- NULL
ISIC4$`SNA System` <- NULL

# Only take the SNA93 codes we care about, for now
target_sna_codes <- c("A","B","C","D","E","B+C+D+E","F","G",
                      "H","I","J","K","L","M","N","M+N","O",
                      "P","Q","O+P+Q","R","S","T","R+S+T")



# Move SNA93 Item Codes into Columns instead of Rows in new DF
ISIC4_wider <- ISIC4 %>%
  filter(`SNA93 Item Code` %in% target_sna_codes) %>%
  pivot_wider(
    names_from = `SNA93 Item Code`,
    values_from = `Value` 
  )


# Creates a dataframe to filter for countries with missing categories
ISIC4_filter_for_na <- ISIC4_wider %>%
  group_by(`Country or Area`, `Year`) %>%
  filter(if_any(all_of(target_sna_codes), is.na)) %>%
  select_if(~ any(is.na(.)))

# SNA codes we're going to need to remove due to potential double counting later
removal_sna_codes <- c("B","C","D","E","M","N","O","P","Q","R","S","T")

# New DF for relative growth values
ISIC4_growth <- ISIC4_wider %>%
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
ISIC4_growth$total_value_added <- NULL

# Filter out base years. because all countries report category A, just filter out the blank A years
ISIC4_growth <- ISIC4_growth %>% filter(!is.na(`A`))

# write to excel file
write.xlsx(ISIC4_growth, file.choose())