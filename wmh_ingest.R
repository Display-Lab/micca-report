library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(stringr)
library(zoo)

source('components_and_measures.R')

read_wmh_data <- function(filename){
  # Had to accomodate leading empty column
  # Date format in input data is bad
  col_classes <- cols_only(col_skip(),
                           col_character(), col_date(format="%m/%d/%Y"), col_character(),
                           col_character(), col_character(), col_character(),
                           col_character(), col_character(), col_character(), 
                           col_character())
  
  # Force column names and hope order doesn't change.
  col_names <- c("mrn","delivery_date","counseling","contra_choice","imm_method",
                 "contra_prov","larc_prov","payer","choice_date","peripartum_care")
  
  # Need to skip the first 19 rows where they've reproduced the example spreadsheet as reference.
  raw_data <- readr::read_csv(filename, col_types = col_classes, col_names=FALSE, na=c(""), skip=9)
  colnames(raw_data) <- col_names
  return(raw_data)
}

standardize_wmh_data <- function(raw_data){
  raw_data %>% 
    filter(!is.na(mrn))  %>%
    mutate_if(.predicate=is.character, tolower) %>%
    mutate(
      counseling      = ifelse(counseling == 'yes', TRUE, FALSE),
      payer           = sub(pattern = "molina", replacement = "medicaid", payer),
      contra_choice   = ifelse(contra_choice == "immediate pp", "unknown", contra_choice),
      imm_method      = sub(pattern="^none.*", "none", imm_method),
      imm_method      = ifelse(imm_method == "tubal ligation", "pptl", imm_method),
      larc_prov       = sub(pattern="^not provisioned.*", "not provisioned", larc_prov),
      contra_prov     = ifelse(contra_prov == "unknown", larc_prov, contra_prov),
      institution     = "WMH" ) %>%
    filter(payer %in% c("medicaid", "molina", "private", "other", "unknown"),
           contra_choice %in% c("immediate pp iud", "immediate pp nexplanon", "pptl", "other", "none", "unknown")) %>%
    select(mrn,delivery_date,counseling, contra_choice, imm_method, contra_prov, 
           larc_prov, payer, choice_date, peripartum_care)
}

# Read raw data
df_raw <- read_wmh_data("data/wmh_2020_Q1.csv")

# Standardize
df_std <- standardize_wmh_data(df_raw)

# Emit standardized data
write_csv(df_std, "wmh_std_2020_Q1.csv")

# Calculate components and measures
df_comp <- calculate_components(df_std)
df_measure <- calculate_measures(df_comp)

# Create MAPTG data
maptg <- make_maptg(df_measure, "WMH")

# Emit cleaned data to file
write_csv(maptg, "maptg_wmh_2020_Q1.csv")
