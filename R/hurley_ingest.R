library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(stringr)
library(zoo)

source('components_and_measures.R')

read_hurley_data <- function(filename){
  # Ingest hurley data
  col_classes <- cols_only(col_character(), col_date(), col_character(),
                           col_character(),col_character(),col_character(),
                           col_character(),col_character(),col_character(), 
                           col_character())
  
  # Force column names and hope order doesn't change.
  #col_names <- c("mrn","delivery_date","counseling","contra_choice","imm_method",
  #               "contra_prov","larc_prov","payer","choice_date")
  
  col_names <- c("mrn","delivery_date","counseling","contra_choice","imm_method",
                 "contra_prov","larc_prov","payer","choice_date","peripartum_care")
  
  raw_data <- readr::read_csv(filename, col_types = col_classes, na=c(""))
  colnames(raw_data) <- col_names
  return(raw_data)
}

standardize_hurley_data <- function(raw_data){
  # ASSUME: D7 larc_prov is "4-60 at pp visit" for "4-60 days"
  # Enforce choice: immediate PP IUD, immediate PP Nexplanon, PPTL, other, none, unknown
  # Correct "Unkown" to "unknown"
  # Recode choice to unknown for counseling == FALSE
  #  Currently unused: choice_date = ymd(base::strptime(choice_date, format="%Y-%m-%d")),
  raw_data %>% 
    filter(!is.na(mrn))  %>%
    mutate_if(.predicate=is.character, tolower) %>%
    mutate(
      counseling      = ifelse(counseling == 'yes', TRUE, FALSE),
      contra_choice   = ifelse(contra_choice == "unkown", "unknown", contra_choice),
      contra_choice   = ifelse(counseling, contra_choice, "unknown"),
      imm_method      = recode(.x=imm_method, "none during delivery admission"="none"),
      contra_prov     = sub("(\\d)days","\\1 days",contra_prov),
      larc_prov       = sub("(\\d)days","\\1 days",larc_prov),
      larc_prov       = ifelse(larc_prov == "4-60 days", "4-60 days pp visit", larc_prov),
      payer           = sub(pattern = "-.*$", replacement = "", payer),
      peripartum_care = case_when( peripartum_care == "yes" ~ TRUE,
                                   peripartum_care == "no" ~ FALSE),
      institution = "Hurley" ) %>%
    filter(payer %in% c("medicaid", "molina", "private", "other", "unknown"),
           contra_choice %in% c("immediate pp iud", "immediate pp nexplanon", "pptl", "other", "none", "unknown"))
}

# Measure Numerators in terms of component

# Read raw data
jan_raw <- read_hurley_data('data/hurley_2020-01.csv')
feb_raw <- read_hurley_data('data/hurley_2020-02.csv')
mar_raw <- read_hurley_data('data/hurley_2020-03.csv')


# standardize data
jan_data <- standardize_hurley_data(jan_raw)
feb_data <- standardize_hurley_data(feb_raw)
mar_data <- standardize_hurley_data(mar_raw)

# combine and emit standard data for future reference by hurley
norm_data <-  bind_rows(jan_data, feb_data, mar_data)
write_csv(norm_data, "hurley_std_2020_Q1.csv")

# Calc measures components by delivery month and payer
jan_comp <- calculate_components(jan_data)
feb_comp <- calculate_components(feb_data)
mar_comp <- calculate_components(mar_data)

# Consolidate components from each month
#  Problem binding yearqtr columns together.  Information is lost
#  Related to: https://github.com/tidyverse/dplyr/issues/2457
comp_data <- bind_rows(jan_comp, feb_comp, mar_comp)

# calculate measures from components
measure_data <- calculate_measures(comp_data)

# make into maptg format
maptg <- make_maptg(measure_data, "Hurley")

# Emit cleaned data to file
write_csv(maptg, "maptg_hurley_2020_Q1.csv")

