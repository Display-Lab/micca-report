library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(stringr)
library(zoo)

read_hurley_data <- function(filename){
  # Ingest hurley data
  col_classes <- cols_only(col_character(), col_date(), col_character(),
                           col_character(),col_character(),col_character(),
                           col_character(),col_character(),col_character())
  
  # Force column names and hope order doesn't change.
  col_names <- c("mrn","delivery_date","counseling","contra_choice","imm_method",
                 "contra_prov","larc_prov","payer","choice_date")
  
  raw_data <- readr::read_csv(filename, col_types = col_classes, na=c(""))
  colnames(raw_data) <- col_names
  return(raw_data)
}

normalize_hurley_data <- function(raw_data){
  # ASSUME: D7 larc_prov is "4-60 at pp visit" for "4-60 days"
  # Enforce choice: immediate PP IUD, immediate PP Nexplanon, PPTL, other, none, unknown
  # Correct "Unkown" to "unknown"
  # Recode choice to unknown for counseling == FALSE
  #  Currently unused: choice_date = ymd(base::strptime(choice_date, format="%Y-%m-%d")),
  raw_data %>% 
    filter(!is.na(mrn))  %>%
    mutate(
      counseling      = ifelse(counseling == 'Yes', TRUE, FALSE),
      contra_choice   = ifelse(contra_choice == "Unkown", "unknown", contra_choice),
      contra_choice   = ifelse(counseling, contra_choice, "unknown"),
      imm_method      = recode(.x=imm_method, "none during delivery admission"="none"),
      contra_prov     = sub("(\\d)days","\\1 days",contra_prov),
      larc_prov       = sub("(\\d)days","\\1 days",larc_prov),
      larc_prov       = ifelse(larc_prov == "4-60 days", "4-60 days pp visit", larc_prov),
      payer           = sub(pattern = "-.*$", replacement = "", payer),
      institution = "Hurley" ) %>%
    mutate_if(.predicate=is.character, tolower) %>%
    filter(payer %in% c("medicaid", "molina", "private", "other", "unknown"),
           contra_choice %in% c("immediate pp iud", "immediate pp nexplanon", "pptl", "other", "none", "unknown"))
}

calculate_hurley_components <- function(proc_data){
  proc_data %>% 
    mutate(report_month = floor_date(delivery_date, unit="month"),
           quarter = as.yearqtr(delivery_date)) %>%
    group_by(report_month, payer) %>%
    summarize(
      C1 = n(),
      C2 = sum(contra_prov == "0-3 days"),
      C3 = sum(contra_prov == "4-60 days"),
      C4 = sum(larc_prov == "0-3 days"),
      C5 = sum(larc_prov == "4-60 days pp visit" | larc_prov == "4-60 days not pp visit" ),
      #C6 = NA, # deprecated
      C7 = sum(counseling),
      C8 = sum(contra_choice != "unknown"),
      C9 = sum(contra_choice == "immediate pp iud" | contra_choice == "immediate pp nexplanon"),
      #C10 = sum(contra_choice != "none"), # deprecated
      C11 = sum(counseling & contra_choice == imm_method),
      C12 = sum(larc_prov == "4-60 days pp visit" | larc_prov == "4-60 days not pp visit"),
      C13 = sum(contra_choice == "immediate pp iud"),
      C14 = sum(contra_choice == "immediate pp nexplanon"),
      C15 = sum(contra_choice == "pptl"),
      C16 = sum(contra_choice == "other")
    )
}

# numerators and denominators by month and payer
calculate_measures <- function(component_data){
  # this currently drops quarter and choice month
  component_data %>%
    group_by(report_month, payer) %>%
    summarise_all(.funs=sum) %>%
    transmute(
      group = payer,
      n_M1  = C2,  d_M1  = C1,
      n_M2  = C3,  d_M2  = C1,
      n_M3  = C4,  d_M3  = C1,
      n_M4  = C5,  d_M4  = C1,
      n_M5  = C7,  d_M5  = C1,
      n_M6  = C8,  d_M6  = C1,
      n_M7  = C9,  d_M7  = C8,
      n_M8  = C11, d_M8  = C8,
      n_M9  = C12, d_M9  = C1,
      n_M10 = C13, d_M10 = C8,
      n_M11 = C14, d_M11 = C8,
      n_M12 = C15, d_M12 = C8,
      n_M13 = C16, d_M13 = C8,
      n_M14 = C1,  d_M14 = NA) %>%
      pivot_longer(cols = starts_with("n_"), names_to = "measure_n", 
                   names_prefix = "n_", values_to = "numerator") %>% 
      pivot_longer(cols = starts_with("d_"), names_to = "measure_d", 
                   names_prefix = "d_", values_to = "denominator") %>%
      filter(measure_n == measure_d) %>%
      rename( measure = measure_n ) %>%
      select(-measure_d)
}

# Measure Numerators in terms of component

# Read raw data
jan_raw <- read_hurley_data('data/hurley_2020-01.csv')
feb_raw <- read_hurley_data('data/hurley_2020-02.csv')
mar_raw <- read_hurley_data('data/hurley_2020-03.csv')


# Normalize data
jan_data <- normalize_hurley_data(jan_raw)
feb_data <- normalize_hurley_data(feb_raw)
mar_data <- normalize_hurley_data(mar_raw)

# combine and emit normalized data for future reference by hurley
norm_data <-  bind_rows(jan_data, feb_data, mar_data)
write_csv(norm_data, "hurley_2020_Q1.csv")

# Calc measures components by delivery month and payer
jan_comp <- calculate_hurley_components(jan_data)
feb_comp <- calculate_hurley_components(feb_data)
mar_comp <- calculate_hurley_components(mar_data)

# Consolidate components from each month
#  Problem binding yearqtr columns together.  Information is lost
#  Related to: https://github.com/tidyverse/dplyr/issues/2457
comp_data <- bind_rows(jan_comp, feb_comp, mar_comp)

# calculate measures from components
measure_data <- calculate_measures(comp_data)

# make into maptg format
maptg <- measure_data %>% 
  rename(time=report_month) %>%
  add_column(ascribee="Hurley", .before=1)

# Emit cleaned data to file
write_csv(maptg, "maptg_hurley_2020_Q1.csv")

