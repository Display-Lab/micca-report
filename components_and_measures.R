library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(stringr)
library(zoo)

# Calculate components and measures common functions
calculate_components <- function(proc_data){
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
      C11 = sum(contra_choice != "unknown" & contra_choice == imm_method & contra_prov == "0-3 days"),
      C12 = sum(larc_prov == "4-60 days pp visit" | larc_prov == "4-60 days not pp visit"),
      C13 = sum(contra_choice == "immediate pp iud"),
      C14 = sum(contra_choice == "immediate pp nexplanon"),
      C15 = sum(contra_choice == "pptl"),
      C16 = sum(contra_choice == "other"),
      C17 = sum(contra_choice == "immediate pp iud" & imm_method == "immediate pp iud"),
      C18 = sum(contra_choice == "immediate pp nexplanon" & imm_method == "immediate pp nexplanon"),
      C19 = sum(contra_choice == "pptl" & imm_method == "pptl"),
      C20 = sum(contra_choice == "other" & imm_method == "other"),
      C21 = sum(imm_method == "immediate pp iud"),
      C22 = sum(imm_method == "immediate pp nexplanon")
    )
}

calculate_measures <- function(component_data){
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
      n_M14 = C1,  d_M14 = NA,
      n_M16 = C17, d_M16 = C13,
      n_M17 = C18, d_M17 = C14,
      n_M18 = C19, d_M18 = C15,
      n_M19 = C20, d_M19 = C16,
      n_M20 = C21, d_M20 = C3,
      n_M21 = C22, d_M21 = C3 ) %>%
      pivot_longer(cols = starts_with("n_"), names_to = "measure_n", 
                   names_prefix = "n_", values_to = "numerator") %>% 
      pivot_longer(cols = starts_with("d_"), names_to = "measure_d", 
                   names_prefix = "d_", values_to = "denominator") %>%
      filter(measure_n == measure_d) %>%
      rename( measure = measure_n ) %>%
      select(-measure_d)
}

make_maptg <- function(measure_data, institution){
  measure_data %>% 
    rename(time=report_month) %>%
    mutate(group = ifelse(group !="medicaid", "non_medicaid", group)) %>%
    add_column(ascribee=institution, .before=1)
}