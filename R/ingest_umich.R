#' @import readr
read_umich_data <- function(filename){
  col_classes <- cols_only(col_character(), col_date(), col_character(),
                           col_character(),col_character(),col_character(),
                           col_character(),col_character(),col_character(),
                           col_character())

  # This column order is different than the specification
  col_names <- c("mrn","delivery_date","counseling","contra_choice","imm_method",
                 "contra_prov","larc_prov","choice_date","payer", "peripartum_care")

  raw_data <- readr::read_csv(filename, col_types = col_classes, na=c(""))
  colnames(raw_data) <- col_names
  return(raw_data)
}

#' @import dplyr tidyr stringr
standardize_umich_data <- function(raw_data){
  raw_data %>%
    filter(!is.na(mrn))  %>%
    mutate_if(.predicate=is.character, tolower) %>%
    mutate(
      counseling      = ifelse(counseling == 'yes', TRUE, FALSE),
      payer           = sub(pattern = "molina", replacement = "medicaid", payer),
      contra_choice   = ifelse(choice_date == "unknown", "unknown", contra_choice),
      institution = "UMich" ) %>%
    filter(payer %in% c("medicaid", "molina", "private", "other", "unknown"),
           contra_choice %in% c("immediate pp iud", "immediate pp nexplanon", "pptl", "other", "none", "unknown")) %>%
    select(mrn,delivery_date,counseling, contra_choice, imm_method, contra_prov,
           larc_prov, payer, choice_date, peripartum_care)
}

#' @import readr dplyr tidyr tibble lubridate stringr zoo
ingest_umich <- function(){
  # Read raw data
  df_raw <- read_umich_data("data/umich_2020_Q1.csv")
  # Standardize
  df_std <- standardize_umich_data(df_raw)
  # Emit standardized data
  write_csv(df_std, "umich_std_2015-07-01_2020-04-06.csv")

  # Calculate components and measures
  df_comp <- calculate_components(df_std)
  df_measure <- calculate_measures(df_comp)

  # Create MAPTG data
  maptg <- make_maptg(df_measure, "UMich")

  # Emit cleaned data to file
  write_csv(maptg, "maptg_umich.csv")
}
