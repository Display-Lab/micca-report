#' Combine maptg data sets from multiple sites
#' @note DEPRECATED
#' @import readr dplyr
merge_maptg_files <- function(){
  maptg_colspec <- cols(ascribee = col_character(),
                        time = col_date(format="%Y-%m-%d"),
                        group = col_factor(),
                        numerator = col_integer(),
                        denominator = col_integer())

  maptg_files <- c('proc-data/maptg_hurley_2020_Q1.csv',
                   'proc-data/maptg_munson_2020_Q1.csv',
                   'proc-data/maptg_wmh_2020_Q1.csv',
                   'proc-data/maptg_umich.csv')

  df_list <- lapply(maptg_files, read_csv, col_types=maptg_colspec)

  full_maptg_data <- Reduce(bind_rows, df_list)

  write_csv(full_maptg_data, "data/maptg_consolidated.csv")
}
