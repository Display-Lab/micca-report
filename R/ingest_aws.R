#' Point at the site_data directory sync'd from AWS.
#' @return dataframe of deduplicated MAPTG data for all sites.
ingest_aws_dir <- function(data_dir='site_data'){
  paths <- enumerate_aws_dir(data_dir)
  raw_maptg <- aggregate_files(paths)
  dedupe_maptg(raw_maptg)
}

#' Find and enumerate all csv file in the data directory.
#' Return in revese alphabetical order as date is expected to be in filename.
#'   Alternative is to include date-uploaded metadata in data set directly
#' @return vector of file paths reverse alphabetically sorted.
enumerate_aws_dir <- function(data_dir){
  f_list <- list.files(path=data_dir, pattern=".csv$", full.names=TRUE,
                       recursive=TRUE, ignore.case=TRUE)
  sort(f_list, decreasing = TRUE)
}

#' @import readr
#' @importFrom dplyr distinct
#' @importFrom readr cols col_character col_date col_factor col_integer
aggregate_files <- function(paths){
  maptg_colspec <- readr::cols(ascribee = col_character(),
                               time = col_date(format="%Y-%m-%d"),
                               group = col_factor(),
                               numerator = col_integer(),
                               denominator = col_integer())

  df_list <- lapply(paths, read_csv, col_types=maptg_colspec)

  Reduce(bind_rows, df_list)
}

#' Deduplicate data assumes newest version of data occurs first in dataset
#' @importFrom dplyr distinct
dedupe_maptg <- function(raw_maptg){
  dplyr::distinct(.data=raw_maptg, time, group, measure, ascribee, .keep_all=T)
}
