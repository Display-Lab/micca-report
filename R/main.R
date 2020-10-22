#' Main
#' @param site_data string path to directory where site data is organized
#' @param year numeric year. Defaults to current year.
#' @param quarter numeric quarter. Defaults to previous quarter.
#' @importFrom lubridate year now
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
main <- function(data_dir='site_data', year=NA, quarter=NA){
  # Default to current year
  if(is.na(year)){
    year <- year(now())
  }

  # Default to previous quarter
  if(is.na(quarter)){
    quarter <- prev_quarter( quarter(now()) )
    year <- ifelse(quarter == 4, year-1, year)
  }

  begin <- quarter_begin(year, quarter)
  end <- quarter_end(year, quarter)

  full_data <- ingest_aws_dir(data_dir)
  trimmed_data <- trim_data(full_data, begin, end)

  recipients <- trimmed_data %>% pull(ascribee) %>% unique()
  report_name_prefix <- paste(year, "Q", quarter, sep='')
  output_paths <- sapply(recipients, report_path, prefix=report_name_prefix)

  output_dirs <- sapply(output_paths, dirname)
  sapply(output_dirs, dir.create, recursive=T, mode='0755')

  mapply(create_wrapper, recipients, output_paths, MoreArgs=list(data=trimmed_data) )
}

#' @param data maptg dataframe
#' @param begin yyyy-MM-dd format date of begin date (inclusive)
#' @param end yyyy-MM-dd format date of end date (excluded)
#' @return maptg dataframe trimmed to time interval
#' @importFrom lubridate ymd
#' @importFrom dplyr filter
trim_data <- function(data, begin, end){
  begin_date <- lubridate::ymd(begin)
  end_date <- lubridate::ymd(end)
  data %>% dplyr::filter( time >= begin_date, time < end_date)
}

#' @return Filename string for a report based on generation time and recipient.
#' @param recip String name of ascribee that will recieve report.
#' @param prefix String prefix for each report.
report_name <- function(recip, prefix){
  base <- paste(prefix, recip, sep="_")
  paste(base, ".pdf", sep="")
}

#' @return Path string for a report based on generation time and recipient.
#' @param recip String name of ascribee that will recieve report.
#' @param prefix String prefix for each report.  Defaults to date string.
report_path <- function(recip, prefix=NA){
  recip_dashed <- gsub(' ', '-', recip)
  if(is.na(prefix)){
    prefix <- strftime(now(), format="%Y-%m-%d")
  }
  filename <- report_name(recip_dashed, prefix)
  file.join('site_reports', recip_dashed, filename)
}


