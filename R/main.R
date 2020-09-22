#' Main entry function generates reports of all ascribees with data in data_dir
#' @param begin yyyy-MM-dd format date of begin date (inclusive)
#' @param end yyyy-MM-dd format date of end date (excluded)
#' @param data_dir path to backend data directory.  Expects ascribde/timestamp_maptg.csv
#' @note Given no begin or end, will assume previous quarter.
#' @importFrom lubridate ymd
#' @export
main <- function(data_dir='site_data', begin=NA, end=NA){
  if(is.na(begin) || is.na(end)){
    quarter_lims <- previous_quarter_range()
    begin <- as.Date(quarter_lims$begin)
    end <- as.Date(quarter_lims$end)
  } else {
    begin <- ymd(begin)
    end <- ymd(end)
  }

  full_data <- ingest_aws_dir(data_dir)
  trimmed_data <- trim_data(full_data, begin, end)

  recipients <- trimmed_data %>% pull(ascribee) %>% unique()
  output_paths <- sapply(recipients, report_path)

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
report_path <- function(recip){
  recip_dashed <- gsub(' ', '-', recip)
  base <- paste(strftime(now(), format="%Y-%m-%d"), recip_dashed, sep="_")
  output_path <- paste(base, ".pdf", sep="")
}

#' Convenience function reordering the params of create_report for use with mapply
create_wrapper <- function(recipient, outpath, data){
  create_report(data, recipient, outpath)
}

