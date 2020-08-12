#' @param date POSXIct or Date in the quarter.
#' @importFrom lubridate now year
#' @return named list with begin and end dates of quarter
quarter_range <- function( date=NA ){
  if(is.na(date)){ date <- lubridate::now("UTC") }

  begin <- lubridate::floor_date(date, unit="quarter")
  end <- lubridate::ceiling_date(date, unit="quarter")
  return(list(begin=begin, end=end))
}

#' Get the nearest previous quarter.
#' @param date POSIXct or Date that previous quarter will be relative to.
#' @importFrom lubridate now quarter
#' @return named list with begin and end dates of previous quarter
previous_quarter_range <- function(date=NA){
  if(is.na(date)){ date <- lubridate::now("UTC") }

  curr_q_floor <- lubridate::floor_date(date, unit="quarter")
  prev_q_date <- curr_q_floor %m-% months(1)

  quarter_range(prev_q_date)
}