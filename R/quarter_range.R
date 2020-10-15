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
#' @importFrom lubridate now quarter %m-% floor_date
#' @return named list with begin and end dates of previous quarter
previous_quarter_range <- function(date=NA){
  if(is.na(date)){ date <- lubridate::now("UTC") }

  curr_q_floor <- lubridate::floor_date(date, unit="quarter")
  prev_q_date <- curr_q_floor %m-% months(1)

  quarter_range(prev_q_date)
}

#' Quarter Begin
#' @param year numeric year
#' @param quarter numeric quarter
#' @return Date for beginning of quarter (inclusive)
quarter_begin <- function(year, quarter){
  month <- 1 + 3*(quarter-1)
  make_date(year, month, 01)
}

#' Quarter End
#' @param year numeric year
#' @param quarter numeric quarter
#' @return Date for end of quarter (excluded)
#' @importFrom lubridate make_date
quarter_end <- function(year, quarter){
  month <- (4 + 3*(quarter-1)) %% 12
  make_date(year, month, 01)
}

#' Previous Quarter
#' @param numeric quarter
#' @return numeric previous quarter
prev_quarter <- function(quarter){ ifelse(quarter ==1, 4, quarter-1) }
