#' Generate report content for single site.
#' @param recip string recipient of report: UMich, Hurley, Munson, WMH
#' @param include_cid  boolean flag to show(TRUE) or hide(FALSE) the content identifiers
#' @import knitr tikzDevice readr ggplot2 dplyr stringr tidyr kableExtra
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate group_by
#' @importFrom rlang new_environment base_env
main <- function(maptg_data, recip, output_path='report.pdf', include_cid=F){

  #### TOP LEVEL REPORT ENV SETUP
  # Include packages required for running report.
  top_env <- report_environment()

  obs_end_date   <- maptg_data$time %>% max
  obs_start_date <- maptg_data$time %>% min

  top_env$start_month <- format(obs_start_date, "%b")
  top_env$end_month   <- format(obs_end_date, "%b")
  top_env$end_year    <- format(obs_end_date, "%Y")

  if( is.na(MR$ASCRIBEE_TITLES[recip]) ){
    top_env$ascribee_title <- recip
  } else {
    top_env$ascribee_title <- MR$ASCRIBEE_TITLES[recip]
  }

  #### SUMS: OVERVIEW
  top_env$m14_sum <- numerator_sum(maptg_data, "M14", recip)
  top_env$m5_sum <- numerator_sum(maptg_data, "M5", recip)
  top_env$m1_sum <- numerator_sum(maptg_data, "M1", recip)

  #### GENERATE FIGURES
  # make this the env handed to report
  report_env <- build_figures(maptg_data, recip)
  # Add top environment as parent of figures to gain access libraries and variables
  parent.env(report_env) <- top_env

  #### Generate Report
  template_path <- system.file("templates","report_v2.Rnw", package="miccareport")
  build_dir <- tempdir()

  options(tinytex.engine="pdflatex")
  suppressMessages(
    utils::capture.output(
      knitr::knit2pdf(input = template_path,
                      envir=report_env,
                      pdf_file= output_path)
   ))

  return(invisible(NULL))
}

######################
# Process Input Data #
######################
#' @param data maptg dataframe
#' @param begin yyyy-MM-dd format date of begin date inclusive
#' @param end yyyy-MM-dd format date of end date excluded
#' @return maptg dataframe trimmed to time interval
trim_data <- function(data, begin, end){
  begin_date <- lubridate::ymd(begin)
  end_date <- lubridate::ymd(end)
  data %>% filter( time >= begin_date, time < end_date)
}

########################
# Generate ALL Reports #
########################

#' @return Filename string for a report based on generation time and recipient.
#' @param recip String name of ascribee that will recieve report.
report_path <- function(recip){
  base <- paste("report", strftime(now(), format="%Y-%m-%d_%H%M"), recip, sep="_")
  output_path <- paste(base, ".pdf", sep="")
}

main_mwrap <- function(recipient, outpath, data){
  main(data, recipient, outpath)
}

#' @param begin yyyy-MM-dd format date of begin date inclusive
#' @param end yyyy-MM-dd format date of end date excluded
#' @importFrom lubridate ymd
#' @export
generate_all <- function(data_dir='site_data', begin=NA, end=NA){
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

  mapply(main_mwrap, recipients, output_paths, MoreArgs=list(data=trimmed_data) )
}
