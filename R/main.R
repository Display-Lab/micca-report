#' Generate report content for single site.
#' @param recip string recipient of report: UMich, Hurley, Munson, WMH
#' @param include_cid  boolean flag to show(TRUE) or hide(FALSE) the content identifiers
#' @import knitr tikzDevice readr ggplot2 dplyr stringr tidyr kableExtra
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate group_by
main <- function(maptg_data, recip, output_path='report.pdf', include_cid=F){

  ##### REPORT ENV SETUP
  obs_end_date <- maptg_data$time %>% max
  obs_start_date <- maptg_data$time %>% min

  start_month <- format(obs_start_date, "%b")
  end_month <- format(obs_end_date, "%b")
  end_year <- format(obs_end_date, "%Y")
  INCLUDE_CID=include_cid
  ascribee_title <- ifelse(is.na(MR$ASCRIBEE_TITLES[recip]), recip, MR$ASCRIBEE_TITLES[recip])

  #### SUMS: OVERVIEW
  m14_sum <- numerator_sum(maptg_data, "M14", recip)
  m5_sum <- numerator_sum(maptg_data, "M5", recip)
  m1_sum <- numerator_sum(maptg_data, "M1", recip)

  #### FIGURE
  plot_data <- make_fig7F5D31_data(maptg_data, recip)
  fig7F5D31 <- make_fig7F5D31(plot_data)
  content_id <- deparse(substitute(fig7F5D31))
  info7F5D31 <- paste(content_id, "m14")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M1")
  figADA835A <- circle_plot(maptg_data, recip, "M1", plot_mean)
  content_id <- deparse(substitute(fig7F5D31))
  infoADA835A <- paste(content_id, "m1")

  #### FIGURE
  fig707A6E <- line_plot(maptg_data, recip, "M1")
  content_id <- deparse(substitute(fig707A6E))
  info707A6E <- paste(content_id, "m1")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M3")
  fig9C0A4F <- circle_plot(maptg_data, recip, "M3", plot_mean)
  content_id <- deparse(substitute(fig9C0A4F))
  info9C0A4F <- paste(content_id, "m3")

  ### FIGURE
  plot_data <- make_figBE214E_data(maptg_data, recip)
  figBE214E <- make_figBE214E(plot_data)
  content_id <- deparse(substitute(figBE214E))
  infoBE214E <- paste(content_id, "m20,21")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M5")
  fig540727 <- circle_plot(maptg_data, recip, "M5", plot_mean)
  content_id <- deparse(substitute(fig540727))
  info540727 <- paste(content_id, "m5")

  #### FIGURE
  fig5BF5D0 <- line_plot(maptg_data, recip, "M5")
  content_id <- deparse(substitute(fig5BF5D0))
  info5BF5D0 <- paste(content_id, "m5")

  #### TABLE DATA
  tbl82C4A3 <- make_table_data_tbl82C4A3(maptg_data, recip)
  content_id <- deparse(substitute(tbl82C4A3))
  info82C4A3 <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  plot_data <- make_fig1903AB_data(maptg_data, recip)
  fig1903AB <- make_fig1903AB(plot_data)
  content_id <- deparse(substitute(fig1903AB))
  info1903AB <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M8")
  figBDBC81 <- circle_plot(maptg_data, recip, "M8", plot_mean)
  content_id <- deparse(substitute(figBDBC81))
  infoBDBC81 <- paste(content_id, "m8")

  #### FIGURE
  plot_data <- make_figE8F578_data(maptg_data, recip)
  figE8F578 <- make_figE8F578(plot_data)
  content_id <- deparse(substitute(figE8F578))
  infoE8F578 <- paste(content_id, "m16,17,18,19")

  # Generate Report
  template_path <- system.file("templates","report.Rnw", package="miccareport")

  build_dir <- tempdir()

  options(tinytex.engine="pdflatex")
  knitr::knit2pdf(input = template_path, pdf_file=output_path, clean=TRUE)

  # TODO: Build report_env to directly control what variables are accessible in the report.
  #knitr::knit2pdf(input = template_path, envir=report_env, pdf_file= output_path)

  return(invisible(NULL))
}

######################
# Process Input Data #
######################
#' @note DEPRECATED
trim_input <- function(){
  full_maptg_data <- readr::read_csv('data/maptg.csv', trim_ws=T)

  # Trim to date range
  START_DATE <- lubridate::ymd('2020-01-01')
  END_DATE <- lubridate::ymd('2020-04-01')
  trimmed_data <- full_maptg_data %>% filter( time >= START_DATE, time < END_DATE)
  return(trimmed_data)
}

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
