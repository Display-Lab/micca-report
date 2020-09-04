#' Create report content for single site.
#' @param data data frame of maptg data.
#' @param recip string recipient of report as it appears in the data as ascribee.
#' @param include_cid  boolean flag to show(TRUE) or hide(FALSE) the content identifiers
create_report <- function(data, recip, output_path='report.pdf', include_cid=F){

  # Create report environment
  report_env <- create_report_env(data, recip)

  # Use report environment to create report
  template_path <- system.file("templates","report_v2.Rnw", package="miccareport")
  knit_report(template_path, report_env, output_path)

  return(invisible(NULL))
}

#` Create report environment
create_report_env <- function(maptg_data, recip){
  # Top level report env setup
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

  # Sums for reports
  top_env$m14_sum <- numerator_sum(maptg_data, "M14", recip)
  top_env$m5_sum <- numerator_sum(maptg_data, "M5", recip)
  top_env$m1_sum <- numerator_sum(maptg_data, "M1", recip)

  # Create figures
  # make this the env handed to report
  report_env <- build_figures(maptg_data, recip)
  # Add top environment as parent of figures to gain access libraries and variables
  parent.env(report_env) <- top_env

  return(report_env)
}

#' knit report and emit pdf as a side effect.
#' Return invisible ouput path
#' @importFrom knitr knit2pdf
knit_report <- function(template_path, report_env, output_path){
  options(tinytex.engine="pdflatex")
  suppressMessages(
    utils::capture.output(
      knitr::knit2pdf(input = template_path,
                      envir=report_env,
                      pdf_file= output_path)
   ))
  return(invisible(output_path))
}
