#' Generate all figures for report
#'   Add each figure into an environment for ease of management
#' @return environment containing figures
#' @importFrom rlang new_environment base_env
build_figures <- function(maptg_data, recip){
  figs_env <- new_environment(parent=base_env())

  # V2 One off.  Split data to Q1 and Q2.
  q1_times <- c(ymd('2020-01-01'), ymd('2020-02-01'), ymd('2020-03-01'))
  maptg_q1 <- dplyr::filter(maptg_data, time %in% q1_times)

  q2_times <- c(ymd('2020-04-01'), ymd('2020-05-01'), ymd('2020-06-01'))
  maptg_q2 <- dplyr::filter(maptg_data, time %in% q2_times)

  #### FIGURE
  plot_data <- make_fig7F5D31_data(maptg_data, recip)
  figs_env$fig7F5D31 <- make_fig7F5D31(plot_data)

  content_id <- deparse(quote(fig7F5D31))
  figs_env$info7F5D31 <- paste(content_id, "m14")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M1")
  figs_env$figADA835A <- circle_plot(maptg_data, recip, "M1", plot_mean)

  content_id <- deparse(quote(figADA835A))
  figs_env$infoADA835A <- paste(content_id, "m1")

  #### FIGURE
  figs_env$fig707A6E <- line_plot(maptg_data, recip, "M1")

  content_id <- deparse(quote(fig707A6E))
  figs_env$info707A6E <- paste(content_id, "m1")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M3")
  figs_env$fig9C0A4F <- circle_plot(maptg_data, recip, "M3", plot_mean)

  content_id <- deparse(quote(fig9C0A4F))
  figs_env$info9C0A4F <- paste(content_id, "m3")

  ### FIGURE
  plot_data <- make_figBE214E_data(maptg_data, recip)
  figs_env$figBE214E <- make_figBE214E(plot_data)

  content_id <- deparse(quote(figBE214E))
  figs_env$infoBE214E <- paste(content_id, "m20,21")

  #### FIGURE
  plot_mean <- micca_mean(maptg_q2, "M5")
  figs_env$fig540727 <- circle_plot(maptg_q2, recip, "M5", plot_mean)

  content_id <- deparse(quote(fig540727))
  figs_env$info540727 <- paste(content_id, "m5")

  #### FIGURE
  figs_env$fig5BF5D0 <- line_plot(maptg_q2, recip, "M5")

  content_id <- deparse(quote(fig5BF5D0))
  figs_env$info5BF5D0 <- paste(content_id, "m5")

  #### TABLE DATA
  figs_env$tbl82C4A3 <- make_table_data_tbl82C4A3(maptg_q2, recip)

  content_id <- deparse(quote(tbl82C4A3))
  figs_env$info82C4A3 <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  plot_data <- make_fig1903AB_data(maptg_data, recip)
  figs_env$fig1903AB <- make_fig1903AB(plot_data)

  content_id <- deparse(quote(fig1903AB))
  figs_env$info1903AB <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M8")
  figs_env$figBDBC81 <- circle_plot(maptg_data, recip, "M8", plot_mean)

  content_id <- deparse(quote(figBDBC81))
  figs_env$infoBDBC81 <- paste(content_id, "m8")

  #### FIGURE
  plot_data <- make_figE8F578_data(maptg_data, recip)
  figs_env$figE8F578 <- make_figE8F578(plot_data)

  content_id <- deparse(quote(figE8F578))
  figs_env$infoE8F578 <- paste(content_id, "m16,17,18,19")

  #### FIGURE
  plot_data <- make_figFEA046_data(maptg_q2, recip)
  figs_env$figFEA046 <- make_figFEA046(plot_data)

  content_id <- deparse(quote(figFEA046))
  figs_env$infoFEA046 <- paste(content_id, "m10,11,12,13")

  return(figs_env)
}
