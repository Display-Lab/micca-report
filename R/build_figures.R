#' Generate all figures for report
#'   Add each figure into an environment for ease of management
#' @return environment containing figures
#' @importFrom rlang new_environment base_env
build_figures <- function(maptg_data, recip){
  figs_env <- new_environment(parent=base_env())

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

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M5")
  figs_env$fig540727 <- circle_plot(maptg_data, recip, "M5", plot_mean)

  content_id <- deparse(quote(fig540727))
  figs_env$info540727 <- paste(content_id, "m5")

  #### FIGURE
  figs_env$fig5BF5D0 <- line_plot(maptg_data, recip, "M5")

  content_id <- deparse(quote(fig5BF5D0))
  figs_env$info5BF5D0 <- paste(content_id, "m5")

  #### TABLE DATA
  figs_env$tbl82C4A3 <- make_table_data_tbl82C4A3(maptg_data, recip)

  content_id <- deparse(quote(tbl82C4A3))
  figs_env$info82C4A3 <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M8")
  figs_env$figBDBC81 <- circle_plot(maptg_data, recip, "M8", plot_mean)

  content_id <- deparse(quote(figBDBC81))
  figs_env$infoBDBC81 <- paste(content_id, "m8")

  #### FIGURE
  measures <- c("M10","M11","M12","M13")
  #post generation remove x-axis labels
  figs_env$figFEA046 <- horizontal_bar_plot(maptg_data, recip, measures) +
    theme(axis.text.x=element_blank(), axis.ticks=element_blank())

  content_id <- deparse(quote(figFEA046))
  figs_env$infoFEA046 <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  measures <- c("M16","M17","M18","M19")
  figs_env$fig96B019 <- horizontal_bar_plot(maptg_data, recip, measures)

  content_id <- "fig96B019"
  figs_env$info96B019 <- paste(content_id, "m16,17,18,19")

  #### Figure
  measures <- c("M20","M21")
  figs_env$fig779D32 <- horizontal_bar_plot(maptg_data, recip, measures)

  content_id <- "fig779D32"
  figs_env$info779D32 <- paste(c(content_id, measures))

  #### FIGURE
  measures <- "M2"
  plot_mean <- micca_mean(maptg_data, measures)
  figs_env$fig0DB9AF <- circle_plot(maptg_data, recip, measures, plot_mean)

  content_id <- "fig0DB9AF"
  figs_env$info0DB9AF <- paste(content_id, measures)

  #### FIGURE
  measures <- "M2"
  figs_env$fig022768 <- line_plot(maptg_data, recip, measures)

  content_id <- "fig022768"
  figs_env$info022768 <- paste(content_id, measures)

  #### FIGURE
  measures <- "M4"
  plot_mean <- micca_mean(maptg_data, measures)
  figs_env$figA28BC2 <- circle_plot(maptg_data, recip, measures, plot_mean)

  content_id <- "figA28BC2"
  figs_env$infoA28BC2 <- paste(content_id, measures)

  #### FIGURE
  measures <- "M4"
  figs_env$figC6EAF6 <- line_plot(maptg_data, recip, measures)

  content_id <- "figC6EAF6"
  figs_env$infoC6EAF6 <- paste(content_id, measures)

  return(figs_env)
}
