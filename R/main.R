#' Generate report content for single site.
#' @param recip string recipient of report: UMich, Hurley, Munson, WMH
#' @param include_cid  boolean flag to show(TRUE) or hide(FALSE) the content identifiers
#' @import knitr tikzDevice readr ggplot2 dplyr stringr tidyr kableExtra
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
  plot_data <- maptg_data %>%
    filter(measure =="M14", ascribee == recip) %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(time) %>%
    mutate(denominator = sum(numerator),
           payer_rate = numerator/denominator)

  fig7F5D31 <- ggplot(plot_data, aes(x=time, y=payer_rate)) +
    geom_bar(aes(fill=group), stat='identity', color=MR$DL_DARK_BLUE) +
    scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
    ylab("% Women Delivered") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    theme_minimal() +
    theme(legend.position="bottom",  axis.title.x = element_blank(),
          legend.title = element_blank(), legend.box.spacing = unit(0,"mm"))

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

  ## TODO side by side figures facetted together or cowplotted
  plot_data <- maptg_data %>%
    filter(measure %in% c("M20", "M21"), ascribee == recip) %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(group, measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(pull(., numerator))) %>%
    mutate(ratio = numerator/denominator) %>%
    left_join(MR$MEASURE_NAMES, by="measure")

  figBE214E <- ggplot(plot_data, aes(y=ratio)) +
    geom_bar(aes(fill=short_name,x="Device"), stat='identity') +
    geom_bar(aes(fill=group,x="Payer"), stat='identity') +
    ylab("% of Immediate LARC Provided") +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    theme_minimal() +
    theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank(),
          legend.text = element_text(size=8), legend.key.size = unit(4, "mm"))  +
    guides(fill=guide_legend(nrow=2))

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
  tbl82C4A3 <- maptg_data %>%
    filter(ascribee == recip,
           measure %in% c("M10", "M11","M12","M13")) %>%
    group_by(measure) %>%
    summarize(
      numerator = sum(numerator, na.rm=TRUE),
      denominator = sum(denominator, na.rm=TRUE),
      percent = sprintf('%.0f%%', 100*(numerator/denominator))) %>%
    left_join(MR$MEASURE_NAMES, by="measure") %>%
    select(short_name, numerator, percent) %>%
    rename(Choice=short_name, Count=numerator, Percentage=percent )

  content_id <- deparse(substitute(tbl82C4A3))
  info82C4A3 <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  # Preference of LARC by payer
  plot_data <- maptg_data %>%
    filter(ascribee == recip,
           measure %in% c("M10", "M11","M12","M13"))  %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(group, measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(denominator)) %>%
    mutate(rate = numerator/denominator) %>%
    left_join(MR$MEASURE_NAMES, by="measure")

  fig1903AB <- ggplot(plot_data, aes(x=group, y=rate)) +
    geom_bar(aes(fill=short_name), stat='identity', color=MR$DL_DARK_BLUE) +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    theme_minimal() +
    theme(legend.position="bottom", axis.title = element_blank(),
          title = element_text(size=10), legend.title = element_blank(),
          legend.text = element_text(size=8), legend.key.size = unit(4, "mm"),
          legend.box.spacing = unit(0,"mm")) +
    guides(fill=guide_legend(nrow=2))

  content_id <- deparse(substitute(fig1903AB))
  info1903AB <- paste(content_id, "m10,11,12,13")

  #### FIGURE
  plot_mean <- micca_mean(maptg_data, "M8")
  figBDBC81 <- circle_plot(maptg_data, recip, "M8", plot_mean)

  content_id <- deparse(substitute(figBDBC81))
  infoBDBC81 <- paste(content_id, "m8")

  #### FIGURE
  pref_pal <- c("Provided"=MR$DL_BLUE, "Preferred"=MR$DL_MAUVE)

  plot_data <- maptg_data %>%
    filter(ascribee == recip,
           measure %in% c("M16","M17","M18", "M19"))  %>%
    group_by(measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(denominator)) %>%
    left_join(MR$MEASURE_NAMES, by="measure") %>%
    mutate(rate = numerator/denominator,
           short_name = as.factor(short_name),
           mpos = as.numeric(short_name))

  nudge_factor <- (plot_data %>% pull(denominator) %>% max) /20

  figE8F578 <- ggplot(plot_data, aes(x=short_name)) +
    geom_bar(aes(y=numerator, color="Provided", fill="Provided"), stat='identity') +
    geom_linerange(aes(y=denominator, xmin=mpos-0.4, xmax=mpos+0.4, color="Preferred", fill="Preferred"),
                   size=1, linetype=3) +
    geom_text(aes(label=denominator,y=denominator), nudge_y = nudge_factor, size=3 ) +
    scale_color_manual(name="prov",values=pref_pal) +
    scale_fill_manual(name="prov",values=pref_pal) +
    theme_minimal() +
    theme(legend.position="right", axis.title.x = element_blank(),
          axis.text.x = element_text(angle=45, hjust=1, face="bold"),
          legend.title = element_blank(), legend.text = element_text(size=8),
          legend.key.size = unit(4, "mm") ) +
    #guides(color=guide_legend(override.aes=list(fill="Provided"))) +
    ylab("Number of Women")

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
