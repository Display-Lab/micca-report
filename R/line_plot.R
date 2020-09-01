# Common Line Plot
#' @importFrom dplyr %>% filter group_by summarize ungroup filter bind_rows mutate
#' @import ggplot2
line_plot <- function(maptg_data, recip, measure_id){
  recipient_title <- MR$ASCRIBEE_TITLES[recip]

  micca_data <- maptg_data %>%
    filter(measure == measure_id) %>%
    group_by(time, ascribee) %>%
    summarize(
      numerator = sum(numerator, na.rm=TRUE),
      denominator = sum(denominator, na.rm=TRUE),
      rate = numerator/denominator) %>%
    ungroup() %>%
    group_by(time) %>%
    summarize(
      numerator = floor(mean(numerator)),
      denominator = floor(mean(denominator)),
      rate = mean(rate),
      ascribee = "MICCA Average")

  plot_data <- maptg_data %>%
    filter(measure == measure_id, ascribee == recip) %>%
    bind_rows(micca_data) %>%
    group_by(ascribee, time) %>%
    summarize(
      numerator = sum(numerator, na.rm=TRUE),
      denominator = sum(denominator, na.rm=TRUE),
      rate = numerator/denominator) %>%
    mutate(
      recipient = ifelse(ascribee == recip, T, F),
      perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
      arrow = ifelse(recipient, "show", "noshow"),
      pcolor = ifelse(recipient, MR$DL_DARK_BLUE, MR$DL_GRAY))

  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")


  ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
    geom_line(size=1, lineend="round") +
    geom_point(size=2, fill=MR$DL_FILL, shape=21, stroke=1.2) +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
    scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
    scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
    geom_point(mapping = aes(y = rate + 0.07, shape=arrow), size=4, color=MR$DL_DARK_BLUE) +
    geom_label(mapping = aes(label=perf_label), nudge_y = 0.13, fill=MR$DL_DARK_BLUE,
               color=MR$DL_FILL, label.r = unit(0, "lines"), label.size=0) +
    single_line_theme() +
    scale_color_manual(labels = unique(plot_data$ascribee),
                       values = unique(plot_data$pcolor), guide = guide_legend(title=NULL)) +
    theme(legend.position="bottom", legend.box.spacing = unit(0,"mm"))
}
