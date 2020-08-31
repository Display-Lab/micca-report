#' Horizontal Bar Plot
#' @param maptg_data long form tabular data in maptg format
#' @param recip name of recipient as it appears in the data
#' @param measures vector of measure ids from which to create bars.
horizontal_bar_plot <- function(maptg_data, recip, measures){
  plot_data <- hbar_plot_data(maptg_data, recip, measures)
  hbar_plot_fig(plot_data)
}

#' @importFrom dplyr %>% filter mutate group_by summarize mutate case_when left_join
hbar_plot_data <- function(maptg_data, recip, measures){
  maptg_data %>%
    filter(ascribee == recip,
           measure %in% measures)  %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(group, measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(denominator),
              label = paste(numerator,denominator, sep="/")) %>%
    mutate(rate = numerator/denominator,
           x_cat = paste(measure, group, sep="-"),
           x_adjust = case_when(
             group == "Medicaid" ~ 0.2,
             group == "non-Medicaid" ~ -0.2,
             TRUE ~ 0 ),
           callout = ifelse(rate < 0.81, label, NA),
           bar_label = ifelse(is.na(callout), label, NA)) %>%
    left_join(MR$MEASURE_NAMES, by="measure")
}

#' @import ggplot2
hbar_plot_fig <- function(plot_data){
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")
  x_names <- plot_data$short_name %>% unique()

  ggplot(plot_data,
         aes(x=stage(short_name, after_stat = (x + x_adjust)),
             y=rate, label=label, fill=group, x_adjust=x_adjust)) +
    single_bar_theme() +
    geom_col(width = .4 ) +
    geom_text(aes(label=bar_label), nudge_y = -0.10, hjust=0.5, color=MR$DL_FILL) +
    geom_label(aes(label=callout), nudge_y = 0.07, hjust=0,
               fill="white", color="black", label.r = unit(0, "lines")) +
    scale_y_continuous(limits=c(0,1.1), expand=c(0,0),
                       breaks=breaks_y, labels = labels_y) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    coord_flip() +
    guides(fill=guide_legend(nrow=1))
}
