#' Horizontal Bar Plot
#' @param data long form tabular data in maptg format
#' @param recip name of recipient as it appears in the data
#' @param measures vector of measure ids from which to create bars.
horizontal_bar_plot <- function(data, recip, measures){
  plot_data <- hbar_plot_data(data, recip, measures)
  hbar_plot_fig(plot_data)
}

#' @importFrom dplyr %>% filter mutate group_by summarize mutate case_when left_join
#' @param data data frame of MAPTG format data
#' @note after_stat transoform in hbar_plot_fig considers 0's missing data.
#'   workaround using .001 for 0 in rate
hbar_plot_data <- function(data, recip, measures){
  data %>%
    filter(ascribee == recip,
           measure %in% measures)  %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(group, measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(denominator),
              label = paste(numerator,denominator, sep="/")) %>%
    mutate(rate = numerator/denominator,
           rate = case_when(
             rate == 0 ~ 0.001,
             is.nan(rate) ~ 0.001,
             TRUE ~ rate),
           x_cat = paste(measure, group, sep="-"),
           x_adjust = case_when(
             group == "Medicaid" ~ 0.2,
             group == "non-Medicaid" ~ -0.2,
             TRUE ~ 0 ),
           callout = ifelse(rate < 0.81, label, NA),
           bar_label = ifelse(is.na(callout), label, NA)
           ) %>%
    left_join(MR$MEASURE_NAMES, by="measure")
}

#' @import ggplot2
hbar_plot_fig <- function(plot_data){
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")
  x_names <- plot_data$short_name %>% unique()
  col_width = 0.6

  ggplot(plot_data,
         aes(x=short_name,
             y=rate, label=label, fill=group, x_adjust=x_adjust)) +
    single_bar_theme() +
    geom_col(width = col_width, position=position_dodge() ) +
    geom_text(aes(y=stage(start=rate, after_scale(y-0.1)),
                  label=bar_label),
               position = position_dodge(width = col_width),
               hjust=0.5, color=MR$DL_FILL, size=2) +
    geom_text(aes(y=stage(start=rate, after_scale(y+0.07)),
                   label=callout),
               position=position_dodge(width=col_width),
               hjust=0, color="black", size=2) +
    scale_y_continuous(limits=c(0,1.1), expand=expansion(0,add=c(0,.1)),
                       breaks=breaks_y, labels = labels_y) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    coord_flip() +
    guides(fill=guide_legend(nrow=1))
}
