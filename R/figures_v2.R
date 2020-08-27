# Figures for one off v2 version of report.

#' @import dplyr
make_figFEA046_data <- function(maptg_data, recip){
  #maptg_data %>%
  plot_data <- maptg_data %>%
    filter(ascribee == recip,
           measure %in% c("M10", "M11","M12","M13"))  %>%
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
make_figFEA046 <- function(plot_data){
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")
  x_names <- plot_data$short_name %>% unique()

  #ggplot(data=df, aes(x=performers, y=lengths)) +
    #geom_text(nudge_y = -0.10, color=MR$DL_FILL) +
    #geom_label(nudge_y = 0.15, fill="white", color="black", label.r = unit(0, "lines")) +
    #geom_point(mapping = aes(y = rate + 0.07), shape=23, size=2.5, fill=MR$DL_BLUE, color=MR$DL_BLUE) +
    #scale_x_discrete(x_names, expand=expansion(add=c(0.65,2))) +
    #scale_x_discrete(labels=plot_data$short_name) +
  ggplot(plot_data,
         aes(x=stage(short_name, after_stat = (x + x_adjust)),
             y=rate, label=label, fill=group, x_adjust=x_adjust)) +
    single_bar_theme() +
    geom_col(width = .4 ) +
    geom_text(aes(label=bar_label), nudge_y = -0.10, hjust=0.5, color=MR$DL_FILL) +
    geom_label(aes(label=callout), nudge_y = 0.07, hjust=0,
               fill="white", color="black", label.r = unit(0, "lines")) +
    #geom_text(color=MR$DL_FILL, position = position_dodge(width = 0.8)) +
    #geom_label(fill="white", color="black", label.r = unit(0, "lines"), position = position_dodge(width=0.8)) +
    scale_y_continuous(limits=c(0,1.1), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    coord_flip() +
    guides(fill=guide_legend(nrow=1))
}
