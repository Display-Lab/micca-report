# Figures from initial version of report.
#' @importFrom dplyr %>% filter mutate group_by recode
make_fig7F5D31_data <- function(maptg_data, recip){
  maptg_data %>%
    filter(measure =="M14", ascribee == recip) %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(time) %>%
    mutate(denominator = sum(numerator),
           payer_rate = numerator/denominator)
}

#' @import ggplot2
make_fig7F5D31 <- function(plot_data){
  ggplot(plot_data, aes(x=time, y=payer_rate)) +
    geom_bar(aes(fill=group), stat='identity', color=MR$DL_DARK_BLUE) +
    scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
    ylab("% Women Delivered") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    theme_minimal() +
    theme(legend.position="bottom",  axis.title.x = element_blank(),
          legend.title = element_blank(), legend.box.spacing = unit(0,"mm"))
}

#' @importFrom dplyr %>% filter mutate group_by recode summarize left_join
make_figBE214E_data <- function(maptg_data, recip){
  maptg_data %>%
    filter(measure %in% c("M20", "M21"), ascribee == recip) %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(group, measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(pull(., numerator))) %>%
    mutate(ratio = numerator/denominator) %>%
    left_join(MR$MEASURE_NAMES, by="measure")
}

#' @import ggplot2
make_figBE214E <- function(plot_data){
  ## TODO side by side figures facetted together or cowplotted
  ggplot(plot_data, aes(y=ratio)) +
    geom_bar(aes(fill=short_name,x="Device"), stat='identity') +
    geom_bar(aes(fill=group,x="Payer"), stat='identity') +
    ylab("% of Immediate LARC Provided") +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    theme_minimal() +
    theme(legend.position="bottom",  axis.title.x = element_blank(),  legend.title = element_blank(),
          legend.text = element_text(size=8), legend.key.size = unit(4, "mm"))  +
    guides(fill=guide_legend(nrow=2))
}

#' @importFrom dplyr %>% filter mutate group_by recode summarize left_join select rename
make_table_data_tbl82C4A3 <- function(maptg_data, recip){
  maptg_data %>%
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
}

#' @importFrom dplyr %>% filter mutate group_by recode summarize left_join select rename
make_fig1903AB_data <- function(maptg_data, recip){
  maptg_data %>%
    filter(ascribee == recip,
           measure %in% c("M10", "M11","M12","M13"))  %>%
    mutate(group=recode(group, "medicaid"="Medicaid","non_medicaid"="non-Medicaid")) %>%
    group_by(group, measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(denominator)) %>%
    mutate(rate = numerator/denominator) %>%
    left_join(MR$MEASURE_NAMES, by="measure")
}

#' @import ggplot2
make_fig1903AB <- function(plot_data){
  # Preference of LARC by payer
  ggplot(plot_data, aes(x=group, y=rate)) +
    geom_bar(aes(fill=short_name), stat='identity', color=MR$DL_DARK_BLUE) +
    scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
    scale_fill_manual(values = MR$REPORT_PAL, guide=guide_legend()) +
    theme_minimal() +
    theme(legend.position="bottom", axis.title = element_blank(),
          title = element_text(size=10), legend.title = element_blank(),
          legend.text = element_text(size=8), legend.key.size = unit(4, "mm"),
          legend.box.spacing = unit(0,"mm")) +
    guides(fill=guide_legend(nrow=2))
}

#' @importFrom dplyr %>% filter mutate group_by summarize left_join
make_figE8F578_data <- function(maptg_data, recip){
  maptg_data %>%
    filter(ascribee == recip,
           measure %in% c("M16","M17","M18", "M19"))  %>%
    group_by(measure) %>%
    summarize(numerator = sum(numerator),
              denominator = sum(denominator)) %>%
    left_join(MR$MEASURE_NAMES, by="measure") %>%
    mutate(rate = numerator/denominator,
           short_name = as.factor(short_name),
           mpos = as.numeric(short_name))
}

#' @import ggplot2
make_figE8F578 <- function(plot_data){
  pref_pal <- c("Provided"=MR$DL_BLUE, "Preferred"=MR$DL_MAUVE)
  nudge_factor <- (plot_data %>% pull(denominator) %>% max) /20

  ggplot(plot_data, aes(x=short_name)) +
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
}
