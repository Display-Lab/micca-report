####################
# Common Functions #
####################
dl_annotate <- function(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
                        ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, na.rm = FALSE, ...) {
  args_list <- list(...)
  args_list["color"] <- "#00274C"
  args_list["geom"] <- geom
  args_list["x"] <- x
  args_list["y"] <- list(y)
  do.call(annotate, args = args_list)
}

top_performer_theme <- function(){
  theme_classic() +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.margin = unit(c(-.1,-.1,-.1,-.1),"npc") )
}

single_line_theme <- function(){
  theme_classic() +
    theme(axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.text = element_text(color="#00274c"),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position = "none")
}

micca_mean <- function(maptg_data, measure_id){
  maptg_data %>%
    filter(measure==measure_id) %>% 
    group_by(ascribee) %>%
    summarize(hosp_mean = sum(numerator) / sum(denominator)) %>% 
    ungroup() %>%
    summarize(micca_mean = mean(hosp_mean)) %>% 
    pull(micca_mean) 
}

# Common Circle Plot
circle_plot <- function(maptg_data, recip, measure_id, benchmark=NULL, benchmark_label="MICCA\nAve." ){
  plotting_attrs <- tibble(obs=c("numerator","gap","denominator"),
                           ring=c(50,50,58),
                           width=c(16,16,6),
                           fill_color=c(DL_DARK_BLUE, DL_LIGHT_BLUE, DL_LIGHT_BLUE))
  
  plot_data <- maptg_data %>% 
    filter(measure == measure_id, ascribee == recip) %>%
    group_by(measure) %>%
    summarize(
      numerator = sum(numerator, na.rm=TRUE),
      denominator = sum(denominator, na.rm=TRUE),
      gap = denominator - numerator ) %>%
    pivot_longer(cols=c("denominator","numerator", "gap"), 
                 names_to = "obs", values_to = "value") %>%
    left_join(plotting_attrs)
  
  numer <- plot_data %>% filter(obs=="numerator") %>% pull(value)
  denom <- plot_data %>% filter(obs=="denominator") %>% pull(value)
  perf_label <- paste(floor(100*numer/denom), "%",sep="")
    
  fig <- ggplot(plot_data, aes(x=ring, y=value, fill=fill_color, width=width)) +
    geom_col() +
    scale_fill_identity() +
    scale_x_continuous(limits=c(0,70)) +
    coord_polar(theta="y", direction=-1) +
    dl_annotate("text", x=5, y=denom/2, label=perf_label, size=9, color=DL_DARK_BLUE, fontface=2) +
    dl_annotate("text", x=20, y=0, label=paste(numer, denom, sep="/"),
                size=4, color=DL_DARK_BLUE) +
    top_performer_theme()
  
  if(!is.null(benchmark)){
    # scale benchmark to observed denominator and invert to accomodate polor coordiate direction=-1
    bench_val <- plot_data %>% filter(obs=="denominator") %>% pull(value) * (1-benchmark)
    
    fig <- fig +
      scale_x_continuous(limits=c(0,95)) +
      geom_segment(aes(x=40, y = bench_val, xend = 65, yend = bench_val),
                 linetype="dashed", color=DL_GREEN) +
      dl_annotate("text", x=77, y=bench_val, label=benchmark_label,
                size=2.5, color=DL_BLUE)
  }
  fig
}

# Common Line Plot
line_plot <- function(maptg_data, recip, measure_id){
  recipient_title <- ASCRIBEE_TITLES[recip]
  
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
    mutate(ascribee = ASCRIBEE_TITLES[ascribee]) %>%
    bind_rows(micca_data) %>%
    group_by(ascribee, time) %>%
    summarize(
      numerator = sum(numerator, na.rm=TRUE),
      denominator = sum(denominator, na.rm=TRUE),
      rate = numerator/denominator) %>%
    mutate(
      recipient = ifelse(ascribee == recipient_title, T, F),
      perf_label = ifelse(recipient, paste(numerator, denominator, sep="/"), NA),
      arrow = ifelse(recipient, "show", "noshow"),
      pcolor = ifelse(recipient, DL_DARK_BLUE, DL_GRAY)) 
  
  # y axis labels
  breaks_y <- c(0.20, 0.4, 0.6, 0.8, 1.0)
  labels_y <- c("20%", "40%", "60%", "80%", "100%")
  
  ggplot(data=plot_data, aes(x=time, y=rate, color=ascribee)) +
    geom_line(size=1, lineend="round") +
    geom_point(size=2, fill=DL_FILL, shape=21, stroke=1.2) +
    scale_y_continuous(limits=c(0,1.15), expand=c(0,0), breaks=breaks_y, labels = labels_y) +
    scale_x_date(date_labels = "%b", expand=c(0.1,0), breaks=unique(plot_data$time)) +
    scale_shape_manual(values = c("show"=18, "noshow"=NA), guide = FALSE) +
    geom_point(mapping = aes(y = rate + 0.07, shape=arrow), size=4, color=DL_DARK_BLUE) +
    geom_label(mapping = aes(label=perf_label), nudge_y = 0.13, fill=DL_DARK_BLUE,
               color=DL_FILL, label.r = unit(0, "lines"), label.size=0) +
    single_line_theme() +
    scale_color_manual(labels = unique(plot_data$ascribee), 
                       values = unique(plot_data$pcolor), guide = guide_legend(title=NULL)) +
    theme(legend.position="bottom", legend.box.spacing = unit(0,"mm"))
}

numerator_sum <- function(maptg_data, measure_id, recipient){
m14_sum <- maptg_data %>% 
  filter(measure == measure_id, ascribee == recipient) %>% 
  pull(numerator) %>% 
  sum()
}