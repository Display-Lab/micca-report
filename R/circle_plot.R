#' Circle Plot
#' @import dplyr tibble
circle_plot <- function(maptg_data, recip, measure_id, benchmark=NULL, benchmark_label="MICCA\nAve." ){
  plotting_attrs <- tibble(obs=c("numerator","gap","denominator"),
                           ring=c(50,50,58),
                           width=c(16,16,6),
                           fill_color=c(MR$DL_DARK_BLUE, MR$DL_LIGHT_BLUE, MR$DL_LIGHT_BLUE))

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
    dl_annotate("text", x=5, y=denom/2, label=perf_label, size=9, color=MR$DL_DARK_BLUE, fontface=2) +
    dl_annotate("text", x=20, y=0, label=paste(numer, denom, sep="/"),
                size=4, color=MR$DL_DARK_BLUE) +
    top_performer_theme()

  if(!is.null(benchmark)){
    # scale benchmark to observed denominator and invert to accomodate polor coordiate direction=-1
    bench_val <- plot_data %>% filter(obs=="denominator") %>% pull(value) * (1-benchmark)

    fig <- fig +
      scale_x_continuous(limits=c(0,95)) +
      geom_segment(aes(x=40, y = bench_val, xend = 65, yend = bench_val),
                 linetype="dashed", color=MR$DL_GREEN) +
      dl_annotate("text", x=77, y=bench_val, label=benchmark_label,
                size=2.5, color=MR$DL_BLUE)
  }
  fig
}
