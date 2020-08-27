####################
# Common Functions #
####################
# Useful for generating figure ids
nameit <- function(){paste0('fig', paste0(sample(c(0:9, LETTERS[1:6]), 6, T), collapse=''))}

# Display lab plot annotation wrapper
#' @import ggplot2
dl_annotate <- function(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
                        ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, na.rm = FALSE, ...) {
  args_list <- list(...)
  args_list["color"] <- "#00274C"
  args_list["geom"] <- geom
  args_list["x"] <- x
  args_list["y"] <- list(y)
  do.call(annotate, args = args_list)
}

#' @import dplyr
micca_mean <- function(maptg_data, measure_id){
  maptg_data %>%
    filter(measure==measure_id) %>%
    group_by(ascribee) %>%
    summarize(hosp_mean = sum(numerator) / sum(denominator)) %>%
    ungroup() %>%
    summarize(micca_mean = mean(hosp_mean)) %>%
    pull(micca_mean)
}

#' @import dplyr
#' @importFrom lubridate ymd
numerator_sum <- function(maptg_data, measure_id, recipient){
  # V2 One off - limit to Q2
  q2_times <- c(ymd('2020-04-01'), ymd('2020-05-01'), ymd('2020-06-01'))

  maptg_data %>%
    filter( time %in% q2_times,
            measure == measure_id,
            ascribee == recipient) %>%
    pull(numerator) %>%
    sum()
}
