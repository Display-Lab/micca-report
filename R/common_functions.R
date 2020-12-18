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

#' @importFrom dplyr %>% filter group_by summarize ungroup pull
#' @note Munson data is filtered out as it's not a full cohort nor representative sample.
micca_mean <- function(data, measure_id){
  data %>%
    filter(measure == measure_id,
           ascribee != "Munson Medical Center") %>%
    group_by(ascribee) %>%
    summarize(hosp_mean = sum(numerator) / sum(denominator)) %>%
    ungroup() %>%
    summarize(micca_mean = mean(hosp_mean)) %>%
    pull(micca_mean)
}

#' @importFrom dplyr %>% filter pull
#' @importFrom lubridate ymd
numerator_sum <- function(maptg_data, measure_id, recipient){
  maptg_data %>%
    filter( measure == measure_id,
            ascribee == recipient) %>%
    pull(numerator) %>%
    sum()
}
