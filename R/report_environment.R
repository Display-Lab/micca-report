#' Create base report environment with required libraries in scope
#' @import knitr tikzDevice
#' @importFrom kableExtra kable_styling row_spec footnote
report_environment <- function(){
  new.env()
}
