#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @importFrom purrr map_lgl map
#' @importFrom tibble tribble
#' @import stringr
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom methods is
#' @import xml2
#' @importFrom rhdf5 h5read h5readAttributes
#' @importFrom lubridate duration
NULL

## quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c(".", "y"))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}