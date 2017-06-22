#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @importFrom purrr map map_lgl map_chr map_df sort_by
#' @importFrom tibble tribble
#' @importFrom lubridate interval duration
#' @import stringr
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom methods is
#' @importFrom utils unzip head tail modifyList packageVersion
#' @import xml2
#' @importFrom rhdf5 h5ls h5read h5readAttributes H5close
#' @importFrom lazyeval as.lazy
#' @importFrom UNF unf
NULL

## quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c(".", "y"))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}