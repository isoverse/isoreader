#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @importFrom purrr map_lgl
#' @importFrom tibble tribble
#' @import stringr
#' @import ggplot2
#' @importFrom stats setNames
NULL

## quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c(".", "y"))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}

#' read isotope data file
#' 
#' Deprecated, use [isoread_dual_inlet()], [isoread_continuous_flow()] and [isoread_scan()] instead.
#'
#' @export
isoread <- function(...) {
  stop(
    "Deprecated, use isoread_dual_inlet(), isoread_continuous_flow() or isoread_scan() instead.",
    call. = FALSE)
}