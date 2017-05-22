#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import stringr
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