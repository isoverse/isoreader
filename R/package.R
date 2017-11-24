#' @keywords internal
"_PACKAGE"

#' @import tidyr
#' @importFrom rlang quo quos UQ UQE !! !!!
#' @importFrom tidyselect everything starts_with ends_with matches
#' @importFrom dplyr select select_ rename rename_ arrange mutate mutate_ filter distinct as_data_frame left_join right_join full_join data_frame bind_rows bind_cols
#' @importFrom glue glue collapse
#' @importFrom purrr map map2 map_lgl map_chr map_df map_int map_dbl
#' @importFrom tibble tribble deframe rownames_to_column
#' @importFrom lubridate interval duration
#' @import stringr
#' @import ggplot2
#' @importFrom stats setNames embed
#' @importFrom methods is
#' @importFrom utils unzip head tail modifyList packageVersion
#' @import xml2
#' @importFrom rhdf5 h5ls h5read h5readAttributes H5close
#' @importFrom lazyeval as.lazy
#' @importFrom UNF unf
#' @importFrom openxlsx createWorkbook createStyle addWorksheet writeData saveWorkbook
#' @importFrom feather write_feather
NULL

# quiets concerns of R CMD check about . that appears in pipelines 
# and some very commonly used variable names used in NSE commands
utils::globalVariables(c(".", "file_id", "mass"))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}