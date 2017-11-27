#' @keywords internal
"_PACKAGE"

#' @import tidyr
#' @importFrom rlang enquo quo quos UQ UQE !! !!! quo_expr quo_text quo_is_symbol is_quosure is_empty
#' @importFrom tidyselect everything starts_with ends_with matches
#' @importFrom dplyr vars select select_ rename rename_ arrange mutate mutate_ mutate_at filter filter_ distinct as_data_frame left_join right_join full_join data_frame bind_rows bind_cols group_by group_by_ ungroup tally summarize
#' @importFrom glue glue collapse
#' @importFrom purrr map map_lgl map_chr map_df map_int map_dbl map2 map2_chr
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