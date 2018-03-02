#' @keywords internal
"_PACKAGE"

#' @import tidyr
#' @importFrom rlang enquo quo quos UQ UQE !! !!! quo_expr quo_text quo_is_null quo_is_symbol quo_is_lang is_quosure is_empty is_integerish eval_tidy sym lang_head
#' @importFrom tidyselect everything starts_with ends_with matches vars_select
#' @importFrom dplyr vars n select select_ rename rename_ arrange desc mutate mutate_ mutate_at filter filter_ distinct as_data_frame left_join right_join full_join data_frame bind_rows bind_cols group_by group_by_ ungroup tally summarize do
#' @importFrom glue glue collapse
#' @importFrom purrr map map_lgl map_chr map_df map_int map_dbl map2 map2_chr map2_lgl safely
#' @importFrom tibble tribble deframe rownames_to_column
#' @importFrom lubridate interval duration as_datetime
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
utils::globalVariables(c(".", "file_id", "mass", "quiet"))

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}