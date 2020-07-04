#' @keywords internal
"_PACKAGE"

#' @importFrom rlang expr enexpr enquo quo quos UQ !! !!! := get_expr quo_squash quo_is_null quo_is_symbol is_quosure is_empty is_integerish eval_tidy sym new_formula f_lhs f_rhs as_label
#' @importFrom tidyselect everything starts_with ends_with matches
#' @importFrom tibble tibble is_tibble
#' @importFrom dplyr vars n select rename arrange desc mutate mutate_at mutate_if filter distinct as_data_frame left_join right_join full_join data_frame bind_rows bind_cols group_by ungroup tally summarize do case_when
#' @importFrom tidyr gather spread nest unnest extract
#' @importFrom glue glue
#' @importFrom purrr map map_lgl map_chr map_df map_int map_dbl map2 map2_chr map2_lgl map2_dbl map2_int safely is_empty
#' @importFrom future plan future availableCores resolved value
#' @importFrom tibble tribble deframe rownames_to_column
#' @importFrom lubridate interval duration as_datetime
#' @importFrom stringr str_c str_detect str_to_title str_replace str_replace_all str_replace_na str_match str_match_all str_interp str_subset str_extract fixed
#' @importFrom methods is
#' @importFrom utils unzip head tail modifyList packageVersion
#' @importFrom xml2 xml_find_all xml_child xml_text read_xml xml_children as_list
#' @importFrom rhdf5 h5ls h5read h5readAttributes H5close
#' @importFrom UNF unf
#' @importFrom openxlsx createWorkbook createStyle addWorksheet writeData saveWorkbook
#' @importFrom feather write_feather
NULL

# re-export rlang !! and !!!
#' @export
rlang::`!!`

#' @export
rlang::`!!!`

# re-export magrittr functions
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# re-export select/rename functions
#' @export
tidyselect::everything

#' @export
tidyselect::starts_with

#' @export
tidyselect::ends_with

#' @export
tidyselect::matches

# re-export dplyr's filter function to overwrite the global filter
#' @export
dplyr::filter

# re-export tibblef or use in examples
#' @export
tibble::tibble

# quiets concerns of R CMD check about . in pipelineds
# and .data in tidyverse functions
utils::globalVariables(".")
#' @importFrom rlang .data

# release questions 
release_questions <- function() {
  c(
    "Is it passing travis, appveyor and win-builder?"
  )
}