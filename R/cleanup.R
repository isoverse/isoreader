# cleanup functions of data read from files

#' @importFrom readr parse_number parse_logical parse_integer parse_double parse_datetime
#' @export
readr::parse_number

#' @export
readr::parse_logical

#' @export
readr::parse_integer

#' @export
readr::parse_double

#' @export
readr::parse_datetime

#' Overview of text data extraction functions
#'
#' The following functions are intended to make it easy to extract relevant information from textual data.
#' These functions are primarily intended for use in \code{\link{iso_mutate_file_info}} and inside the filtering conditions passed to \code{\link{iso_filter_files}}. However, they can of course also be used stand-alone and in regular \code{\link[dplyr]{mutate}} or \code{\link[dplyr]{filter}} calls on the data frames returned by the data retrieval functions (\code{\link{iso_get_raw_data}}, \code{\link{iso_get_file_info}}, \code{\link{iso_get_vendor_data_table}}, etc.). Not that all the \code{parse_} functions are used in \code{\link{iso_parse_file_info}} for easy type conversions.
#'
#' For simultaneous extraction of pure text data into multiple columns, please see the \code{\link[tidyr]{extract}} function from the \link{tidyr} package.
#'
#' \itemize{
#' \item \code{\link{extract_substring}} is a generic convenience function to extract parts of textual data (based on regular expression matches).
#' Can be used in combination with the parsing functions to turn extracted substrings into numerical or logical data.
#'
#' \item \code{\link{extract_word}} is a more specific convenience function to extract the 1st/2nd/3rd word from textual data.
#'
#' \item \code{\link[readr:parse_atomic]{parse_number}} is a convenience function to extract a number even if it is surrounded by text (re-exported from the \link{readr} package).
#'
#' \item \code{\link[readr:parse_atomic]{parse_double}} parses text that holds double (decimal) numerical values without any extraneous text around -
#' use \code{\link[readr:parse_atomic]{parse_number}} instead if this is not the case (re-exported from the \link{readr} package)
#'
#' \item \code{\link[readr:parse_atomic]{parse_integer}} parses text that holds integer (whole number) numerical values without any extraneous text around -
#' use \code{\link[readr:parse_atomic]{parse_number}} instead if this is not the case (re-exported from the \link{readr} package)
#'
#' \item \code{\link[readr:parse_atomic]{parse_logical}} parses text that holds logical (boolean, i.e. TRUE/FALSE) values (re-exported from the \link{readr} package)
#'
#' \item \code{\link[readr:parse_atomic]{parse_datetime}} parses text that holds date and time information (re-exported from the \link{readr} package)
#'
#' }
#' @name extract_data
#' @family data extraction functions
NULL


#' Extract a substring from text
#'
#' This is a convenience function to capture substrings from textual data.
#' Uses \code{\link[stringr:str_match]{str_match_all}} internally but instead of returning everything, always returns only one single part of the match, depending on parameters \code{capture_n} and \code{capture_group}.
#'
#' @param string string to extract
#' @param pattern regular expression pattern to search for
#' @param capture_n within each string, which match of the \code{pattern} should be extracted? e.g. if the pattern searches for words, should the first, second or third word be captured?
#' @param capture_bracket for the captured match, which capture group should be extracted? i.e. which parentheses-enclosed segment of the \code{pattern}?
#' by default captures the whole pattern (\code{capture_bracket = 0}).
#' @param missing what to replace missing values with? Note that values can be missing because there are not enough captured matches or because the actual capture_bracket is empty.
#' @return character vector of same length as \code{string} with the extracted substrings
#' @family data extraction functions
#' @export
extract_substring <- function(string, pattern, capture_n = 1, capture_bracket = 0, missing = NA_character_) {

  # safety checks
  if (missing(string)) stop("no string supplied", call. = FALSE)
  if (missing(pattern)) stop("no extraction pattern supplied", call. = FALSE)
  if (length(string) == 0) return(c())

  # find matches
  matches <- str_match_all(string, pattern)

  # safety checks on capture backets (are there enough?)
  if (ncol(matches[[1]]) < (capture_bracket+1))
    stop(glue("regexp capture group {capture_bracket} requested but only {ncol(matches[[1]])-1} groups captured"), call. = FALSE)

  # get captured groups
  captured <- map_chr(matches, function(x) if (nrow(x) < capture_n) return(NA_character_) else x[capture_n, capture_bracket+1])
  captured[is.na(captured)] <- missing
  return(captured)
}

#' Extract words from text
#'
#' This extracts words from text, by default looks for continuous sequences of numbers and/or letters.
#' Can adjust whether characters such as "_", "-", " ", and "." should be counted as part of a word or separate them and whether numbers should be included.
#'
#' @inheritParams extract_substring
#' @param capture_n which word to extract? 1st, 2nd, 3rd?
#' @param include_numbers whether to include numbers (0-9) as part of the word (if FALSE, numbers will work as a word separator)
#' @param include_underscore whether to include the underscore character (_) as part of a word (if FALSE, it will work as a word separator)
#' @param include_dash whether to include the dash character (-) as part of a word (if FALSE, it will work as a word separator)
#' @param include_space whether to include the space character ( ) as part of a word (if FALSE, it will work as a word separator)
#' @param include_colon whether to include the colon character (.) as part of a word (if FALSE, it will work as a word separator)
#' @family data extraction functions
#' @examples
#' x_text <- extract_word(c("sample number16.2", "sample number7b"),
#'                        capture_n = 2, include_colon = TRUE)
#' # "number16.2" "number7b"
#' x_num <- parse_number(x_text)
#' # 16.2 7.0
#' @export
extract_word <- function(string, capture_n = 1, include_numbers = TRUE, include_underscore = FALSE, include_dash = FALSE, include_space = FALSE, include_colon = FALSE, missing = NA_character_) {
  chr_numbers <- if(include_numbers) "0-9" else ""
  chr_underscore <- if(include_underscore) "_" else ""
  chr_dash <- if(include_dash) "-" else ""
  chr_space <- if(include_space) " " else ""
  chr_colon <- if(include_colon) "." else ""
  pattern <- glue("[{chr_numbers}A-Za-z{chr_underscore}{chr_dash}{chr_space}{chr_colon}]+") %>% as.character()
  extract_substring(string, pattern = pattern, capture_n = capture_n, missing = missing)
}
