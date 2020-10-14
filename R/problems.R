#' @title Problem Functions Overview
#' @description The following functions to check for and deal with problems are available.
#' 
#' @details
#' \itemize{
#' \item \code{iso_get_problems} is a re-export of \code{\link[readr]{problems}}
#' 
#' \item \code{\link{iso_get_problems_summary}}
#' 
#' \item \code{\link{iso_has_problems}}
#' 
#' \item \code{\link[readr:problems]{stop_for_problems}}
#' 
#' \item \code{\link{iso_filter_files_with_problems}}
#' }
#' @name iso_problem_functions
#' @family problem functions
NULL

#' Check for parsing problems
#' @inheritParams iso_get_raw_data
#' @family problem functions
#' @return boolean
#' @export
iso_has_problems <- function(iso_files) {
 
  # safety checks
  if (missing(iso_files) || !iso_is_object(iso_files)) stop("please provide iso_files", call. = FALSE)
  iso_files <- iso_as_file_list(iso_files)
  
  return(n_problems(iso_files) > 0)
}

#' @importFrom readr problems
#' @export
readr::problems

#' Retrieve parsing problems
#' 
#' This function retrieves parsing problems encountered during the reading of a set of iso files.
#' 
#' @importFrom readr problems
#' @inheritParams iso_get_raw_data
#' @family problem functions
#' @export
iso_get_problems <- function(iso_files, select = everything()) {
  probs <- problems(iso_as_file_list(iso_files))
  select_cols <- get_column_names(probs, select = enquo(select), n_reqs = list(select = "*"), cols_must_exist = FALSE)$select
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  return(dplyr::select(probs, !!!select_cols))
}

#' @importFrom readr stop_for_problems
#' @export
readr::stop_for_problems

#' Retrieve a summary of the problems
#'
#' Returns a data frame listing how many errors and warnings were encountered for each file. For details on each error/warning, see \link[readr]{problems} and the \link{iso_problem_functions}.
#' @inheritParams iso_get_raw_data
#' @param problem_files_only whether to list only problem files or all files
#' @family problem functions
#' @return data frame with file_id and number of encountered errors and warnings
#' @export
iso_get_problems_summary <- function(iso_files, problem_files_only = TRUE, include_file_info = NULL) {
  # safety checks
  if (missing(iso_files) || !iso_is_object(iso_files)) stop("please provide iso_files", call. = FALSE)
  iso_files <- iso_as_file_list(iso_files)
  include_file_info_quo <- rlang::enquo(include_file_info)
  
  # tally up problems
  probs_templ <- tibble(file_id = character(0), error = integer(0), warning = integer(0))
  if (n_problems(iso_files) > 0) {
    probs <- problems(iso_files) %>% 
      # tally up number of warnings/errors per file
      group_by(.data$file_id, .data$type) %>%
      tally() %>% 
      spread(.data$type, .data$n) %>%
      # to ensure these columns exists
      bind_rows(probs_templ) %>% 
      ungroup()
  } else {
    probs <- probs_templ
  }
  
  if (!problem_files_only) {
    # merge with file list to get all listed
    probs <- tibble(
      file_id = names(iso_files)
    ) %>%
      left_join(probs, by = "file_id") 
  }
  
  # finalize data frame
  probs <- probs %>%
    mutate(
      warning = ifelse(!is.na(.data$warning), .data$warning, 0L),
      error = ifelse(!is.na(.data$error), .data$error, 0L)
    ) 
  
  # if file info
  if (!quo_is_null(include_file_info_quo)) {
    info <- iso_get_file_info(iso_files, select = !!include_file_info_quo, quiet = TRUE)
    probs <- right_join(info, probs, by = "file_id")
  }
  
  return(probs)
}

# readr style function for warning about problems
warn_problems <- function(x, cutoff = 5L, width = getOption("width")) {
  stopifnot(cutoff > 1L)
  n <- n_problems(x)
  if (n == 0) return(invisible(x))
  has_many_problems <- n > cutoff
  probs <- iso_get_problems(x) %>%
    dplyr::mutate(i = as.character(dplyr::row_number())) %>% 
    dplyr::select(c("i", "file_id", "type", "func", "details"))
  probs_list <-
    base::rbind(
      c("#", "FILE", "PROBLEM", "OCCURRED IN", "DETAILS"), 
      if (has_many_problems) utils::head(probs, cutoff - 1L) else probs
    ) %>% as.list()
    
  # add .... entries at end if there are too many problems
  if (has_many_problems) {
    widths <- purrr::map_int(probs_list, ~max(nchar(.x), 1L))
    dots <- purrr::map(widths, ~paste(rep(".", .x), collapse = ""))
    probs_list <- purrr::map2(probs_list, dots, c)
    probs_list$i[cutoff + 1L] <- sprintf("%d-%d", cutoff, n)
  }
  
  # format problems
  probs_f <- purrr::map(probs_list, format, justify = "left")
  
  # format lines to account for max display width
  probs_lines <- do.call(paste, c(probs_f, list(sep = " | "))) %>% 
    stringr::str_trim(side = "right")
  line_widths <- purrr::map_int(probs_lines, nchar)
  probs_lines[line_widths > width] <-
    purrr::map_chr(
      probs_lines[line_widths > width],
      ~paste0(stringr::str_sub(.x, 1L, width - 3L), "...")
    )
  # output warning
  warning(
    "encountered ", n, " problem", if (n > 1) "s", ".\n", 
    paste(probs_lines, collapse = "\n"), 
    "\nUse iso_get_problems(...) for more details.", 
    call. = FALSE, immediate. = TRUE, noBreaks. = TRUE
  )
  return(invisible(x))
}

#' Renamed to iso_filter_files_with_problems
#' 
#' This function has been renamed to \link{iso_filter_files_with_problems} for naming consistency.
#' @param ... deprecated
#' @export
iso_omit_files_with_problems <- function(...) {
  warning("iso_filter_files_with_problems() was renamed and will be removed in a future version of the isoreader package. Please use iso_filter_files_with_problems() directly instead to make your code future-proof.", immediate. = TRUE, call. = FALSE)
  iso_filter_files_with_problems(...)
}

#' Filter out problematic files
#' 
#' Use this function to filter out files that have encountered problems, either errors, warnings or both and returns the remaining iso_files. For additional functions available to check for and deal with problems, see the \link{iso_problem_functions}.
#' @inheritParams iso_get_raw_data
#' @param remove_files_with_errors whether to remove files with errors (default is TRUE)
#' @param remove_files_with_warnings whether to remove files with warnings (default is FALSE)
#' @family problem functions
#' @export
iso_filter_files_with_problems <- function(iso_files, remove_files_with_errors = TRUE, remove_files_with_warnings = FALSE, quiet = default(quiet)) {
  
  if (missing(iso_files) || !iso_is_object(iso_files)) stop("please provide a list of iso_files", call. = FALSE)
  types <- c()
  if (remove_files_with_errors) types <- c(types, "error")
  if (remove_files_with_warnings) types <- c(types, "warning")
  if (length(types) == 0) return(iso_files)
  iso_files <- iso_as_file_list(iso_files)
  
  # find trouble file ids
  trouble_files <- problems(iso_files) %>% 
    filter(.data$type %in% types) %>% 
    { unique(.$file_id) }
  
  # exclude
  exclude <- names(iso_files) %in% trouble_files
  if (!quiet) {
    sprintf("Info: removing %d/%d files that have any %s (keeping %d)", 
            sum(exclude), length(iso_files), 
            collapse(types, ", ", last = " or "),
            length(iso_files) - sum(exclude)) %>% message()
  }
  return(iso_files[!exclude])
}

# register a problem during isoreader operations
# helper function to standardize problems for file reads
# with a filename, type and details
# will propage the problem to all underlying files
# @obj iso_file or iso_file_list
# @param keep_duplicats - whether to keep identical copies of the same problems 
register_problem <- function(obj, type = NA_character_, details = NA_character_, ..., 
                                  func = find_parent_call("register_problem"), keep_duplicates = FALSE) {
  if (func == "NULL") func <- NA_character_
  problem <- tibble(type = type, func = func, details = details, ...)
  if (iso_is_file_list(obj)) {
    obj <- as.list(obj)
    for (i in 1:length(obj)) {
      existing_problems <- get_problems(obj[[i]])
      all_problems <- suppressWarnings(bind_rows(existing_problems, problem))
      if (!keep_duplicates) all_problems <- unique(all_problems)
      obj[[i]] <- set_problems(obj[[i]], all_problems)
    } 
    obj <- iso_as_file_list(obj)
  } else {
    all_problems <- suppressWarnings(bind_rows(get_problems(obj), problem))
    if (!keep_duplicates) all_problems <- unique(all_problems)
    obj <- obj %>% set_problems(all_problems)
  }
  return(obj)
}

# register warning
register_warning <- function(obj, details = NA_character_, ..., 
                             func = find_parent_call("register_warning"), warn = TRUE) {
  force(func)
  if (warn) log_warning(details)
  register_problem(obj, type = "warning", details = details, func = func, ...)
}

# register error
register_error <- function(obj, details = NA_character_, ..., 
                           func = find_parent_call("register_error"), warn = TRUE) {
  force(func)
  if (warn) log_warning(paste0("caught error - ", details))
  register_problem(obj, type = "error", details = details, func = func, ...)
}


# set problems
set_problems <- function(obj, problems) {
  attr(obj, "problems") <- problems
  return(obj)
}

# get problems (for internal use only, external use is readr::problems)
get_problems <- function(x) {
  probs <- attr(suppressWarnings(x), "problems")
  if (is.null(probs)) {
    # return empty initialized problems structure
    return(get_problems_structure())
  } 
  return(probs)
}

# combine problems
# @return problems data frame (needs to be assigned with set_problems)
combined_problems <- function(...) {
  objs <- list(...)
  suppressWarnings(
    lapply(objs, get_problems) %>% 
      bind_rows()
  )
}

# isoreader problems structure
get_problems_structure <- function() {
  tibble(
    type = character(),
    func = character(),
    details = character()
  )
}

# initialize problems attribute
initialize_problems_attribute <- function(obj) {
  if (n_problems(obj) == 0) {
    obj <- set_problems(obj, get_problems_structure())
  }
  return(obj)
}

# equivalent approach to readr (not exported there)
n_problems <- function(x) {
  probs <- attr(suppressWarnings(x), "problems")
  if (is.null(probs)) 0 else nrow(probs)
}

