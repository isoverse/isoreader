#' Renamed to iso_filter_files_with_problems
#' 
#' This function has been renamed to \link{iso_filter_files_with_problems} for naming consistency.
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
    filter(type %in% types) %>% 
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

#' Filter iso_files
#' 
#' Filter for specific iso_files using file info columns (\code{\link{iso_get_file_info}}). Works just like dplyr's \link[dplyr]{filter} except that it provides the user with some information on what has been filtered. You can also use \link[dplyr]{filter} directly to filter collections of \code{iso_file} objects.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{filter} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}})
#' @export 
iso_filter_files <- function(iso_files, ..., quiet = default(quiet)) {
  # safety checks
  if(!iso_is_file_list(iso_files)) stop("can only filter collections of iso files", call. = FALSE)
  filtered_iso_files <- filter(iso_files, ...)
  
  # information
  if (!quiet) {
    str_interp("Info: applying file filter, keeping $[d]{n} of $[d]{n_all} files", 
               list(n = length(filtered_iso_files), n_all = length(iso_files))) %>% message()
  }
  
  return(filtered_iso_files)
}

#' @export
filter.iso_file_list <- function(.data, ..., .preserve = FALSE) {
  # filter iso_files by file info
  file_info <- iso_get_file_info(.data, quiet = TRUE) %>% dplyr::filter(...)
  # return filtered iso_files
  if (nrow(file_info) == 0) return(NULL)
  else .data[names(.data) %in% file_info$file_id]
}