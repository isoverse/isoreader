# dplyr style functions that operate on file info

# rename & select utils ========

# internal function for rename and select
select_rename_isofile <- function(isofile, func, quos) {
  old_vars <- names(isofile$file_info)
  new_vars <- func(old_vars, !!!quos, .strict = FALSE)
  # make sure file_id is always included
  if (!"file_id" %in% new_vars)
    new_vars <- c(c(file_id = "file_id"), new_vars)
  
  # change vars data frame
  vars <- tibble(
    file = isofile$file_info$file_id,
    from  = as.character(new_vars),
    to = names(new_vars),
    changed = !to %in% old_vars
  )
  
  # check on file_id rename
  check_vars <- "file_id"
  if (any(prob <- check_vars %in% filter(vars, changed)$from)) {
    glue::glue("renaming the '{paste(check_vars[prob], collapse = \"', '\")}' column ",
               "may lead to unpredictable behaviour and is therefore not allowed, sorry") %>% 
      stop(call. = FALSE)
  }
  isofile$file_info <- dplyr:::select_impl(isofile$file_info, new_vars)
  return(list(isofile = isofile, vars = vars))
}

# internal function to check for rename/select duplicates
check_names_changes <- function(vars) {
  reps <- vars %>% group_by(file, to) %>% 
    summarize(n = n(), from = paste(from, collapse = "', '")) %>% 
    ungroup() %>% 
    filter(n > 1)
  if (nrow(reps)> 0) {
    labels <- reps %>% select(to, from) %>% unique() %>% 
      mutate(label = paste0(" - '", to, "' <= '", from, "'")) %>% 
      { paste(.$label, collapse = "\n") }
    glue::glue("the following column(s) would be assigned to the same name in at ",
               "least 1 file leading to an unresolvable naming conflict:\n{labels}") %>% 
      stop(call. = FALSE)
  }
}

# select ==================

#' Select file info columns
#' 
#' Select which file info columns (\code{\link{iso_get_file_info}}) to keep within isofile objects. Works just like dplyr's \link[dplyr]{select} except that it can select different columns in different iso_files depending on what exists in each file (also works for on-the-fly renaming of columns). This is very useful when working with data from multiple instruments that may have the same information (e.g. sample name) stored in different columns. You can also use \link[dplyr]{select} directly but it will not provide summary information on the operation. To rename columns without removing all other information, use \link{iso_rename_file_info} instead.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{select} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}}). Note that the \code{file_id} column will always be kept, no matter the selection criteria, and cannot be renamed to protect from unexpected behaviour.
#' @family file_info operations
#' @export 
iso_select_file_info <- function(iso_files, ..., quiet = default(quiet)) {
  UseMethod("iso_select_file_info")
}

#' @export
iso_select_file_info.default <- function(iso_files, ..., quiet = default(quiet)) {
  stop("this function is not defined for objects of type '", 
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_select_file_info.iso_file <- function(iso_files, ..., quiet = default(quiet)) {
  iso_select_file_info(iso_as_file_list(iso_files), ..., quiet = quiet)[[1]]
}

#' @export
iso_select_file_info.iso_file_list <- function(iso_files, ..., quiet = default(quiet)) {
  # variables for all files
  select_quos <- quos(...)
  
  # select
  isofiles_select <- map(iso_files, select_rename_isofile, tidyselect::vars_select, select_quos)
  
  # info summary
  all_vars <- map(isofiles_select, "vars") %>% bind_rows()
  
  # check for duplicates
  check_names_changes(all_vars)
  
  # summary information
  if (!quiet) {
    info <- all_vars %>% group_by(to, from) %>% 
      summarize(
        n = n(),
        label = 
          ifelse(
            changed[1],
            sprintf(" - '%s' (renamed to '%s') in %d files", from[1], to[1], n),
            sprintf(" - '%s' in %d files", from[1], n)
          ))
    glue::glue("Info: keeping {length(unique(all_vars$from))} file info column(s) ",
               "wherever they exist across {length(iso_files)} isofile(s):\n",
               "{paste(info$label, collapse = '\n')}") %>% 
      message()
  }
  
  return(iso_as_file_list(map(isofiles_select, "isofile")))
}

#' @export
select.iso_file <- function(.data, ...) {
  iso_select_file_info(.data, ..., quiet = TRUE)
}

#' @export
select.iso_file_list <- function(.data, ...) {
  iso_select_file_info(.data, ..., quiet = TRUE)
}



# rename ==================

#' Rename file info columns
#' 
#' Rename individual file info columns (\code{\link{iso_get_file_info}}) within isofile objects. Works just like dplyr's \link[dplyr]{rename} except that it can rename different columns into the same name in different iso_files depending on what exists in each file. This is very useful when working with data from multiple instruments that may have the same information (e.g. sample name) stored in different columns. You can also use \link[dplyr]{rename} directly but it will not provide summary information on the operation. To select specific columns to keep (discarding all others), use \link{iso_select_file_info} instead.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{rename} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}})
#' @family file_info operations
#' @export 
iso_rename_file_info <- function(iso_files, ..., quiet = default(quiet)) {
  UseMethod("iso_rename_file_info")
}

#' @export
iso_rename_file_info.default <- function(iso_files, ..., quiet = default(quiet)) {
  stop("this function is not defined for objects of type '", 
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_rename_file_info.iso_file <- function(iso_files, ..., quiet = default(quiet)) {
  iso_rename_file_info(iso_as_file_list(iso_files), ..., quiet = quiet)[[1]]
}

#' @export
iso_rename_file_info.iso_file_list <- function(iso_files, ..., quiet = default(quiet)) {
  # variables for all files
  rename_quos <- quos(...)
  
  # rename
  isofiles_rename <- map(iso_files, select_rename_isofile, tidyselect::vars_rename, rename_quos)
  
  # info summary
  all_vars <- map(isofiles_rename, "vars") %>% bind_rows()
  
  # check for duplicates
  check_names_changes(all_vars)
  
  # summary information
  if (!quiet) {
    info <- all_vars %>% filter(changed) %>% group_by(to, from) %>% tally() %>% 
      mutate(label = sprintf(" - '%s' to '%s' in %d files", from, to, n))
    total_n <- all_vars %>% filter(changed) %>% select(from) %>% unique()
    glue::glue("Info: renaming {nrow(total_n)} file info column(s) ",
               "wherever they exist across {length(iso_files)} isofile(s):\n",
               "{paste(info$label, collapse = '\n')}") %>% 
      message()
  }
  
  return(iso_as_file_list(map(isofiles_rename, "isofile")))
}

#' @export
rename.iso_file <- function(.data, ...) {
  iso_rename_file_info(.data, ..., quiet = TRUE)
}

#' @export
rename.iso_file_list <- function(.data, ...) {
  iso_rename_file_info(.data, ..., quiet = TRUE)
}

# filter ==================


#' Filter iso_files
#' 
#' Filter for specific isofiles using file info columns (\code{\link{iso_get_file_info}}). Works just like dplyr's \link[dplyr]{filter} except that it provides the user with some information on what has been filtered. Returns \code{NULL} if none of the isofiles' file info matches the filter criteria. You can also use \link[dplyr]{filter} directly to filter collections of \code{iso_file} objects.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{filter} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}})
#' @family file_info operations
#' @export 
iso_filter_files <- function(iso_files, ..., quiet = default(quiet)) {
  UseMethod("iso_filter_files")
} 
 
#' @export
iso_filter_files.default <- function(iso_files, ..., quiet = default(quiet)) {
  stop("this function is not defined for objects of type '", 
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_filter_files.iso_file <- function(iso_files, ..., quiet = default(quiet)) {
  iso_filter_files(iso_as_file_list(iso_files), ..., quiet = quiet)[[1]]
}

#' @export
iso_filter_files.iso_file_list <- function(iso_files, ..., quiet = default(quiet)) {
  # filter iso_files by file info
  file_info <- iso_get_file_info(iso_files, quiet = TRUE) %>% dplyr::filter(...)
  filtered_iso_files <-
    if (nrow(file_info) == 0) NULL
    else iso_files[names(iso_files) %in% file_info$file_id]
  
  # information
  if (!quiet) {
    str_interp("Info: applying file filter, keeping $[d]{n} of $[d]{n_all} files", 
               list(n = length(filtered_iso_files), n_all = length(iso_files))) %>% message()
  }
  
  return(filtered_iso_files)
}

#' @export
filter.iso_file <- function(.data, ...) {
  iso_filter_files(.data, ..., quiet = TRUE)
}

#' @export
filter.iso_file_list <- function(.data, ..., .preserve = FALSE) {
  iso_filter_files(.data, ..., quiet = TRUE)
}

# mutate ==================

#' Mutate file info
#' 
#' Mutate the file info (\code{\link{iso_get_file_info}}) within isofile objects by changing existing columns or introducing new ones. Works just like dplyr's \link[dplyr]{mutate}. You can also use \link[dplyr]{mutate} directly but it will not provide summary information on the operation. Note that this will create missing columns that exist in some but not all of the passed in isofile objects in all isofile objects (filling them with NAs) the same way that \code{\link{iso_get_file_info}} does.
#' 
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{mutate} conditions applied to the combined file info (see \code{\link{iso_get_file_info}})
#' @family file_info operations
#' @export 
iso_mutate_file_info <- function(iso_files, ..., quiet = default(quiet)) {
  UseMethod("iso_mutate_file_info")
} 

#' @export
iso_mutate_file_info.default <- function(iso_files, ..., quiet = default(quiet)) {
  stop("this function is not defined for objects of type '", 
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_mutate_file_info.iso_file <- function(iso_files, ..., quiet = default(quiet)) {
  iso_mutate_file_info(iso_as_file_list(iso_files), ..., quiet = quiet)[[1]]
}

#' @export
iso_mutate_file_info.iso_file_list <- function(iso_files, ..., quiet = default(quiet)) {
  
  # mutate iso_files' file info
  file_info <- 
    iso_get_file_info(iso_files, quiet = TRUE) %>% 
    dplyr::mutate(...) 
  
  # convert back to list format
  file_info <-
    file_info %>% 
    ensure_data_frame_list_columns() %>% 
    split(seq(nrow(file_info))) 
  
  # information
  if (!quiet) {
    glue::glue("Info: mutating file info for {length(iso_files)} data file(s)") %>% 
      message()
  }
  
  # mutate
  mutated_iso_files <-
    map2(iso_files, file_info, ~{ .x$file_info <- .y; .x }) %>% 
    iso_as_file_list()
  
  # return
  return(mutated_iso_files)
}

#' @export
mutate.iso_file <- function(.data, ...) {
  iso_mutate_file_info(.data, ..., quiet = TRUE)
}

#' @export
mutate.iso_file_list <- function(.data, ...) {
  iso_mutate_file_info(.data, ..., quiet = TRUE)
}