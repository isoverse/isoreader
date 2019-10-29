# dplyr style functions that operate on file info

# rename & select utils ========

# internal function for rename and select
select_rename_isofile <- function(isofile, func, quos) {
  
  # global vars
  to <- changed <- NULL
  
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
  isofile$file_info <- dplyr::select(isofile$file_info, new_vars)
  return(list(isofile = isofile, vars = vars))
}

# internal function to check for rename/select duplicates
check_names_changes <- function(vars) {
  
  # global vars
  to <- from <- NULL
  
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
  
  # global vars
  changed <- to <- from <- NULL
  
  # variables for all files
  select_quos <- quos(...)
  
  # select
  isofiles_select <- map(iso_files, select_rename_isofile, tidyselect::vars_select, select_quos)
  
  # info summary
  all_vars <- map2(names(isofiles_select), isofiles_select, ~mutate(.y$vars, file_id = .x)) %>% bind_rows()
  
  # check for duplicates
  check_names_changes(all_vars)
  
  # summary information
  if (!quiet) {
    
    info <- all_vars %>%
      group_by(file_id) %>%
      summarize(
        label = 
          ifelse(
            changed,
            sprintf("'%s'->'%s'", from, to),
            sprintf("'%s'", from)
          ) %>% paste(collapse = ", ")
      ) %>%
      dplyr::count(label) %>%
      mutate(label = sprintf(" - for %d file(s): %s", n, label)) %>%
      arrange(desc(n))

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
  # global vars
  changed <- to <- from <- NULL
  
  # variables for all files
  rename_quos <- quos(...)
  
  # rename
  isofiles_rename <- map(iso_files, select_rename_isofile, tidyselect::vars_rename, rename_quos)
  
  # info summary
  all_vars <- map2(names(isofiles_rename), isofiles_rename, ~mutate(.y$vars, file_id = .x)) %>% bind_rows()
  
  # check for duplicates
  check_names_changes(all_vars)
  
  # summary information
  if (!quiet) {
    
    info <- all_vars %>%
      filter(changed) %>% 
      group_by(file_id) %>%
      summarize(label = sprintf("'%s'->'%s'", from, to) %>% paste(collapse = ", ")) %>%
      dplyr::count(label) %>%
      mutate(label = sprintf(" - for %d file(s): %s", n, label)) %>%
      arrange(desc(n))
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
filter.iso_file <- function(.data, ..., .preserve = FALSE) {
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
  
  # information
  if (!quiet) {
    glue::glue("Info: mutating file info for {length(iso_files)} data file(s)") %>% 
      message()
  }
  
  # mutate iso_files' file info
  file_info <- 
    iso_get_file_info(iso_files, quiet = TRUE) %>% 
    dplyr::mutate(...) 
  
  # convert back to list format
  file_info <-
    file_info %>% 
    ensure_data_frame_list_columns() %>% 
    split(seq(nrow(file_info))) 
  
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

# parse ======

#' Parse file info
#' 
#' Convenience function to batch parse file info (\code{\link{iso_get_file_info}}) columns in isofile objects for the most common parsing calls. Uses the \code{parse_} functions exported from \link{readr} and described in \link{extract_data}. Note that for less common parsing calls or calls that require additional parameters to the parsing function, it is better to parse columns one-by-one using \code{\link{iso_mutate_file_info}} instead.
#' 
#' @inheritParams iso_get_raw_data
#' @param number dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a number using \link[readr]{parse_number}. Use \code{c(...)} to select multiple columns.
#' @param double dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a double using \link[readr]{parse_double}. Use \code{c(...)} to select multiple columns.
#' @param integer dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to an integer using \link[readr]{parse_integer}. Use \code{c(...)} to select multiple columns.
#' @param logical dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a boolean (TRUE/FALSE) using \link[readr]{parse_logical}. Use \code{c(...)} to select multiple columns.
#' @param datetime dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a date-time using \link[readr]{parse_datetime}. Use \code{c(...)} to select multiple columns.
#' @param text dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to text using \link[base]{as.character}. Use \code{c(...)} to select multiple columns.
#' @family file_info operations
#' @export 
iso_parse_file_info <- function(iso_files, number = c(), double = c(), integer = c(), logical = c(), datetime = c(), text = c(), quiet = default(quiet)) {
  UseMethod("iso_parse_file_info")
} 

#' @export
iso_parse_file_info.default <- function(iso_files, ...) {
  stop("this function is not defined for objects of type '", 
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_parse_file_info.iso_file <- function(iso_files, ...) {
  iso_parse_file_info(iso_as_file_list(iso_files), ...)[[1]]
}

#' @export
iso_parse_file_info.iso_file_list <- function(iso_files, number = c(), double = c(), integer = c(), logical = c(), datetime = c(), text = c(), quiet = default(quiet)) {
  
  # global
  column <- new_class <- old_class <- already_cast <- convert_to <- problem <- convert <- func <- NULL
  
  # get file info
  file_info <- iso_get_file_info(iso_files, quiet = TRUE) 
  
  # conversion classes
  classes <- 
    tribble(
      ~parse,     ~new_class,  ~func,
      "number",   "numeric",   "parse_number",
      "double",   "numeric",   "parse_double",
      "integer",  "integer",   "parse_integer",
      "logical",  "logical",   "parse_logical",
      "datetime", "POSIXct",   "parse_datetime",
      "text",     "character", "as.character"
    )
  
  # determine variables
  vars <-
    list(
      number = tidyselect::vars_select(names(file_info), !!enquo(number)),
      double = tidyselect::vars_select(names(file_info), !!enquo(double)),
      integer = tidyselect::vars_select(names(file_info), !!enquo(integer)),
      logical = tidyselect::vars_select(names(file_info), !!enquo(logical)),
      datetime = tidyselect::vars_select(names(file_info), !!enquo(datetime)),
      text = tidyselect::vars_select(names(file_info), !!enquo(text))
    ) %>% 
    tibble::enframe(name = "parse", value = "column") %>% 
    tidyr::unnest(column) %>% 
    # find out number of casts per column
    group_by(column) %>% mutate(n = n()) %>% ungroup() %>% 
    # get column info
    left_join(classes, by = "parse") %>% 
    mutate(
      old_class = map_chr(column, ~class(file_info[[.x]])[1]),
      already_cast = new_class == old_class,
      problem = !already_cast & new_class != "character" & old_class != "character"
    )
  
  # check on multi-casts
  if (any(vars$n > 1)) {
    probs <- 
      vars %>% filter(n > 1) %>% group_by(column) %>% 
      summarize(convert_to = paste(unique(parse), collapse = ", ")) %>% 
      mutate(label = sprintf(" - '%s' to %s", column, convert_to))
    glue::glue("cannot convert the same column(s) to multiple formats:\n",
               "{paste(probs$label, collapse = '\n')}") %>% 
      stop(call. = FALSE)
  }
  
  # information
  if (!quiet) {
    info <- 
      vars %>% filter(!problem, !already_cast) %>% group_by(parse) %>% 
      summarize(convert = paste(unique(column), collapse = "', '")) %>% 
      mutate(label = sprintf(" - to %s: '%s'", parse, convert))
    already <- filter(vars, already_cast)$column %>% 
      { if(length(.) > 0) 
          sprintf("\n - already the target data type (and thus ignored): '%s'", 
               paste(., collapse = "', '"))
        else ""
      }
    glue::glue(
      "Info: parsing {nrow(filter(vars, !problem, !already_cast))} ",
      "file info columns for {length(iso_files)} data file(s)",
      if (nrow(info) > 0) ":\n{paste(info$label, collapse = '\n')}" else "",
      "{already}") %>% 
      message()
  }
  
  # check on conversion problems
  if (any(vars$problem)) {
    probs <- 
      vars %>% filter(problem) %>% 
      mutate(label = 
               sprintf(" - cannot convert '%s' from %s to %s", 
                       column, old_class, parse))
    glue::glue(
      "missing automatic parsers for the following type conversions ",
      "(columns are ignored):\n{paste(probs$label, collapse = '\n')}") %>% 
      warning(immediate. = TRUE, call. = FALSE)
  }
  
  # cast
  mutate_quos <- 
    vars %>% filter(!problem, !already_cast) %>% 
    with(purrr::map2(column, func, ~quo((!!.y)(!!sym(.x)))) %>% setNames(column))
  
  # mutate file info
  file_info <- 
    file_info %>% 
    mutate(!!!mutate_quos) %>% 
    ensure_data_frame_list_columns() %>% 
    split(seq(nrow(file_info))) 
  
  # mutate
  mutated_iso_files <-
    map2(iso_files, file_info, ~{ .x$file_info <- .y; .x }) %>% 
    iso_as_file_list()
  
  # return
  return(mutated_iso_files)
}

# add info =======

#' Add additional file information
#'
#' This function makes it easy to add additional file info (\code{\link{iso_get_file_info}}) to isofile objects and data frames by a single \code{\link[dplyr]{left_join}} or multiple sequential \code{\link[dplyr]{left_join}} operations. The function provides a detailed summary of the information that was added unless \code{quiet = TRUE}. Note that one-to-many joins are not permitted (and will fail with an informative error) since this would lead to likely unintended data duplication in the isofiles. However, one-to-one and many-to-one joins are fully supported and should cover all needed use cases for this function. Also note that for each join, only the \code{new_file_info} rows that have defined non-NA, non-empty ("") values in all \code{join_by} columns will be considered for the join and that only \code{new_file_info} columns that do NOT already exist in ANY file information will be added. For changing the values of existing file information, please use \code{\link{iso_mutate_file_info}} instead.
#' 
#' Single \code{\link[dplyr]{left_join}}: this is the most common use of this function and basically a simple left join operation (with some additional safety checks). Specify a single \code{join_by} in the \code{...}, such as e.g. \code{c("file_id")} to add additional file information joining by the \code{file_id} column. 
#' 
#' Multiple sequential \code{\link[dplyr]{left_join}}: this use case is for applying a set of increasingly more specific \code{join_by} rules. For example, \code{... = c("Identifier 1", "Identifier 2"), c("file_id")} would serve to first add one set of new file information for all isofiles based on their \code{Identifier 1} and \code{Identifier 2} columns and then overwrite the new information with more specific details for a subset of isofiles based on their \code{file_id} column, all based on a single overview \code{new_file_info} data frame. Basically, each set of \code{join_by} conditions specified in \code{...} must describe a valid \code{\link[dplyr]{left_join}} \code{join_by} parameter to merge the \code{new_file_info} with the existing file info. Each set of \code{new_file_info} data can overwrite the previous \code{join_by} matches such that the last set of \code{join_by} column(s) provided in \code{...} will overwrite all previous matches for which it applies, even if they have already been a match for a previous column.
#' @rdname iso_add_file_info
#' @inheritParams iso_get_raw_data
#' @param new_file_info data frame with new file information to add to the isofiles
#' @param ... each parameter specifies a set of \code{join_by} column(s) to add the \code{new_file_info} to the existing file information. The provided paramters are applied sequentially. At least one must be specified.
#' @return the original iso files or data frame with the new file info added in.
#' @family file_info operations
#' @export
iso_add_file_info.iso_file_list <- function(iso_files, new_file_info, ..., quiet = default(quiet)) {

  # mutate iso_files' file info
  file_info <- 
    iso_get_file_info(iso_files, quiet = TRUE) %>% 
    iso_add_file_info(new_file_info = new_file_info, ..., quiet = quiet)
  
  # safety check
  if (!identical(names(iso_files), file_info$file_id)) {
    stop("file IDs of added file information does not match original file IDs, this should not be possible to happen and suggests there is a bug in the iso_add_file_info function, please report how this happened at https://github.com/isoverse/isoreader/issues", call. = FALSE)
  }
  
  # convert back to list format
  file_info <-
    file_info %>% 
    ensure_data_frame_list_columns() %>% 
    split(seq(nrow(file_info))) 
  
  # mutate
  updated_iso_files <-
    map2(iso_files, file_info, ~{ .x$file_info <- .y; .x }) %>% 
    iso_as_file_list()

  return(updated_iso_files)
}

#' @export
#' @rdname iso_add_file_info
#' @param df a data frame of iso files data retrieved by any of the data retrieval functions (e.g. \code{\link{iso_get_file_info}}, \code{\link{iso_get_raw_data}, etc.}
iso_add_file_info.data.frame <- function(df, new_file_info, ..., quiet = default(quiet)) {
  
  # global vars
  join_by_col <- ..priority <- new_data_idx <- data <- ..df_id <- n_ni_matches <- n_ni_considered <- n_df_matches <- ..ni_id <- n_ni_actual <- n_df_actual <- NULL
  
  # safety checks
  join_bys <- list(...)
  if (missing(new_file_info)) stop("no new_file_info supplied", call. = FALSE)
  if (length(join_bys) == 0) stop("must specify at least one set of join_by column(s) in ...", call. = FALSE)
  if (!"file_id" %in% names(df)) stop("file_id column must be part of the data frame", call. = FALSE)
  
  # new columns
  new_cols <- setdiff(names(new_file_info), names(df))
  
  # information
  n_data_files <- length(unique(df$file_id))
  if (!quiet) {
    glue::glue(
      "Info: adding new file information ('{paste(new_cols, collapse = \"', '\")}') ",
      "to {n_data_files} data file(s), ",
      "joining by '{purrr::map_chr(join_bys, paste, collapse = \"'+'\") %>% paste(collapse = \"' then '\")}'...") %>%
      message()
  }
  
  # additional safety checks
  if (length(new_cols) == 0) {
    glue::glue("no new information columns that don't already exist, returning data unchanged") %>% 
      warning(immediate. = TRUE, call. = FALSE)
    return(df)
  }
  missing_df <- setdiff(unique(unlist(join_bys)), names(df))
  missing_new_fi <- setdiff(unique(unlist(join_bys)), names(new_file_info))
  if (length(missing_df) > 0 || length(missing_new_fi) > 0) {
    glue::glue(
      "all join_by (...) columns must exist in both the existing file ",
      "information and the new_file_info",
      if(length(missing_df) > 0) "\n - missing in existing file info: '{paste(missing_df, collapse = \"', '\")}'" else "",
      if (length(missing_new_fi) > 0) "\n - missing in new file info: '{paste(missing_new_fi, collapse = \"', '\")}'" else "") %>% 
      stop(call. = FALSE)
  }
  
  # figure out which new file info columns that are in the join_bys have data in which rows
  join_by_cols <- 
    tibble(
      join_by_col = join_bys,
      ..priority = 1:length(join_by_col)
    )
  
  new_data_rows <-
    join_by_cols %>% 
    unnest(join_by_col) %>% 
    mutate(
      new_data_idx = map(join_by_col, ~which(!is.na(new_file_info[[.x]]) & nchar(as.character(new_file_info[[.x]])) > 0))
    ) %>% 
    group_by(..priority) %>% 
    summarize(new_data_idx = list(Reduce(intersect, new_data_idx))) %>% 
    right_join(join_by_cols, by = "..priority")
  
  # prep for joins
  shared_cols <- intersect(names(new_file_info), names(df)) %>% { setNames(., paste0("..ni_temp_", .)) }
  df <- mutate(df, ..df_id = dplyr::row_number())
  new_file_info <- mutate(new_file_info, ..ni_id = dplyr::row_number())
  
  # join new file info based on the join by and new row indices
  join_new_file_info <- function(join_by, new_rows, shared_cols) {
    if (length(join_by) > 0 && length(new_rows) > 0) {
      dplyr::inner_join(df, rename(new_file_info[new_rows, ], !!!shared_cols), by = join_by)
    } else { 
      tibble()
    }
  }
  
  # NOTE: the column overwrite leads to more confusing behaviour than probably worth it 
  # --> people should use iso_mutate_file_info instead for adding information to columns that already exist
  
  # # find new file info that can overwrite existing columns (because they are empty)
  # find_overwrite_cols <- function(data, shared_cols) {
  #   shared_cols[map_lgl(as.character(shared_cols), ~all(is.na(data[[.x]])))]
  # }
  # # cleanup new file info based on overwrite columns
  # cleanup_new_file_info <- function(data, overwrite_cols) {
  #   if (length(overwrite_cols) > 0) {
  #     data <- data %>%
  #       # FIXME: do this with a mutate to preserve column order
  #       select(!!!map(as.character(overwrite_cols), ~quo(-!!.x))) %>%
  #       rename(!!!setNames(names(overwrite_cols), as.character(overwrite_cols)))
  #   }
  #   return(select(data, -starts_with("..ni_temp_")))
  # }
  
  # generate joined data
  joined_data <- 
    new_data_rows %>% 
    mutate(
      n_ni_considered = map_int(new_data_idx, length),
      shared_cols = map(join_by_col, ~shared_cols[!shared_cols %in% .x]),
      data = purrr::pmap(list(join_by_col, new_data_idx, shared_cols), join_new_file_info),
      n_ni_matches = map_int(data, ~length(unique(.x$..ni_id))),
      n_df_matches = map_int(data, ~length(unique(.x$file_id)))
      #new_cols = purrr::map(overwrite_cols, ~names(new_file_info) %>% { .[. %in% c(new_cols, .x)] }),
      #overwrite_cols = purrr::map2(data, shared_cols, find_overwrite_cols),
      #data = purrr::map2(data, overwrite_cols, cleanup_new_file_info)
    )
  
  # select data based on priority
  final_data <- joined_data %>% select(..priority, data) %>% unnest(data) %>% 
    select(-starts_with("..ni_temp_")) %>% 
    group_by(..df_id) %>% 
    filter(..priority == max(..priority)) %>% 
    ungroup()
  
  # make sure all data is present (even those not matched by any join)
  final_data <- final_data %>% 
    bind_rows(filter(df, !..df_id %in% final_data$..df_id)) %>% 
    arrange(..df_id)
  
  # safety checks
  dup_data <- final_data %>% group_by(..df_id) %>% mutate(n = n()) %>% filter(n > 1L)
  if (nrow(dup_data) > 0) {
    error_data <- dup_data %>% 
      left_join(joined_data, by = "..priority") %>% 
      group_by(..priority) %>% 
      summarize(
        label = sprintf(
          "'%s' join: %d/%d new info rows match %d/%d data files but would lead to the duplication of %d data files.", 
          paste(join_by_col[[1]], collapse = "'+'"),
          n_ni_matches[1],
          n_ni_considered[1],
          n_df_matches[1],
          n_data_files,
          length(unique(..df_id))
        )
      )

    glue::glue(
      "join operation(s) would create duplicate entries:\n - ",
      "{paste(error_data$label, collapse = '\n - ')}") %>% 
    stop(call. = FALSE)
  }
  
  # info summary
  info_sum <- 
    final_data %>% group_by(..priority) %>% 
    summarize(
      n_ni_actual = length(unique(..ni_id)),
      n_df_actual = length(unique(file_id))
    ) %>%
    right_join(joined_data, by = "..priority") %>% 
    mutate(
      n_ni_actual = ifelse(is.na(n_ni_actual), 0L, n_ni_actual),
      n_df_actual = ifelse(is.na(n_df_actual), 0L, n_df_actual),
      label = sprintf(
        "'%s' join: %d/%d new info rows matched %d/%d data files%s", 
        purrr::map_chr(join_by_col, paste, collapse = "'+'"),
        n_ni_matches,
        n_ni_considered,
        n_df_matches,
        n_data_files,
        ifelse(n_ni_actual != n_ni_matches | n_df_actual != n_df_matches, 
               sprintf(" - %d of these was/were also matched by subsequent joins which took precedence", n_df_matches - n_df_actual),
               ""
        )
        # NOTE: the column overwrite leads to more confusing behaviour than probably worth it 
        # --> new columns should be universal and will be part of overall message
        # ifelse(n_df_actual > 0,
        #        sprintf(", columns added: '%s'", purrr::map_chr(new_cols, paste, collapse = "', '")),
        #        ""
        # )
      )
    )
  if (!quiet && nrow(info_sum) > 0) {
    message(" - ", paste(info_sum$label, collapse = "\n - "))
  }
  
  return(select(final_data, -..df_id, -..ni_id, -..priority))
}

# check doesn't work unless it's at the beginning
#' @rdname iso_add_file_info
#' @export
iso_add_file_info <- function(...) {
  UseMethod("iso_add_file_info")
}

#' @export
iso_add_file_info.default <- function(x, ...) {
  if (missing(x))
    stop("this function cannot be called without parameters", call. = FALSE)  
  stop("this function is not defined for objects of type '",
       class(x)[1], "'", call. = FALSE)
}

#' @export
iso_add_file_info.iso_file <- function(iso_files, ...) {
  iso_add_file_info(iso_as_file_list(iso_files), ...)[[1]]
}