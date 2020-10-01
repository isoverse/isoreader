# dplyr style functions that operate on file info

# select ==================

#' Select file info columns
#'
#' Select which file info columns (\code{\link{iso_get_file_info}}) to keep within isofile objects. Works just like dplyr's \link[dplyr]{select} and can rename columns on-the-fly. You can also use \link[dplyr]{select} directly but it will not provide summary information on the operation. To rename columns without removing all other information, use \link{iso_rename_file_info} instead. Set \code{file_specific = TRUE} to select different columns in different iso_files depending on what exists in each file. This is very useful when working with data from multiple instruments that may have the same information (e.g. sample name) stored in different columns.
#'
#' @inheritParams iso_get_raw_data
#' @param ... dplyr-style \link[dplyr]{select} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}}). Note that the \code{file_id} column will always be kept, no matter the selection criteria, and cannot be renamed to protect from unexpected behavior.
#' @param file_specific whether to run the select criteria (\code{...}) specifically within each individual file rather than on all files jointly. This is a lot slower but makes it possible to  select different columns in different iso_files depending on what exists in each file and is mostly of use when working with data from multiple instruments.
#' @family file_info operations
#' @export
iso_select_file_info <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {
  UseMethod("iso_select_file_info")
}

#' @export
iso_select_file_info.default <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {
  stop("this function is not defined for objects of type '",
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_select_file_info.iso_file <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {
  iso_select_file_info(iso_as_file_list(iso_files), ..., file_specific = FALSE, quiet = quiet)[[1]]
}

#' @export
iso_select_file_info.iso_file_list <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {

  # info message
  select_exps <- rlang::enexprs(...)
  select_exp <- rlang::expr(c(!!!select_exps))
  if (!quiet) {
    if (file_specific) {
      message( "Info: selecting/renaming the following file info:")
    } else{
      glue::glue(
        "Info: selecting/renaming the following file info across {length(iso_files)} data file(s): ",
        if (length(select_exps) == 0) "keeping only 'file_id'"
        else get_info_message_concat(select_exps, include_names = TRUE, names_sep = "->", flip_names_and_values = TRUE)
      ) %>% message()
    }
  }

  # perform selections
  if (file_specific) {

    # run select
    isofiles_select <- map(iso_files, function(isofile) {
      # get column names
      select_cols <-
        tryCatch(
          get_column_names(
            isofile$file_info, df_name = "file_info",
            select = select_exp, n_reqs = list(select = "*"),
            cols_must_exist = FALSE)$select,
          warning = function(w) {
            w$message
          })

      # check if there was an error
      error_msg <- NA_character_
      if (is.null(names(select_cols))) {
        error_msg <- select_cols
        select_cols <-  get_column_names(
          isofile$file_info, df_name = "file_info",
          select = select_exp, n_reqs = list(select = "*"),
          cols_must_exist = FALSE, warn = FALSE)$select
      }

      # make sure to include file_id
      if (!"file_id" %in% select_cols)
        select_cols <- c(c(file_id = "file_id"), select_cols)

      # selected variables
      vars <- tibble(
        file_id = isofile$file_info$file_id,
        from  = select_cols,
        to = names(select_cols),
        changed = .data$from != .data$to
      )

      # select file_info columns
      isofile$file_info <- dplyr::select(isofile$file_info, !!!select_cols)

      # check for file id
      if (!"file_id" %in% names(isofile$file_info)) {
        stop("renaming the 'file_id' column inside an isofile may lead to unpredictable behavior and is therefore not allowed, sorry", call. = FALSE)
      }

      #return both
      return(list(isofile = isofile, vars = vars, error = error_msg))
    })

    # get iso files
    updated_iso_files <- iso_as_file_list(map(isofiles_select, "isofile"))

    # summarize individual file updates
    if (!quiet) {
      info <- map(isofiles_select, "vars") %>%
        bind_rows() %>%
        group_by(.data$file_id) %>%
        summarize(
          label =
            ifelse(
              .data$changed,
              sprintf("'%s'->'%s'", .data$from, .data$to),
              sprintf("'%s'", .data$from)
            ) %>% paste(collapse = ", ")
        ) %>%
        dplyr::count(.data$label) %>%
        mutate(label = sprintf(" - for %d file(s): %s", .data$n, .data$label)) %>%
        arrange(desc(.data$n))
      message(paste(info$label, collapse = "\n"))
    }

    # check if same error for all files
    errors <- map_chr(isofiles_select, "error")
    if (!any(is.na(errors)) && all(errors == errors[1])) {
      warning(errors[[1]], immediate. = TRUE, call. = FALSE)
    }

  } else {
    # across all files  - fast but less flexible
    # retrieve info
    file_info <- iso_files %>%
      # retrieve file info
      map(~.x$file_info) %>%
      # combine in data frame (use safe bind to make sure different data column
      # types of the same name don't trip up the combination)
      safe_bind_rows()

    # check if there are any file_info
    if (nrow(file_info) > 0L) {

      # selecting columns
      select_cols <- get_column_names(
        file_info, select = select_exp,
        n_reqs = list(select = "*"),
        cols_must_exist = FALSE)$select

      if (!"file_id" %in% select_cols)
        select_cols <- c("file_id", select_cols) # file id always included

      # final processing
      file_info <-
        file_info %>%
        # focus on selected columns only (also takes care of the rename)
        dplyr::select(!!!select_cols)

      # check for file id
      if (!"file_id" %in% names(file_info)) {
        stop("renaming the 'file_id' column inside an isofile may lead to unpredictable behavior and is therefore not allowed, sorry", call. = FALSE)
      }

      # convert back to list format
      file_info <-
        file_info %>%
        # should still be list columns but doesn't hurt to check
        ensure_data_frame_list_columns() %>%
        # split by file info
        split(seq(nrow(file_info))) %>%
        # clean back out the columns that were only added through the row bind
        map(~.x[!map_lgl(.x, ~is.list(.x) && all(map_lgl(.x, is.null)))])

      # update
      updated_iso_files <-
        map2(iso_files, file_info, ~{ .x$file_info <- .y; .x }) %>%
        iso_as_file_list()

    } else {
      # no updates
      updated_iso_files <- iso_files
    }
  }

  # return updated iso files
  return(updated_iso_files)
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
#' Rename file info columns (\code{\link{iso_get_file_info}}) within isofile objects. Works just like dplyr's \link[dplyr]{rename}. You can also use \link[dplyr]{rename} directly but it will not provide summary information on the operation. To select specific columns to keep (discarding all others), use \link{iso_select_file_info} instead. Set \code{file_specific = TRUE} to rename different columns in different iso_files depending on what exists in each file. This is very useful when working with data from multiple instruments that may have the same information (e.g. sample name) stored in different columns.
#'
#' @inheritParams iso_select_file_info
#' @param ... dplyr-style \link[dplyr]{rename} conditions applied based on each file's file_info (see \code{\link{iso_get_file_info}})
#' @family file_info operations
#' @export
iso_rename_file_info <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {
  UseMethod("iso_rename_file_info")
}

#' @export
iso_rename_file_info.default <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {
  stop("this function is not defined for objects of type '",
       class(iso_files)[1], "'", call. = FALSE)
}

#' @export
iso_rename_file_info.iso_file <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {
  iso_rename_file_info(iso_as_file_list(iso_files), ..., file_specific = FALSE, quiet = quiet)[[1]]
}

#' @export
iso_rename_file_info.iso_file_list <- function(iso_files, ..., file_specific = FALSE, quiet = default(quiet)) {

  # info message
  rename_exps <- rlang::enexprs(...)
  rename_exp <- rlang::expr(c(!!!rename_exps))
  if (!quiet) {
    if (file_specific) {
      message( "Info: renaming the following file info:")
    } else{
      glue::glue(
        "Info: renaming the following file info across {length(iso_files)} data file(s): ",
        get_info_message_concat(rename_exps, include_names = TRUE, names_sep = "->", flip_names_and_values = TRUE)
      ) %>% message()
    }
  }

  # perform renames
  if (file_specific) {

    # run rename
    isofiles_rename <- map(iso_files, function(isofile) {
      # get column names
      rename_cols <-
        tryCatch(
          get_column_names(
            isofile$file_info, df_name = "file_info",
            rename = rename_exp, n_reqs = list(rename = "*"),
            cols_must_exist = FALSE)$rename,
          warning = function(w) {
            w$message
          })

      # check if there was an error
      error_msg <- NA_character_
      if (is.null(names(rename_cols))) {
        error_msg <- rename_cols
        rename_cols <- get_column_names(
          isofile$file_info, df_name = "file_info",
          rename = rename_exp, n_reqs = list(rename = "*"),
          cols_must_exist = FALSE, warn = FALSE)$rename
      }

      # rename variables
      vars <- tibble(
        file_id = isofile$file_info$file_id,
        from  = rename_cols,
        to = names(rename_cols),
        changed = .data$from != .data$to
      )

      # rename file_info columns
      if (length(rename_cols) > 0)
      isofile$file_info <- dplyr::rename(isofile$file_info, !!!rename_cols)

      # check for file id
      if (!"file_id" %in% names(isofile$file_info)) {
        stop("renaming the 'file_id' column inside an isofile may lead to unpredictable behavior and is therefore not allowed, sorry", call. = FALSE)
      }

      #return both
      return(list(isofile = isofile, vars = vars, error = error_msg))
    })

    # get iso files
    updated_iso_files <- iso_as_file_list(map(isofiles_rename, "isofile"))

    # summarize individual file updates
    if (!quiet) {
      info <- map(isofiles_rename, "vars") %>%
        bind_rows() %>%
        group_by(.data$file_id) %>%
        summarize(
          label =
            ifelse(
              .data$changed,
              sprintf("'%s'->'%s'", .data$from, .data$to),
              sprintf("'%s'", .data$from)
            ) %>% paste(collapse = ", ")
        ) %>%
        dplyr::count(.data$label) %>%
        mutate(label = sprintf(" - for %d file(s): %s", .data$n, .data$label)) %>%
        arrange(desc(.data$n))
      message(paste(info$label, collapse = "\n"))
    }

    # check if same error for all files
    errors <- map_chr(isofiles_rename, "error")
    if (!any(is.na(errors)) && all(errors == errors[1])) {
      warning(errors[[1]], immediate. = TRUE, call. = FALSE)
    }

  } else {
    # across all files  - fast but less flexible
    # retrieve info
    file_info <- iso_files %>%
      # retrieve file info
      map(~.x$file_info) %>%
      # combine in data frame (use safe bind to make sure different data column
      # types of the same name don't trip up the combination)
      safe_bind_rows()

    # renaming columns
    rename_cols <- get_column_names(
      file_info, df_name = "file_info",
      rename = rename_exp, n_reqs = list(rename = "*"),
      cols_must_exist = FALSE)$rename

    # then run the rename
    file_info <- dplyr::rename(file_info, !!!rename_cols)

    # check for file id
    if (!"file_id" %in% names(file_info)) {
      stop("renaming the 'file_id' column inside an isofile may lead to unpredictable behavior and is therefore not allowed, sorry", call. = FALSE)
    }

    # convert back to list format
    file_info <-
      file_info %>%
      # should still be list columns but doesn't hurt to check
      ensure_data_frame_list_columns() %>%
      # split by file info
      split(seq(nrow(file_info))) %>%
      # clean back out the columns that were only added through the row bind
      map(~.x[!map_lgl(.x, ~is.list(.x) && all(map_lgl(.x, is.null)))])

    # update
    updated_iso_files <-
      map2(iso_files, file_info, ~{ .x$file_info <- .y; .x }) %>%
      iso_as_file_list()
  }

  # return updated iso files
  return(updated_iso_files)
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

# file root =====

#' Set iso file directory root
#'
#' Sets the root directory for a set of iso_files (property \code{file_root} in the file information), which is particularly useful for re-reading files (\link{reread_iso_files}) after they have changed location. Can optionally remove the previous root (\code{remove_embedded_root}) if it is still embedded in the isofiles' \code{file_path} instead of \code{file_root}. Will warn about any paths that cannot be simplified by removing the embedded root.
#'
#' @inheritParams iso_get_raw_data
#' @param root new root directory for the isofiles. Can be relative to the current working directory (e.g. \code{"data"}) or an absolute path on the file system (e.g. \code{"/Users/..."} or \code{"C:/Data/.."}). Can be supplied as a vector of same length as the \code{iso_files} if the files have different roots. Use \code{root = "."} to set the root to the current working directory (the default).
#' @param remove_embedded_root set this parameter to a root path that is embedded in the isofiles' \code{file_path}. Will warn about any paths that cannot be simplified by removing the specified \code{remove_embedded_root}.
#' @family file_info operations
#' @export
iso_set_file_root <- function(iso_files, root = ".", remove_embedded_root = NULL, quiet = default(quiet)) {

  # safety check
  if (is.null(root) || is.na(root) || !length(root) %in% c(1L, length(iso_files))) {
    stop("must supply a value for the file root, either single value or a vector with the same length as iso_files", call. = FALSE)
  }
  if (!is.null(remove_embedded_root) && length(remove_embedded_root) != 1) {
    stop("only a single value can be provided to remove an embedded root. If you want to remove different embedded roots, split your iso_files using iso_filter_files() and then remove the embedded root in the subsets.", call. = FALSE)
  }

  # single vs. multiple iso files
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)

  # information
  if (!quiet) {
    glue::glue(
      "Info: setting file root for {length(iso_files)} data file(s)",
      if(length(root) == 1) {" to '{root}'"} else {""},
      if(!is.null(remove_embedded_root)) {" and removing embedded root '{remove_embedded_root}'"} else {""}) %>%
      message()
  }

  # remove embedded root
  if (!is.null(remove_embedded_root)) {
    embedded_root_simplified <- iso_shorten_relative_paths(remove_embedded_root)$path
    original_paths <- map_chr(iso_files, ~.x$file_info$file_path)
    paths <-
      original_paths %>%
      iso_root_paths(root = embedded_root_simplified, check_existence = FALSE) %>%
      mutate(original_path = !!original_paths)

    no_match_paths <- filter(paths, root != !!embedded_root_simplified)
    if (nrow(no_match_paths) > 0) {
      sprintf(
        "%d/%d file paths do not include the embedded root. The following paths could NOT be simplified:\n - %s",
        nrow(no_match_paths), nrow(paths),
        paste(no_match_paths$original_path, collapse = "\n - ")
      ) %>% warning(immediate. = TRUE, call. = FALSE)
    }

    # file info updates
    paths <- paths %>% mutate(
      path = ifelse(.data$root == !!embedded_root_simplified, .data$path, .data$original_path),
      root = !!root
    )
    file_info_update <- with(paths, map2(root, path, ~list(file_info = list(file_root = .x, file_path = .y))))
    names(file_info_update) <- names(iso_files)

  } else {
    # just the root update
    file_info_update <- map(names(iso_files), ~list(file_info = list(file_root = root)))
    names(file_info_update) <- names(iso_files)
  }

  # update
  iso_files <- as.list(iso_files) %>%
    modifyList(file_info_update) %>%
    iso_as_file_list()

  # return single (if passed in as single)
  if (single_file && length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}


# parse ======

#' Parse file info
#'
#' Convenience function to batch parse file info (\code{\link{iso_get_file_info}}) columns in isofile objects for the most common parsing calls. Uses the \code{parse_} functions exported from \link{readr} and described in \link{extract_data}. Note that for less common parsing calls or calls that require additional parameters to the parsing function, it is better to parse columns one-by-one using \code{\link{iso_mutate_file_info}} instead.
#'
#' @inheritParams iso_get_raw_data
#' @param number dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a number using \link[readr:parse_atomic]{parse_number}. Use \code{c(...)} to select multiple columns.
#' @param double dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a double using \link[readr:parse_atomic]{parse_double}. Use \code{c(...)} to select multiple columns.
#' @param integer dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to an integer using \link[readr:parse_atomic]{parse_integer}. Use \code{c(...)} to select multiple columns.
#' @param logical dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a boolean (TRUE/FALSE) using \link[readr:parse_atomic]{parse_logical}. Use \code{c(...)} to select multiple columns.
#' @param datetime dplyr-style \link[dplyr]{select} condition to choose columns that should be converted to a date-time using \link[readr:parse_atomic]{parse_datetime}. Use \code{c(...)} to select multiple columns.
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
      number =
        names(file_info)[tidyselect::eval_select(rlang::enexpr(number), file_info)],
      double =
        names(file_info)[tidyselect::eval_select(rlang::enexpr(double), file_info)],
      integer =
        names(file_info)[tidyselect::eval_select(rlang::enexpr(integer), file_info)],
      logical =
        names(file_info)[tidyselect::eval_select(rlang::enexpr(logical), file_info)],
      datetime =
        names(file_info)[tidyselect::eval_select(rlang::enexpr(datetime), file_info)],
      text =
        names(file_info)[tidyselect::eval_select(rlang::enexpr(text), file_info)]
    ) %>%
    tibble::enframe(name = "parse", value = "column") %>%
    tidyr::unnest(.data$column) %>%
    # find out number of casts per column
    group_by(.data$column) %>% mutate(n = n()) %>% ungroup() %>%
    # get column info
    left_join(classes, by = "parse") %>%
    mutate(
      old_class = map_chr(.data$column, ~class(file_info[[.x]])[1]),
      already_cast = .data$new_class == .data$old_class,
      problem = !.data$already_cast & .data$new_class != "character" & .data$old_class != "character"
    )

  # check on multi-casts
  if (any(vars$n > 1)) {
    probs <-
      vars %>% filter(.data$n > 1) %>% group_by(.data$column) %>%
      summarize(convert_to = paste(unique(.data$parse), collapse = ", ")) %>%
      mutate(label = sprintf(" - '%s' to %s", .data$column, .data$convert_to))
    glue::glue("cannot convert the same column(s) to multiple formats:\n",
               "{paste(probs$label, collapse = '\n')}") %>%
      stop(call. = FALSE)
  }

  # information
  if (!quiet) {
    info <-
      vars %>% filter(!.data$problem, !.data$already_cast) %>%
      group_by(.data$parse) %>%
      summarize(convert = paste(unique(.data$column), collapse = "', '")) %>%
      mutate(label = sprintf(" - to %s: '%s'", .data$parse, .data$convert))
    already <- filter(vars, .data$already_cast)$column %>%
      { if(length(.) > 0)
          sprintf("\n - already the target data type (and thus ignored): '%s'",
               paste(., collapse = "', '"))
        else ""
      }
    glue::glue(
      "Info: parsing {nrow(filter(vars, !.data$problem, !.data$already_cast))} ",
      "file info columns for {length(iso_files)} data file(s)",
      if (nrow(info) > 0) ":\n{paste(info$label, collapse = '\n')}" else "",
      "{already}") %>%
      message()
  }

  # check on conversion problems
  if (any(vars$problem)) {
    probs <-
      vars %>% filter(.data$problem) %>%
      mutate(label =
               sprintf(" - cannot convert '%s' from %s to %s",
                       .data$column, .data$old_class, .data$parse))
    glue::glue(
      "missing automatic parsers for the following type conversions ",
      "(columns are ignored):\n{paste(probs$label, collapse = '\n')}") %>%
      warning(immediate. = TRUE, call. = FALSE)
  }

  # cast
  mutate_quos <-
    vars %>% filter(!.data$problem, !.data$already_cast) %>%
    # note for RMD check, since this is a with statement, does not take .data!
    {
      rlang::set_names(
        purrr::map2(.$column, .$func, ~quo((!!.y)(!!sym(.x)))),
        .$column
      )
    }

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
#' This function makes it easy to add additional file info (\code{\link{iso_get_file_info}}) to isofile objects and data frames by a single \code{\link[dplyr:mutate-joins]{left_join}} or multiple sequential \code{\link[dplyr:mutate-joins]{left_join}} operations. The function provides a detailed summary of the information that was added unless \code{quiet = TRUE}. Note that one-to-many joins are not permitted (and will fail with an informative error) since this would lead to likely unintended data duplication in the isofiles. However, one-to-one and many-to-one joins are fully supported and should cover all needed use cases for this function. Also note that for each join, only the \code{new_file_info} rows that have defined non-NA, non-empty ("") values in all \code{join_by} columns will be considered for the join and that only \code{new_file_info} columns that do NOT already exist in ANY file information will be added. For changing the values of existing file information, please use \code{\link{iso_mutate_file_info}} instead.
#'
#' Single \code{\link[dplyr:mutate-joins]{left_join}}: this is the most common use of this function and basically a simple left join operation (with some additional safety checks). Specify a single \code{join_by} in the \code{...}, such as e.g. \code{c("file_id")} to add additional file information joining by the \code{file_id} column.
#'
#' Multiple sequential \code{\link[dplyr:mutate-joins]{left_join}}: this use case is for applying a set of increasingly more specific \code{join_by} rules. For example, \code{... = c("Identifier 1", "Identifier 2"), c("file_id")} would serve to first add one set of new file information for all isofiles based on their \code{Identifier 1} and \code{Identifier 2} columns and then overwrite the new information with more specific details for a subset of isofiles based on their \code{file_id} column, all based on a single overview \code{new_file_info} data frame. Basically, each set of \code{join_by} conditions specified in \code{...} must describe a valid \code{\link[dplyr:mutate-joins]{left_join}} \code{join_by} parameter to merge the \code{new_file_info} with the existing file info. Each set of \code{new_file_info} data can overwrite the previous \code{join_by} matches such that the last set of \code{join_by} column(s) provided in \code{...} will overwrite all previous matches for which it applies, even if they have already been a match for a previous column.
#' @rdname iso_add_file_info
#' @inheritParams iso_get_raw_data
#' @param new_file_info data frame with new file information to add to the isofiles
#' @param ... each parameter specifies a set of \code{join_by} column(s) to add the \code{new_file_info} to the existing file information. The provided parameters are applied sequentially. At least one must be specified.
#' @return the original iso files or data frame with the new file info added in.
#' @family file_info operations
#' @export
iso_add_file_info.iso_file_list <- function(iso_files, new_file_info, ..., quiet = default(quiet)) {

  # add to iso_files' file_info
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
      ..priority = 1:length(.data$join_by_col)
    )

  new_data_rows <-
    join_by_cols %>%
    unnest(.data$join_by_col) %>%
    mutate(
      new_data_idx = map(.data$join_by_col, ~which(!is.na(new_file_info[[.x]]) & nchar(as.character(new_file_info[[.x]])) > 0))
    ) %>%
    group_by(.data$..priority) %>%
    summarize(new_data_idx = list(Reduce(intersect, .data$new_data_idx))) %>%
    right_join(join_by_cols, by = "..priority")

  # prep for joins
  shared_cols <- intersect(names(new_file_info), names(df)) %>% { rlang::set_names(., paste0("..ni_temp_", .)) }
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

  # generate joined data
  joined_data <-
    new_data_rows %>%
    mutate(
      n_ni_considered = map_int(.data$new_data_idx, length),
      shared_cols = map(.data$join_by_col, ~shared_cols[!shared_cols %in% .x]),
      data = purrr::pmap(list(.data$join_by_col, .data$new_data_idx, .data$shared_cols), join_new_file_info),
      n_ni_matches = map_int(.data$data, ~length(unique(.x$..ni_id))),
      n_df_matches = map_int(.data$data, ~length(unique(.x$file_id)))
    )

  # select data based on priority
  final_data <-
    joined_data %>%
    select(.data$..priority, .data$data) %>%
    # avoid problems with the temp columns during unnest
    mutate(data = map(.data$data, ~select(.x, -starts_with("..ni_temp_")))) %>%
    unnest(.data$data) %>%
    select(-starts_with("..ni_temp_")) %>%
    group_by(.data$..df_id) %>%
    filter(.data$..priority == max(.data$..priority)) %>%
    ungroup()

  # make sure all data is present (even those not matched by any join)
  final_data <- final_data %>%
    vctrs::vec_rbind(filter(df, !.data$..df_id %in% final_data$..df_id)) %>%
    arrange(.data$..df_id)

  # safety checks
  dup_data <- final_data %>% group_by(.data$..df_id) %>% mutate(n = n()) %>% filter(.data$n > 1L)
  if (nrow(dup_data) > 0) {
    error_data <- dup_data %>%
      left_join(joined_data, by = "..priority") %>%
      group_by(.data$..priority) %>%
      summarize(
        label = sprintf(
          "'%s' join: %d/%d new info rows match %d/%d data files but would lead to the duplication of %d data files.",
          paste(.data$join_by_col[[1]], collapse = "'+'"),
          .data$n_ni_matches[1],
          .data$n_ni_considered[1],
          .data$n_df_matches[1],
          n_data_files,
          length(unique(.data$..df_id))
        )
      )

    glue::glue(
      "join operation(s) would create duplicate entries:\n - ",
      "{paste(error_data$label, collapse = '\n - ')}") %>%
    stop(call. = FALSE)
  }

  # info summary
  info_sum <-
    final_data %>% group_by(.data$..priority) %>%
    summarize(
      n_ni_actual = length(unique(.data$..ni_id)),
      n_df_actual = length(unique(.data$file_id))
    ) %>%
    right_join(joined_data, by = "..priority") %>%
    mutate(
      n_ni_actual = ifelse(is.na(.data$n_ni_actual), 0L, .data$n_ni_actual),
      n_df_actual = ifelse(is.na(.data$n_df_actual), 0L, .data$n_df_actual),
      label = sprintf(
        "'%s' join: %d/%d new info rows matched %d/%d data files%s",
        purrr::map_chr(.data$join_by_col, paste, collapse = "'+'"),
        .data$n_ni_matches,
        .data$n_ni_considered,
        .data$n_df_matches,
        n_data_files,
        ifelse(.data$n_ni_actual != .data$n_ni_matches | .data$n_df_actual != .data$n_df_matches,
               sprintf(" - %d of these was/were also matched by subsequent joins which took precedence",
                       .data$n_df_matches - .data$n_df_actual),
               ""
        )
        # NOTE: the column overwrite leads to more confusing behavior than probably worth it
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

  return(select(final_data, -.data$..df_id, -.data$..ni_id, -.data$..priority))
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
