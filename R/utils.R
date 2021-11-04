# general helper functions ===========

# check if a column is in a data frame
col_in_df <- function(df, col) {
  df_expr <- rlang::enexpr(df)
  if (!is.data.frame(df)) {
    stop(rlang::quo_text(df_expr), " is not a data frame but ", class(df)[1], call. = FALSE)
  }
  col %in% names(df)
}

# collapse helper to deal with naming change in the glue package
collapse <- function(...) {
  glue::glue_collapse(...)
}

# helper to make sure columns exist
# NOTE: is this used?
col_check <- function(cols, data, fun = sys.call(-1), msg = "You may have to change the parameters in your function call") {
  if (!is.null(cols) && length(missing <- setdiff(cols, names(data))) > 0)
    stop("column(s) not in data: '", str_c(missing, collapse = "', '"),
         "'. ", msg, ". Function: ", fun, call. = FALSE)
}

# messages/warnings/progress =======

# helper function for showing a message via progress bar or logging it in log file (parallel)
log_message <- function(..., type = "info", prefix = "Info: ", quiet = default(quiet)) {
  if (!quiet) {
    msg <- purrr::map_chr(list(...), ~paste(format(.x), collapse = "\n")) %>% paste(collapse = "")
    pb <- get_temp("progress_bar", allow_null = TRUE)
    process <- get_temp("parallel_process", allow_null = FALSE)
    if (!is.na(process)) {
      # save to log file
      log_file <- get_temp("parallel_log_file")
      if (!is.null(log_file)) {
        sprintf("\"%s\",%d,\"%s\"\n", type, process,
                str_replace_all(msg, fixed("\""), "\\\"")) %>%
          cat(file = log_file, append = TRUE)
      }
    } else if (!is.null(pb) && !pb$finished) {
      # progress bar
      pb$message(paste0(prefix, msg))
    } else {
      # regular message
      message(paste0(prefix, msg))
    }
  }
}

# helper function for showing a warning via progress bar or logging it in log file (parallel)
log_warning <- function(..., type = "warning", prefix = "Warning: ") {
  # warnings are never quiet
  log_message(..., type = type, prefix = prefix, quiet = FALSE)
}

# log progress on the progress bar or log in progress file (parallel)
log_progress <- function(n = 1L) {
  process <- get_temp("parallel_process", allow_null = FALSE)
  pb <- get_temp("progress_bar", allow_null = TRUE)
  if (!is.na(process)) {
    # save to log file
    log_file <- get_temp("parallel_progress_file")
    if (!is.null(log_file)) {
      cat(rep(" ", n), file = log_file, append = TRUE)
    }
  } else if(!is.null(pb) && !pb$finished) {
    # advance progress
    pb$tick(n)
  }
}

# paralell ========

# setup log files
setup_parallel_logs <- function() {
  tmpfile <- tempfile()

  log <- paste0(tmpfile, ".log")
  cat("", file = log)
  set_temp("parallel_log_file", log)

  progress <- paste0(tmpfile, ".progress")
  cat("", file = progress)
  set_temp("parallel_progress_file", progress)

  if (default(debug)) {
    glue::glue("\n\nDEBUG  (log files will not be deleted afer run):\n\t",
               "log file path '{log}'\n\tprogress file path '{progress}'") %>%
      message()
  }
}

# monitor parallel log files
monitor_parallel_logs <- function(processes) {
  pb <- get_temp("progress_bar", allow_null = TRUE)
  status <- list(log_n = 0L, progress = 0L)
  # check on processes and update progress + user info
  while (TRUE) {
    # update status
    status <- process_parallel_logs(status)

    # processors report
    futures_finished <- purrr::map_lgl(processes$result, future::resolved)

    # done?
    if (all(futures_finished)) break
  }

  # finall call to wrap up logs
  process_parallel_logs(status)
}

# process parallel logs
process_parallel_logs <- function(status) {

  # logs
  log <- get_temp("parallel_log_file")
  if (!is.null(log) && file.exists(log)) {

    # try to read logs
    reset <-
      tryCatch(
        {
          if (default(debug))
            logs <- suppressMessages(readr::read_csv(log, col_names = FALSE, skip = status$log_n))
          else
            logs <- suppressWarnings(suppressMessages(readr::read_csv(log, col_names = FALSE, skip = status$log_n)))
          if (nrow(logs) > 0 && ncol(logs) != 3) stop("incorrect log file format", call. = FALSE)
          NULL # set reset to NULL if it gets to here
        },
        error = function(e) e$message # csv read error
      )

    if (!is.null(reset)) {
      # safety precaution in case log file gets corrupted
      log_message("resetting log file (some progress updates may not display) because of error - '",
                  reset, "'. This can happen sometimes when too many parallel processes finish at the ",
                  "exact same time but should only affect the logs, not the file reads themselves.", prefix = "Warning: ")
      cat("", file = log)
      status$log_n <- 0L
    } else if (nrow(logs) > 0) {
      # display logs
      status$log_n <- status$log_n + nrow(logs)
      logs %>%
        mutate(
          X2 = as.character(.data$X2),
          prefix = case_when(
            X1 == "info" ~ sprintf("Info (process %s): ", .data$X2),
            X1 == "warning" ~ sprintf("Warning (process %s): ", .data$X2),
            TRUE ~ sprintf("Process %s: ", .data$X2)
          )) %>%
        { purrr::walk2(.$X3, .$prefix, ~log_message(.x, prefix = .y)) }
    }
  }

  # finished files
  progress <- get_temp("parallel_progress_file")
  if (!is.null(progress) && file.exists(progress)) {
    progress <- file.size(progress)
    if (progress > status$progress) {
      log_progress(progress - status$progress)
      status$progress <- progress
    }
  }

  return(status)
}

# cleanup parallel logs
cleanup_parallel_logs <- function() {
  log <- get_temp("parallel_log_file")
  if (!is.null(log) && file.exists(log) && !default(debug)) file.remove(log)
  progress <- get_temp("parallel_progress_file")
  if (!is.null(progress) && file.exists(progress) && !default(debug)) file.remove(progress)
}

# example files ====

#' Example files
#'
#' @description The isoreader package comes with a few example files to make it easy to illustrate the functionality.
#'
#' @details \code{iso_get_reader_example}: retrieve the path to an isoreader example file
#' @param filename the name of the example file for which to retrieve the system path
#' @export
iso_get_reader_example <- function(filename) {
  filepath <- system.file(package = "isoreader", "extdata", filename)
  if(!file.exists(filepath))
    sprintf("The example file '%s' does not exist. Please use iso_get_reader_examples() to see a list of all available example files.", filename) %>%
    stop(call. = FALSE)
  return(filepath)
}

#' @rdname iso_get_reader_example
#' @details \code{iso_get_reader_examples}: list of all available isoreader example files
#' @examples
#' iso_get_reader_examples()
#' @export
iso_get_reader_examples <- function() {
  file_types <- iso_get_supported_file_types()
  iso_expand_paths(
      ".", extensions = file_types$extension, root = system.file(package = "isoreader", "extdata")) %>%
    mutate(filename = basename(.data$path)) %>%
    match_to_supported_file_types(file_types) %>%
    arrange(.data$type, .data$extension, .data$filename) %>%
    select(.data$filename, .data$type, .data$software, .data$description)
}

#' @rdname iso_get_reader_example
#' @details \code{iso_get_reader_examples_folder}: path to the location of the reader examples
#' @examples
#' iso_get_reader_examples_folder()
#' @export
iso_get_reader_examples_folder <- function() {
  return(system.file(package = "isoreader", "extdata"))
}

# file paths ====

# convenience function to check if something is a folder (even if it doesn't exist)
# @param path(s)
is_folder <- function(path, check_existence = TRUE) {
  # safety check
  if(check_existence && !all(exists <- file.exists(path)))
    stop("paths do not exist:\n - ", str_c(path[!exists], collapse = "\n - "), call. = FALSE)

  # it's a folder if it exists
  check <- dir.exists(path) |
    # or if we're not checking existence and it does not exist but it has a . in the name
    ( !check_existence & !file.exists(path) & !str_detect(basename(path), fixed(".")) )

  return(check)
}

# convenience function to generate a paths data frame
# @param path path(s) (relative or absolute)
# @param root (root(s) for relative paths)
get_paths_data_frame <- function(path, root, check_existence = TRUE) {

  # global vars
  full_path <- absolute <- NULL

  # error with dimensions
  if (length(path) != 1 && length(root) != 1 && length(path) != length(root)) {
    stop("paths and roots need to have one entry or be of the same length, not ",
         length(path), " and ", length(root), call. = FALSE)
  }

  # path safety check
  if (any(path == "")) {
    stop("empty paths (\"\") are not valid, please use \".\" to refer to the current directory", call. = FALSE)
  }

  # paths data frame
  paths <-
    tibble(
      i = 1:max(length(root), length(path)),
      root = root,
      path = path,
      absolute = R.utils::isAbsolutePath(path),
      full_path = ifelse(absolute, path, file.path(root, path)),
      exists = file.exists(full_path),
      is_dir = is_folder(full_path, check_existence = !!check_existence)
    )

  # safety check
  if (check_existence && !all(paths$exists)) {
    stop("path does not exist: \n\t",
         paste(filter(paths, !exists)$path, collapse = "\n\t"),
         call. = FALSE)
  }

  return(paths)
}

# find out which vectors have the common start
# @param vectors list of vectors
# @param common single vector to check for
has_common_start <- function(vectors, common) {
  common_length <- length(common)
  vector_lengths <- map_int(vectors, length)
  is_common <- rep(TRUE, length(vectors))

  # rule out those that are too short
  is_common [vector_lengths < common_length] <- FALSE

  # check for others whether they are identical
  is_common[is_common] <- map_lgl(
    vectors[is_common],
    ~identical(.x[1:common_length], common)
  )

  # return
  return(is_common)
}

# find the common elements from the start of the vectors
# @param vectors list of vectors
find_common_different_from_start <- function(vectors, empty = character(0)) {

  # global vars
  i <- entry <- same <- v <- data <- result <- NULL

  min_length <- min(map_int(vectors, length))
  if(min_length == 0) {
    return(list(common = empty, different = vectors))
  }

  # all path vectors
  vectors <-
    map2(
      1:length(vectors), vectors,
      ~tibble(v = .x, i = 1:length(.y), entry = .y)
    ) %>%
    bind_rows()

  # common segments
  commons <- vectors %>%
    filter(i <= min_length) %>%
    group_by(i) %>%
    summarize(same = all(entry == entry[1])) %>%
    arrange(i) %>%
    mutate(diff = cumsum(abs(c(same[1] == FALSE,diff(!same))))) %>%
    filter(diff == 0)

  # common vector
  common <- filter(vectors, v==1)$entry[commons$i]
  if (length(common) == 0) common <- empty

  # differences vector
  different <-
    filter(vectors, !i %in% commons$i) %>%
    select(v, entry) %>%
    nest(data = c(-v)) %>%
    full_join(tibble(
      v = unique(vectors$v),
      empty = list(entry = empty)), by = "v") %>%
    mutate(
      missing = map_lgl(data, is.null),
      data = map2(missing, data, ~if(.x) { NULL } else { .y$entry }),
      result = ifelse(missing, empty, data)
    ) %>%
    select(v, result) %>%
    arrange(v) %>%
    tibble::deframe() %>%
    unname()

  return(
    list(
      common = common,
      different = different
    )
  )
}

# helper function to get vector of path segments
# omits segments that are just the current folder (.)
get_path_segments <- function(path) {
  if (is.na(path)) return(path)
  segments <- c()
  while(TRUE) {
    segments <- c(basename(path), segments)
    parent <- dirname(path)
    if (parent == path) break;
    path <- parent
  }
  # ignore without . since it does not change path
  return(segments[segments != "."])
}

# unlist paths
unlist_paths <- function(path_list) {
  if (!all(ok <- purrr::map_lgl(path_list, is.character))) {
    not_ok <- path_list[!ok] %>% purrr::map_chr(~class(.x)[1])
    stop("paths must be character vectors, encountered: ", paste(not_ok, collapse = ", "), call. = FALSE)
  }
  unlist(path_list, use.names = FALSE)
}

#' Expand file paths
#'
#' Helper function to expand the provided paths to find data files in folders and subfolders that match any of the specified extensions. Filepaths will be kept as is, only folders will be expanded. Note that this function is rarely called directly. It is used automatically by \code{\link{iso_read_dual_inlet}} and \code{\link{iso_read_continuous_flow}} to identify files of interest based on the file paths provided.
#'
#' @param path vector of file/folder paths, mixed relative and absolute paths are allowed.
#' @param extensions which extensions to look for? (with or without leading .) - this is typically one or more of the extensions listed by \code{\link{iso_get_supported_file_types}}
#' @param root root directory for the isofiles. Can be relative to the current working directory (e.g. \code{"data"}) or an absolute path on the file system (e.g. \code{"/Users/..."} or \code{"C:/Data/.."}). The default is the current working directory (\code{"."}). Can be supplied as a vector of same length as the provided paths if the paths have different roots.
#' @return data frame with columns \code{root} (\code{root} as provided) and \code{path} of all the found files.
#' @family file system functions
#' @export
iso_expand_paths <- function(path, extensions = c(), root = ".") {

  # global vars
  full_path <- is_dir <- i <- NULL

  # file paths
  if (!is.character(path)) {
    stop("file paths need to be character vectors, not class ", class(path)[1], call. = FALSE)
  }
  paths <- get_paths_data_frame(path, root, check_existence = TRUE)

  # extensions check
  if(length(extensions) == 0) stop("no extensions provided for retrieving file paths", call. = FALSE)
  pattern <- extensions %>% str_replace_all("\\.", "\\\\.") %>% str_c(collapse = "|") %>% { str_c("(", ., ")$") }
  paths <- paths %>%
    mutate(
      is_dir = dir.exists(full_path),
      has_ext = ifelse(is_dir, TRUE, str_detect(basename(full_path), pattern))
    )
  if (!all(paths$has_ext)) {
    stop("some file(s) do not have one of the supported extensions (",
         str_c(extensions, collapse = ", "),
         "):\n\t", with(paths, path[!has_ext]) %>% str_c(collapse = "\n\t"), call. = FALSE)
  }

  # retrieve all the files
  filepaths <-
    paths %>%
    filter(is_dir) %>%
    mutate(file = map(full_path, ~list.files(.x, pattern = pattern, recursive = TRUE, include.dirs = FALSE))) %>%
    unnest(file)

  if (nrow(filepaths) > 0)
    filepaths <- mutate(filepaths, path = file.path(path, file))

  # simplify
  paths <-
    bind_rows(
      filter(paths, !is_dir),
      select(filepaths, i, root, path)
    ) %>%
    arrange(i) %>%
    select(root, path) %>%
    unique() # make sure all unique files

  # double check that filenames are unique
  filenames <- basename(paths$path)
  if (anyDuplicated(filenames)) {
    dups <- duplicated(filenames) | duplicated(filenames, fromLast = T)
    warning("some files from different folders have identical file names:\n\t",
            paths$path[dups] %>% str_c(collapse = "\n\t"), immediate. = TRUE, call. = FALSE)
  }

  return(paths)
}


#' Root paths
#'
#' Function to root both relative and absolute paths to a root directory (or directories) commonly relative to current working directory. Determines the best way to shorten relative paths and put absolute paths in a relative context (if possible) using \link{iso_shorten_relative_paths} and \link{iso_find_absolute_path_roots}, respectively.
#'
#' @inheritParams iso_find_absolute_path_roots
#' @return a data frame with the root directories and paths relative to the root - order of input paths is preserved
#' @family file system functions
#' @export
iso_root_paths <- function(path, root = ".", check_existence = TRUE) {

  paths <- iso_shorten_relative_paths(path, root)
  paths <- iso_find_absolute_path_roots(paths$path, paths$root, check_existence = check_existence)

  return(paths)
}

#' Shorten relative paths
#'
#' Convenience function to shorten relative paths based on overlap with the provided root(s). Also simplifies current directory repeats (e.g. "././." becomes ".") for better legibility. Does not check whether the original or resulting paths point to valid files or folders. Relative paths that do not start with the supplied \code{root} default back to the current working directory (\code{.}). Absolute paths are allowed but are returned as is without attempts at shortening. See \code{iso_find_absolute_path_roots} for rooting absolute paths.
#'
#' @inheritParams iso_expand_paths
#' @return a data frame with the root directories and paths relative to the root - order of input paths is preserved
#' @family file system functions
#' @export
#' @examples
#' iso_shorten_relative_paths(file.path("A", "B", "C"), "A") # root = "A", path = B/C
#' iso_shorten_relative_paths(file.path("A", "B", "C"), file.path("A", "B")) # root = "A/B", path = "C"
#' iso_shorten_relative_paths(file.path("A", "C", "D"), file.path("A", "B")) # root = "A", path = "C/D"
#' iso_shorten_relative_paths(file.path("A", "B", "C"), "B") # root = ".", path stays "A/B/C"
iso_shorten_relative_paths <- function(path, root = ".") {

  # global
  root_folders_all <- root_folders_rel <- absolute <- root_folders <- path_folders <- i <- NULL

  # error with dimensions
  if (length(path) != 1 && length(root) != 1 && length(path) != length(root)) {
    stop("paths and roots need to have one entry or be of the same length, not ",
         length(path), " and ", length(root), call. = FALSE)
  }

  # relative base reference
  wd_folders <- get_path_segments(getwd())

  # generate paths dataframe (WITHOUT concatenating path and root ulnike get_paths_data_frame)
  paths <-
    tibble(
      i = 1:max(length(root), length(path)),
      path = path,
      root = root,
      absolute = R.utils::isAbsolutePath(path),
      path_folders = map(path, get_path_segments)
    ) %>%
    # put roots into working directory context if possible
    group_by(root) %>%
    mutate(
      root_folders_all = map(root[1], get_path_segments),
      root_folders_rel = find_common_different_from_start(c(list(wd_folders), root_folders_all[1]))$different[-1],
      root_folders = if (has_common_start(root_folders_all[1], wd_folders)) root_folders_rel else root_folders_all
    ) %>%
    ungroup()

  # shorten relative paths
  rel_paths <- paths %>% filter(!absolute)
  if (nrow(rel_paths) > 0) {
    rel_paths <- rel_paths %>%
      # shorten with most possible overlap
      group_by(root, path) %>%
      mutate(
        root_folders = list(find_common_different_from_start(c(root_folders[1], path_folders[1]))$common),
        path_folders = find_common_different_from_start(c(root_folders[1], path_folders[1]))$different[-1]
      ) %>%
      ungroup() %>%
      # assmple paths
      mutate(
        path = path_folders %>% map_chr(
          ~if(length(.x) == 0) { "." } else { do.call(file.path, args = as.list(.x))})
      )
  }

  # return all
  paths <- bind_rows(rel_paths, filter(paths, absolute)) %>% arrange(i) %>%
    # simplify root path
    mutate(root = root_folders %>% map_chr(~if(length(.x) == 0) { "." } else { do.call(file.path, args = as.list(.x))}))
  return(select(paths, root, path))
}

#' Find roots for absolute paths
#'
#' Helper function to find the roots of absolute paths. Tries to put absolute paths into the context of the relative root. For those that this is not possible (because they are not in fact a sub-path of the relative roots), identifies the greatest common denominator for absolute paths as their root. Does not change relative paths but does check whether they do exist if \code{check_existence = TRUE} (the default). To modify relative paths, use \link{iso_shorten_relative_paths} prior to calling this function.
#' @inheritParams iso_expand_paths
#' @param check_existence whether to check for the existence of the paths
#' @return a data frame with the root directories and paths relative to the root - order of input paths is preserved
#' @family file system functions
#' @export
iso_find_absolute_path_roots <- function(path, root = ".", check_existence = TRUE) {

  # global vars
  absolute <- is_dir <- full_path <- rel_root_folders <- path_folders <- abs_root_folders <- has_rel_root <- new_path <- i <- NULL

  # anything to work with?
  if(length(path) == 0) return(tibble(root = character(0), path = character(0)))

  # generate data frame and check existence
  paths <- get_paths_data_frame(path, root, check_existence = check_existence)

  # process absolute paths
  abs_paths <- paths %>% filter(absolute)
  if (nrow(abs_paths) > 0) {

    # determine root folders
    abs_paths <- abs_paths %>%
      # get path folders
      mutate(path_folders = ifelse(is_dir, full_path, dirname(full_path)) %>% map(get_path_segments)) %>%
      # get root folders
      group_by(root) %>%
      mutate(
        rel_root_folders = map(root, get_path_segments),
        abs_root_folders = map2(
          root, rel_root_folders,
          ~if(R.utils::isAbsolutePath(.x)) { .y } else { get_path_segments(file.path(getwd(), .x)) }
        ),
        has_rel_root = has_common_start(path_folders, abs_root_folders[[1]])
      ) %>%
      ungroup()

    # absolute paths that share relative root
    abs_rel_paths <- abs_paths %>% filter(has_rel_root)
    if (nrow(abs_rel_paths) > 0) {
      abs_rel_paths <- abs_rel_paths %>%
        group_by(root) %>%
        mutate(new_path = find_common_different_from_start(c(abs_root_folders[1], path_folders))$different[-1]) %>%
        ungroup()
    }

    # absolute paths that don't have a relative root
    abs_paths <- filter(abs_paths, !has_rel_root)
    if (nrow(abs_paths) > 0) {
      common_diff <- find_common_different_from_start(abs_paths$path_folders)
      abs_paths <- abs_paths %>%
        mutate(
          new_path = common_diff$different,
          root = do.call(file.path, args = as.list(common_diff$common))
        )
    }

    # reassemble absolute paths
    abs_paths <-
      bind_rows(abs_paths, abs_rel_paths) %>%
      # expand the paths
      mutate(
        path =
          # process folder and file paths properly
          purrr::pmap(list(path = new_path, is_dir = is_dir, file = basename(path)),
                      function(path, is_dir, file) {
                        if (!is_dir && identical(path, "."))
                          return(file) # file only
                        else if (!is_dir)
                          return(c(path, file)) # path + file
                        else if (length(path) == 0)
                          return(".") # current directory
                        else
                          return(path)
                      }) %>%
          # combine into file path
          map_chr(~do.call(file.path, args = as.list(.x)))
      )
  }

  # combine all
  paths <- bind_rows(abs_paths, filter(paths, !absolute)) %>%  arrange(i)

  return(select(paths, root, path))
}

# file extensions ======


# get file extension
get_file_ext <- function(filepath) {
  basename(filepath) %>% str_extract("\\.[^.]+$")
}

# match file extension
# returns the longest extension that matches
match_file_ext <- function(filepath, extensions) {
  exts_regexp <- extensions %>% stringr::str_to_lower() %>%
    stringr::str_replace_all("\\.", "\\\\.") %>% str_c("$")
  exts <- extensions[str_detect(stringr::str_to_lower(filepath), exts_regexp)]
  if (length(exts) == 0) return(NA_character_)
  else return(exts[stringr::str_length(exts) == max(stringr::str_length(exts))][1])
}

# match multiple filepaths with extensions and return a data frame
# @param filepaths_df data frame with, at minimum, column 'path'
# @param extensions_df data frame with, at miminum, column 'extension'
match_to_supported_file_types <- function(filepaths_df, extensions_df) {
  stopifnot(col_in_df(filepaths_df, "path"))
  stopifnot(col_in_df(extensions_df, "extension"))

  # global vars
  path <- .ext_exists <- NULL

  files <-
    filepaths_df %>%
    mutate(extension = map_chr(path, match_file_ext, extensions_df$extension)) %>%
    left_join(mutate(extensions_df, .ext_exists = TRUE), by = "extension")

  # safety check
  if ( nrow(missing <- dplyr::filter(files, is.na(.ext_exists))) > 0) {
    exts <- missing$path %>% get_file_ext() %>% unique() %>% str_c(collapse = ", ")
    glue::glue(
      "unexpected file extension(s): {exts} ",
      "(expected one of the following: ",
      "{str_c(extensions_df$extension, collapse = ', ')})") %>%
    stop(call. = FALSE)
  }

  return(dplyr::select(files, -.ext_exists))
}

# function execution with error catching =====

# execute function with catch if not in debug mode
# @param func can be either function name or function call
# problems are reported in obj
# @note: maybe could use tidyverse::safely for this at some point?
exec_func_with_error_catch <- function(func, obj, ..., env = asNamespace("isoreader"), msg_prefix = "") {
  if (is.character(func)) func_name <- func
  else func_name <- substitute(func) %>% deparse()
  if (!default("catch_errors")) {
    # debug mode, don't catch any errors
    obj <- do.call(func, args = c(list(obj), list(...)), envir = env)
  } else {
    # regular mode, catch errors and report them as problems
    obj <-
      tryCatch({
        do.call(func, args = c(list(obj), list(...)), envir = env)
      }, error = function(e){
        return(register_error(obj, str_c(msg_prefix, e$message), func = func_name))
      })
  }
  return(obj)
}

# find parent call regardless of if it's called by piping or traditional
# ignores tryCatch calls
# @param current_func the name of the function this is called from (character)
find_parent_call <- function(current_func) {
  calls <- sys.calls()
  calls <- sapply(calls, as.character)
  is_trycatch <- sapply(calls, function(x) any(str_detect(x, "tryCatch")))
  calls <- calls[!is_trycatch]
  has_func <- sapply(calls, function(x) any(str_detect(x, current_func))) %>% which()
  if (has_func[1] == 1) return("") # called from top-level
  calls[[has_func[1] - 1]][1]
}

# formatting =====

# convience function for information message
get_info_message_concat <- function(variable, prefix = "", suffix = "", empty = c(), quotes = TRUE, include_names = FALSE, names_sep = "=", flip_names_and_values = FALSE){
  if (is_quosure(variable) || rlang::is_expression(variable)) {
    # note that as_label does not provide enough information if it is a long call
    # quo_text is in questioning stage but no good replacement exists yet
    variable <- rlang::quo_text(variable, width = 500L)
    if (variable == "NULL") return("")
  } else if (is.list(variable)) {
    variable <- purrr::map_chr(variable, ~{
      if (is_quosure(.x) || rlang::is_expression(.x)) rlang::as_label(.x)
      else as.character(.x)
    })
  }

  # totally empty?
  if (length(variable) == 0) return("")

  # exclude empty strings
  variable <- variable[!variable %in% empty]

  # now empty or all no strings?
  if (length(variable) == 0 || all(nchar(variable) == 0)) return("")

  # any quotes?
  quotes <- if(quotes) "'" else ""

  # include names
  if (include_names && !is.null(names(variable))) {
    var_names <- names(variable)
    if (flip_names_and_values)
      var_names[!nchar(var_names) == 0] <- paste0(quotes, names_sep, quotes, var_names[!nchar(var_names) == 0])
    else
      var_names[!nchar(var_names) == 0] <- paste0(var_names[!nchar(var_names) == 0], quotes, names_sep, quotes)
  } else {
    var_names <- ""
  }
  if (flip_names_and_values)
    vars <- paste(paste0(variable, var_names), collapse = sprintf("%s, %s", quotes, quotes))
  else
    vars <- paste(paste0(var_names, variable), collapse = sprintf("%s, %s", quotes, quotes))
  return(str_c(prefix, quotes, vars, quotes, suffix))
}


# migration to isoprocessor ====

# migration message for function moved to isoprocessor
show_isoprocessor_migration_message <- function(func) {
  glue::glue("as of isoreader version 1.0, '{func}' has moved to ",
             "the isoprocessor package (isoprocessor.isoverse.org) to re-focus ",
             "isoreader on its core functionality. Please install and load ",
             "isoprocessor to access this function:\n",
             "-->  devtools::install_github(\"isoverse/isoprocessor\") # install\n",
             "-->  library(isoprocessor) # load") %>%
    stop(call. = FALSE)
}

# testing utilities ====

# utility function to get a test file path
# could be used to download remote test file for larger test files
get_isoreader_test_file <- function(file, local_folder) {
  return(file.path(local_folder, file))
}
