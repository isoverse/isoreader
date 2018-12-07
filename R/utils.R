# general helper functions ===========

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# collapse helper to deal with naming change in the glue package
collapse <- function(...) {
  if (exists("glue_collapse", where=asNamespace("glue"), mode="function"))
    glue::glue_collapse(...)
  else
    glue::collapse(...)
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
    pb <- get_temp("progress_bar", allow_null = TRUE)
    process <- get_temp("parallel_process", allow_null = FALSE)
    if (!is.na(process)) {
      # save to log file
      log_file <- get_temp("parallel_log_file")
      if (!is.null(log_file)) {
        sprintf("\"%s\",%d,\"%s\"\n", type, process, 
                str_replace_all(.makeMessage(...), fixed("\""), "\\\"")) %>% 
          cat(file = log_file, append = TRUE)
      }
    } else if (!is.null(pb) && !pb$finished) {
      # progress bar
      pb$message(.makeMessage(prefix, ...))
    } else {
      # regular message
      message(.makeMessage(prefix, ...))
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
    logs <- suppressMessages(readr::read_csv(log, col_names = FALSE, skip = status$log_n))
    if (nrow(logs) > 0 && ncol(logs) >= 3) {
      status$log_n <- status$log_n + nrow(logs)
      logs %>% 
        mutate(prefix = case_when(
          X1 == "info" ~ sprintf("Info (process %d): ", X2),
          X1 == "warning" ~ sprintf("Warning (process %d): ", X2),
          TRUE ~ sprintf("Process %d: ", X2)
        )) %>% 
        with(purrr::walk2(X3, prefix, ~log_message(.x, prefix = .y)))
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
  if (!is.null(log) && file.exists(log)) file.remove(log)
  progress <- get_temp("parallel_progress_file")
  if (!is.null(progress) && file.exists(progress)) file.remove(progress)
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
#' @export
iso_get_reader_examples <- function() {
  # global vars
  extension <- filename <- format <- NULL
  file_types <- iso_get_supported_file_types()
  system.file(package = "isoreader", "extdata") %>% 
    expand_file_paths(file_types$extension) %>% 
    { data_frame(filepath = ., filename = basename(filepath) ) } %>% 
    match_to_supported_file_types(file_types) %>% 
    arrange(type, extension, filename) %>% 
    select(filename, type, description)
}

# file paths ====

# guess root path based on common aspects of the filepaths
# prefixes all relative paths with the working directory
# BUT removes the working directory again if it is part of the common filepath of ALL files
# returns a list with two keys:
#   - common: the common path
#   - different: the differences for each filepath
# never includes final filenames in common path
# uses "." if common or different are empty
guess_file_root <- function(filepaths) {
  
  # safety checks
  if(length(filepaths) == 0) return("")
  if (!all(ok <- file.exists(filepaths))) {
    stop("path does not exist: ", 
         paste(basename(filepaths)[!ok], collapse = ", "), 
         call. = FALSE)
  }
  
  # empty path
  empty <- "."
  
  # check absolute vs. relative
  relative <- !R.utils::isAbsolutePath(filepaths)
  is_relative <- all(relative)
  is_file <- !dir.exists(filepaths)
  
  # expand relative paths
  filepaths[relative] <- file.path(getwd(), filepaths[relative])
  
  # find common and different path
  common_different <- 
    filepaths %>% map(get_path_folders) %>% 
    get_common_different_from_start(empty = empty)
  common <- common_different$common
  
  # check whether common is part of the working directory
  wd <- get_path_folders(getwd())
  wd_common <- get_common_different_from_start(list(common, wd))$common
  if (identical(wd, wd_common)) {
    # all of the folders have the working directory
    common <- common[-(1:length(wd))]
    if (length(common) == 0) common <- empty
    is_relative <- TRUE
  }
  
  # absolute path
  if (!is_relative && identical(common, ".")) common <- ""
  
  # add files back to the paths
  different <- common_different$different
  different[is_file] <- map2(
    different[is_file], basename(filepaths)[is_file],
    ~if(identical(.x, empty)) { .y } else { c(.x, .y) })
  
  # return
  return(
    list(
      common = do.call(file.path, args = as.list(common)),
      different = map_chr(different, ~do.call(file.path, args = as.list(.x)))
    )
  )
}

# find out which elements are identical from the start of the vectors
# @param vectors list of vectors
get_common_different_from_start <- function(vectors, empty = character(0)) {
  min_length <- min(map_int(vectors, length))
  if(min_length == 0) {
    return(list(common = empty, different = vectors))
  }
  
  # all path vectors
  vectors <- 
    map2(
      1:length(vectors), vectors, 
      ~data_frame(v = .x, i = 1:length(.y), entry = .y)
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
    nest(-v) %>% 
    full_join(data_frame(
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

# helperfunction to get vector of folders
# only returns folders in the resulting vector unless include_files = TRUE
get_path_folders <- function(filepath, include_files = FALSE) {
  if (!file.exists(filepath)) stop("path does not exist: ", basename(filepath), call. = FALSE)
  if (!include_files && !dir.exists(filepath)) filepath <- dirname(filepath)
  folders <- c()
  while(TRUE) {
    folders <- c(basename(filepath), folders)
    parent <- dirname(filepath)
    if (parent == filepath) break;
    filepath <- parent
  }
  return(folders)
}

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
# @param pfilepaths_df data frame with, at minimum, column 'filepath'
# @param extensions_df data frame with, at miminum, column 'extension'
match_to_supported_file_types <- function(filepaths_df, extensions_df) {
  stopifnot("filepath" %in% names(filepaths_df))
  stopifnot("extension" %in% names(extensions_df))
  files <- 
    filepaths_df %>% 
    mutate(extension = map_chr(filepath, match_file_ext, extensions_df$extension)) %>% 
    left_join(mutate(extensions_df, .ext_exists = TRUE), by = "extension")
  
  # safety check
  if ( nrow(missing <- dplyr::filter(files, is.na(.ext_exists))) > 0) {
    exts <- missing$filepath %>% get_file_ext() %>% unique() %>% str_c(collapse = ", ")
    glue::glue(
      "unexpected file extension(s): {exts} ",
      "(expected one of the following: ",
      "{str_c(extensions_df$extension, collapse = ', ')})") %>% 
    stop(call. = FALSE)
  }
  
  return(dplyr::select(files, -.ext_exists))
}

# expand the file paths in supplied folders that fit the provided data types
# @param extensions regular extensions (with or without the dots), will be turned into regexp pattern
expand_file_paths <- function(paths, extensions = c()) {
  
  # existence check
  exist <- paths %>% sapply(file.exists)
  if (!all(exist)) 
    stop("files/folders do not exist:\n\t", paths[!exist] %>% str_c(collapse = "\n\t"), call. = FALSE)
  
  # extensions check
  if(length(extensions) == 0) stop("no extensions provided for retrieving file paths", call. = FALSE)
  isdir <- paths %>% lapply(file.info) %>% map_lgl("isdir")
  pattern <- extensions %>% str_replace_all("\\.", "\\\\.") %>% str_c(collapse = "|") %>% { str_c("(", ., ")$") }
  has_ext <- paths[!isdir] %>% str_detect(pattern)
  if (!all(has_ext))
    stop("some file(s) do not have one of the supported extensions (", 
         str_c(extensions, collapse = ", "), 
         "):\n\t", paths[!isdir][!has_ext] %>% str_c(collapse = "\n\t"), call. = FALSE)
  
  # retrieve all the files
  filepaths <- 
    mapply(function(path, isdir) {
      list(
        if (!isdir) return(path)
        else file.path(path, list.files(path, pattern = pattern, recursive = TRUE, include.dirs = FALSE))
      )
    }, paths, isdir, USE.NAMES = FALSE) %>% 
    # simplify
    unlist() %>% 
    # make sure all files unique
    unique()
  
  # double check that filenames are unique
  filenames <- basename(filepaths)
  if (anyDuplicated(filenames)) {
    dups <- duplicated(filenames) | duplicated(filenames, fromLast = T)
    warning("some files from different folders have identical file names:\n\t", 
         filepaths[dups] %>% str_c(collapse = "\n\t"), immediate. = TRUE, call. = FALSE)
  }
   
  return(filepaths)
}

# get re-read filepaths
get_reread_filepaths <- function(iso_files) {
  if(!iso_is_object(iso_files)) stop("can only re-read iso_files", call. = FALSE)
  iso_files <- iso_as_file_list(iso_files)
  info <- lapply(iso_files, function(i) i$file_info[c("file_id", "file_path", "file_subpath")])
  return(info %>% map_chr("file_path") %>% unique())
}

# caching ====

# generates the cash file paths for iso_files
generate_cache_filepaths <- function(filepaths, read_options = list()) {
  
  # global vars
  rowname <- size <- mtime <- filepath <- iso_v <- modified <- hash <- cache_file <- NULL
  
  calculate_unf_hash <- function(filepath, size, modified) {
    obj <- c(list(filepath, size, modified), read_options)
    unf(obj)$hash %>% str_c(collapse = "")
  }
  
  file_info <- file.info(filepaths) %>% 
    as_data_frame() %>% 
    rownames_to_column() %>% 
    select(filepath = rowname, size = size, modified = mtime) %>% 
    mutate(
      iso_v = packageVersion("isoreader"),
      hash = mapply(calculate_unf_hash, filepath, size, modified),
      cache_file = sprintf("iso_file_v%d.%d_%s_%s.rds", iso_v$major, iso_v$minor, basename(filepath), hash),
      cache_filepath = file.path(default("cache_dir"), cache_file)
    )
  
  return(file_info$cache_filepath)
}

# Cache iso_file
cache_iso_file <- function(iso_file, cachepath) {
  if (!file.exists(default("cache_dir"))) dir.create(default("cache_dir"))
  saveRDS(iso_file, file = cachepath)
}

# Load cached iso_file
load_cached_iso_file <- function(filepath, check_version = TRUE) {
  # safety check (should never be a problem)
  if (!file.exists(filepath)) stop("cached file does not exist: ", filepath, call. = FALSE) 
  
  # load
  iso_file <- readRDS(filepath)
  
  # make sure object in file was loaded properly
  if (!(iso_is_object(iso_file))) stop("cached file did not contain iso_file(s)", call. = FALSE)
  
  # check version
  # NOTE: this should technially no be possible because the filename of a cached file contains the version
  # however, it is a good safety precaution
  cached_version <- if(iso_is_file_list(iso_file)) iso_file[[1]]$version else iso_file$version
  if (!same_as_isoreader_version(cached_version)) {
    iso_file <- register_warning(iso_file, details = 
      sprintf("file created by a different version of the isoreader package (%s)", as.character(cached_version)))
  }
  
  # return
  return(iso_file)
}

# check for difference in isoreader version
# @note: this function determines which version difference causes caching differences
same_as_isoreader_version <- function(version, isoreader_version = packageVersion("isoreader")) {
  version <- version$major * 10 + version$minor
  isoreader_version <- isoreader_version$major * 10 + isoreader_version$minor
  return(version == isoreader_version)
}

#' Cleanup old cached files
#'
#' Removes old cached files generated by isoreader versions different from the currently installed package. Removes all cached files if \code{clean_all} is set to TRUE.
#' @param all if set to TRUE, all cached files will be removed regardless of their version
#' @export
iso_cleanup_reader_cache <- function(all = FALSE) {
  files <- list.files(default("cache_dir"), pattern = "^iso_?file_.*\\.rds$", full.names = TRUE)
  if (all) {
    file.remove(files)
    if (!default(quiet)) message("Info: removed all (", length(files), ") cached isoreader files.")
  } else {
    iso_file <- NULL
    remove <- sapply(files, function(file){
      iso_file <- readRDS(file)
      if (!(iso_is_object(iso_file))) return(TRUE) # always remove non-iso object files
      cached_version <- if(iso_is_file_list(iso_file)) iso_file[[1]]$version else iso_file$version  
      return(!same_as_isoreader_version(cached_version))
    })
    # remove files
    if (any(remove)) file.remove(files[remove])
    # info message
    if (!default(quiet)) message("Info: removed ", sum(unlist(remove)), " cached isoreader files.")
  }
  invisible(NULL)
}

# function execution with error catching =====

# execute function with catch if not in debug mode
# @param func can be either function name or function call
# problems are reported in obj
# @note: maybe could use tidyverse::safely for this at some point?
exec_func_with_error_catch <- function(func, obj, ..., env = asNamespace("isoreader")) {
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
        return(register_error(obj, e$message, func = func_name))
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
get_info_message_concat <- function(variable, prefix = "", suffix = "", empty = c(), quotes = TRUE){
  if (is_quosure(variable)) {
    if (quo_is_null(variable)) return("")
    variable <- quo_text(variable)
  }
  if (!is_empty(variable) && !all(nchar(variable) == 0) && !variable %in% empty) {
    quotes <- if(quotes) "'" else ""
    vars <- str_c(variable, collapse = str_c(quotes, ", ", quotes, collapse = ""))
    return(str_c(prefix, quotes, vars, quotes, suffix))
  } else
    return("")
}

