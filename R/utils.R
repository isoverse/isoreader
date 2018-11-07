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
  expand_file_paths(system.file(package = "isoreader", "extdata"), iso_get_supported_file_types()$extension) %>% 
    match_to_supported_file_types() %>% 
    arrange(extension, filename) %>% 
    select(filename, extension, format)
}

# file types and paths ====

#' Supported file types
#' 
#' Get an overview of all the file types currently supported by the isoreader package.
#' 
#' @export
iso_get_supported_file_types <- function() {
  # global vars
  extension <- description <- type <- call <- description <- NULL
  bind_rows(
    get_supported_di_files() %>% mutate(type = "Dual Inlet", call = "iso_read_dual_inlet"),
    get_supported_cf_files() %>% mutate(type = "Continuous flow", call = "iso_read_continuous_flow")
  ) %>% 
    mutate(extension = str_c(".", extension)) %>% 
    select(extension, format = description, type, call) 
}

# match files to supported file types
match_to_supported_file_types <- function(filepaths) {
  # global vars
  filepath <- filename <- extension <- ext_id <- format <- NULL
  supported_files <- iso_get_supported_file_types()
  
  # extensions
  ext_regexps <- supported_files$extension %>% str_c("$")
  find_extension <- function(filename) which(str_detect(filename, ext_regexps)) %>% { if(length(.) >0) min(.) else NA_integer_ }
  data_frame(
    filepath = filepaths,
    filename = basename(filepath),
    ext_id = sapply(filename, find_extension),
    extension = supported_files$extension[ext_id]
  ) %>% 
    left_join(supported_files, by = "extension") %>% 
    select(-ext_id)
}

# get file extension
get_file_ext <- function(filepath) {
  basename(filepath) %>% str_extract("\\.[^.]+$")
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
  pattern <- extensions %>% str_replace("^(\\.)?", "\\\\.") %>% str_c(collapse = "|") %>% { str_c("(", ., ")$") }
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
    stop("some files from different folders have identical file names:\n\t", 
         filepaths[dups] %>% str_c(collapse = "\n\t"), call. = FALSE)
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
exec_func_with_error_catch <- function(func, obj, ...) {
  if (is.character(func)) func_name <- func
  else func_name <- substitute(func) %>% deparse()
  if (!default("catch_errors")) {
    # debug mode, don't catch any errors
    obj <- do.call(func, args = c(list(obj), list(...)))
  } else {
    # regular mode, catch errors and report them as problems
    obj <- 
      tryCatch({
        do.call(func, args = c(list(obj), list(...)))
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

