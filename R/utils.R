#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# helper to make sure columns exist
# NOTE: is this used?
col_check <- function(cols, data, fun = sys.call(-1), msg = "You may have to change the parameters in your function call") {
  if (!is.null(cols) && length(missing <- setdiff(cols, names(data))) > 0) 
    stop("column(s) not in data: '", str_c(missing, collapse = "', '"), 
         "'. ", msg, ". Function: ", fun, call. = FALSE)
}

# file types and paths ====

#' Show supported file types
#' @export
show_supported_file_types <- function() {
  extension <- NULL
  description <- NULL
  sprintf(
    c("Isoreader supported file types",
      "Dual Inlet | 'read_dual_inlet()':",
      "   %s", 
      "Continuous flow | 'read_continuous_flow()':",
      "   %s") %>% str_c(collapse = "\n"),
    get_supported_di_files() %>% 
      mutate(label = str_c(".", extension, " = ", description)) %>% 
      { str_c(.$label, collapse = "\n   ") },
    get_supported_cf_files() %>% 
      mutate(label = str_c(".", extension, " = ", description)) %>% 
      { str_c(.$label, collapse = "\n   ") }
  ) %>% 
    cat()
}

# get file extension
get_file_ext <- function(filepath) {
  basename(filepath) %>% str_extract("\\.[^.]+$")
}

# expand the file paths in supplied folders that fit the provided data types
# @param extensions regular extensions (without the dots), will be turned into regexp pattern
expand_file_paths <- function(paths, extensions = c()) {
  
  # existence check
  exist <- paths %>% sapply(file.exists)
  if (!all(exist)) 
    stop("files/folders do not exist:\n\t", paths[!exist] %>% str_c(collapse = "\n\t"), call. = FALSE)
  
  # extensions check
  if(length(extensions) == 0) stop("no extensions provided for retrieving file paths", call. = FALSE)
  isdir <- paths %>% lapply(file.info) %>% map_lgl("isdir")
  pattern <- extensions %>% str_c(collapse = "|") %>% { str_c("\\.(", ., ")$") }
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

# function execution with error catching =====

# execute function with catch if not in debug mode
# @param func can be either function name or function call
# problems are reported in obj
# @note: maybe could use tidyverse::safely for this at some point?
exec_func_with_error_catch <- function(func, obj, ...) {
  if (is.character(func)) func_name <- func
  else func_name <- substitute(func) %>% deparse()
  if (!setting("catch_errors")) {
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

# Caching ====

# generates the cash file path for an isofile
# @deprecated
generate_cache_file_path <- function(ds) {
  file_info <- file.info(ds$file_info$file_path)
  # combine filepath, file size and last modified date to generate universal numeric fingerprint
  list(
    filepath = ds$file_info$file_path,
    size = file_info$size,
    modified = file_info$mtime,
    read_options = ds$read_options
  ) %>% 
    # generate numeric fingerprint has and combine to generate cache filename
    unf() %>% {.$hash} %>% str_c(collapse = "") %>% 
    # combine with file name
    { file.path(setting("cache_dir"), str_c("isofile_", ., ".rda")) }
}

# generates the cash file paths for isofiles
generate_cache_filepaths <- function(filepaths, ...) {
  params <- list(...)
  
  calculate_unf_hash <- function(filepath, size, modified) {
    obj <- c(list(filepath, size, modified), params)
    unf(obj)$hash %>% str_c(collapse = "")
  }
  
  file_info <- file.info(filepaths) %>% 
    as_data_frame() %>% 
    rownames_to_column() %>% 
    select(filepath = rowname, size = size, modified = mtime) %>% 
    mutate(
      version = packageVersion("isoreader") %>% as.character(),
      hash = mapply(calculate_unf_hash, filepath, size, modified),
      cache_file = sprintf("isofile_v-%s_f-%s_h-%s.rds", version, basename(filepath), hash),
      cache_filepath = file.path(setting("cache_dir"), cache_file)
    )
  
  return(file_info$cache_filepath)
}

# Cache isofile
cache_isofile <- function(isofile, cachepath) {
  if (!file.exists(setting("cache_dir"))) dir.create(setting("cache_dir"))
  saveRDS(isofile, file = cachepath)
}

# Load cached isofile
load_cached_isofile <- function(filepath, check_version = TRUE) {
  # safety check (should never be a problem)
  if (!file.exists(filepath)) stop("cached file does not exist: ", filepath, call. = FALSE) 
  
  # load
  isofile <- readRDS(filepath)
  
  # make sure object in file was loaded properly
  if (!(is_iso_object(isofile))) stop("cached file did not contain isofile(s)", call. = FALSE)
  
  # check version
  cached_version <- if(is_isofile_list(isofile)) isofile[[1]]$version else isofile$version
  if (cached_version != packageVersion("isoreader")) {
    isofile <- register_warning(isofile, details = "file created by a different version of the isoreader package")
  }
  
  # return
  return(isofile)
}



#' Cleanup old cached files
#'
#' Removes old cached files generated by isoreader versions different from the currently installed package. Removes all cached files if \code{clean_all} is set to TRUE.
#' @param all if set to TRUE, all cached files will be removed regardless of their version
#' @export
cleanup_isoreader_cache <- function(all = FALSE) {
  files <- list.files(setting("cache_dir"), pattern = "isofile_[^.]+\\.rds", full.names = TRUE)
  if (all) {
    file.remove(files)
    if (!setting("quiet")) message("Info: removed all (", length(files), ") cached isoreader files.")
  } else {
    isofile <- NULL
    remove <- sapply(files, function(file){
      load(file)
      if (!exists("isofile", inherits = FALSE) || !(is_iso_object(isofile))) return(TRUE) # always remove non-iso object files
      cached_version <- if(is_isofile_list(isofile)) isofile[[1]]$version else isofile$version  
      if (cached_version != packageVersion("isoreader")) return(TRUE)
      return(FALSE)
    })
    if (any(remove))
      file.remove(files[remove])
    if (!setting("quiet")) message("Info: removed ", sum(remove), " cached isoreader files.")
  }
  invisible(NULL)
}

# formattng =====

# convience function for information message
get_info_message_concat <- function(variable, prefix = "", suffix = "", quotes = TRUE){
  if (!is.null(variable) > 0) {
    quotes <- if(quotes) "'" else ""
    vars <- str_c(variable, collapse = str_c(quotes, ", ", quotes, collapse = ""))
    return(str_c(prefix, quotes, vars, quotes, suffix))
  } else
    return("")
}

