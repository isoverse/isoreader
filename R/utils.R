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


# example files ====

#' Example files
#' 
#' @description The isoreader package comes with a few example files to make it easy to illustrate the functionality.
#' 
#' @details \code{isoreader_example}: retrieve the path to an isoreader example file
#' @param filename the name of the example file for which to retrieve the system path
#' @export
isoreader_example <- function(filename) {
  filepath <- system.file(package = "isoreader", "extdata", filename)
  if(!file.exists(filepath)) 
    sprintf("The example file '%s' does not exist. Please use get_isoreader_examples() to see a list of all available example files.", filename) %>% 
    stop(call. = FALSE)
  return(filepath)
}

#' @rdname isoreader_example
#' @details \code{get_isoreader_examples}: list of all available isoreader example files
#' @export
get_isoreader_examples <- function() {
  
  supported_files <- get_supported_file_types()
  all_examples <- expand_file_paths(system.file(package = "isoreader", "extdata"), supported_files$extension)
  
  # global vars
  filename <- extension <- ext_id <- format <- NULL
  
  # extensions
  ext_regexps <- supported_files$extension %>% str_c("$")
  find_extension <- function(filename) min(which(str_detect(filename, ext_regexps)))
  data_frame(
    filename = basename(all_examples),
    ext_id = sapply(filename, find_extension),
    extension = supported_files$extension[ext_id]
  ) %>% 
    left_join(supported_files, by = "extension") %>% 
    arrange(extension, filename) %>% 
    select(filename, extension, format)
}

# file types and paths ====

#' Supported file types
#' 
#' Get an overview of all the file types currently supported by the isoreader package.
#' 
#' @export
get_supported_file_types <- function() {
  # global vars
  extension <- description <- type <- call <- description <- NULL
  bind_rows(
    get_supported_di_files() %>% mutate(type = "Dual Inlet", call = "read_dual_inlet()"),
    get_supported_cf_files() %>% mutate(type = "Continuous flow", call = "read_continuous_flow()")
  ) %>% 
    mutate(extension = str_c(".", extension)) %>% 
    select(extension, format = description, type, call) 
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
get_reread_filepaths <- function(isofiles) {
  if(!is_iso_object(isofiles)) stop("can only re-read isofiles", call. = FALSE)
  isofiles <- as_isofile_list(isofiles)
  info <- lapply(isofiles, function(i) i$file_info[c("file_id", "file_path", "file_subpath")])
  return(info %>% map_chr("file_path") %>% unique())
}

# caching ====

# generates the cash file paths for isofiles
generate_cache_filepaths <- function(filepaths, ...) {
  params <- list(...)
  
  # global vars
  rowname <- size <- mtime <- filepath <- modified <- hash <- cache_file <- NULL
  
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

# formatting =====

# convience function for information message
get_info_message_concat <- function(variable, prefix = "", suffix = "", quotes = TRUE){
  if (!is.null(variable) > 0) {
    quotes <- if(quotes) "'" else ""
    vars <- str_c(variable, collapse = str_c(quotes, ", ", quotes, collapse = ""))
    return(str_c(prefix, quotes, vars, quotes, suffix))
  } else
    return("")
}

