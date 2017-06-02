# Structures ----

# basic data structure
make_iso_data_structure <- function() {
  structure(
    list(
      version = packageVersion("isoreader"),
      read_options = list( # records read options+defaults
        file_info = FALSE, # whether file info was read
        raw_data = FALSE # whether mass data was raed (Note: maybe not top-level b/c of scans?)
      ), 
      file_info = list(
        file_id = NA_character_, # unique identifer
        file_path = NA_character_, # path to file (file extension is key for processing)
        file_subpath = NA_character_ # sub path in case file is an archieve
      ),
      raw_data = data_frame() # all mass data (Note: maybe not top-level b/c of scans?)
    ),
    class = c("isofile")
  ) %>% 
    initialize_problems_attribute()
}


# basic dual inlet data structure
make_di_data_structure <- function() {
  struct <- make_iso_data_structure()
  class(struct) <- c("dual_inlet", class(struct))
  return(struct)
}

# basic continuous flow data structure
make_cf_data_structure <- function() {
  struct <- make_iso_data_structure()
  
  # add data_table read option
  struct$read_options <- struct$read_options %>% 
    modifyList(list(data_table = FALSE))
  class(struct) <- c("continuous_flow", class(struct))
  return(struct)
}

# Printing ----

#' Print a collection of isofiles
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @export
print.isofiles <- function(x, ...) {
  sprintf("# data from %d isofiles:\n", length(x)) %>% 
    cat()
  sapply(x, print, show_problems = FALSE)
  if (n_problems(x) > 0) {
    cat("\nAll encountered problems (", n_problems(x), "):\n", sep = "")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' Print a dual inlet isofile
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @param show_problems whether to show encountered problems
#' @export
print.dual_inlet <- function(x, ..., show_problems = TRUE) {
  sprintf("Dual inlet data '%s' (%s; %s) from %s%s\n", 
          x$file_info$file_id,
          get_raw_data_info(x),
          get_file_info_info(x),
          x$file_info$file_path,
          x$file_info$file_subpath %>% { if(!is.na(.)) str_c("|", .) else "" }
  ) %>% cat()
  if (show_problems && n_problems(x) > 0) {
    cat("Problems:\n")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' Print a continuous_flow isofile
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @param show_problems whether to show encountered problems
#' @export
print.continuous_flow <- function(x, ..., show_problems = TRUE) {
  sprintf("Continuous flow data '%s' (%s; %s) from %s%s\n", 
          x$file_info$file_id,
          get_raw_data_info(x),
          get_file_info_info(x),
          x$file_info$file_path,
          x$file_info$file_subpath %>% { if(!is.na(.)) str_c("|", .) else "" }
  ) %>% cat()
  if (show_problems && n_problems(x) > 0) {
    cat("Problems:\n")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

# print info
get_raw_data_info <- function(x) {
  if (x$read_options$raw_data) {
    sprintf(
      "%d data points: %s",
      x$raw_data %>% nrow(),
      x$raw_data %>% select(matches("^[iIvV]")) %>% names() %>% 
      { if(length(.) == 0) "0 ions" else str_c(., collapse = ", ") }
    )
  } else {
    "raw data not read"
  }
}
get_file_info_info <- function(x) {
  if (x$read_options$file_info) {
    sprintf(
      "%d file info entries",
      length(x$file_info)
    )
  } else {
    "file info not read"
  }
}

# Update structures =====

# set data structure file path
set_ds_file_path <- function(ds, file_path, file_id = basename(file_path), file_subpath = NA_character_) {
  if (!is(ds, "isofile")) stop("can only set path for isofile data structures", call. = FALSE)
  if (!file.exists(file_path)) stop("file/folder does not exist: ", file_path, call. = FALSE)
  ds$file_info$file_path <- file_path
  ds$file_info$file_id <- file_id
  ds$file_info$file_subpath <- file_subpath
  return(ds)
}

# update read options in structure
update_read_options <- function(ds, ...) {
  dots <- list(...)
  # remove read_ prefix in function parameters
  names(dots) <- names(dots) %>% str_replace("^read_", "") 
  update <- dots[names(dots) %in% names(ds$read_options)]
  # update all that exist in the read options
  ds$read_options <- modifyList(ds$read_options, update)
  return(ds)
}

# Caching of data struturces ====

# generates the cash file path for an isofile
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
    { file.path(setting("cache_dir"), str_c("isofile_", ., ".RData")) }
}

#' Cleanup old cached files
#'
#' Removes cached files generated by isoreader versions different from the currently installed package. Remove all cached files if \code{clean_all} is set to TRUE.
#' @param clean_all if set to TRUE, all cached files will be removed regardless of their version
#' @export
cleanup_isoreader_cache <- function(clean_all = FALSE) {
  files <- list.files(setting("cache_dir"), pattern = "isofile_[^.]+\\.RData", full = TRUE)
  if (clean_all) {
    file.remove(files)
    if (!setting("quiet")) message("Info: removed all (", length(files), ") cached isoreader files.")
  } else {
    remove <- sapply(files, function(file){
      load(file)
      if (!exists("isofile", inherits = FALSE) || !(is(isofile, "isofiles") || is(isofile, "isofile"))) return(TRUE)
      cached_version <- if(is(isofile, "isofiles")) isofile[[1]]$version else isofile$version  
      if (cached_version != packageVersion("isoreader")) return(TRUE)
      return(FALSE)
    })
    if (any(remove))
      file.remove(files[remove])
    if (!setting("quiet")) message("Info: removed ", sum(remove), " cached isoreader files.")
  }
  invisible(NULL)
}
