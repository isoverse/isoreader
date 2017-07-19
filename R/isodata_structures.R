# Structures ----

# basic data structure
make_isofile_data_structure <- function() {
  structure(
    list(
      version = packageVersion("isoreader"),
      read_options = list( # records read options+defaults
        file_info = FALSE, # whether file info was read
        method_info = FALSE, # whether method info was read
        raw_data = FALSE, # whether mass data was read (Note: maybe not top-level b/c of scans?)
        vendor_data_table = FALSE # whether vendor data table was read
      ), 
      file_info = list(
        file_id = NA_character_, # unique identifer
        file_path = NA_character_, # path to file (file extension is key for processing)
        file_subpath = NA_character_ # sub path in case file is an archieve
      ),
      method_info = list(), # all methods information
      raw_data = data_frame(), # all mass data (Note: maybe not top-level b/c of scans?)
      vendor_data_table = data_frame() # vendor computed data table
    ),
    class = c("isofile")
  ) %>% 
    initialize_problems_attribute()
}


# basic dual inlet data structure
make_di_data_structure <- function() {
  struct <- make_isofile_data_structure()
  class(struct) <- c("dual_inlet", class(struct))
  return(struct)
}

# basic continuous flow data structure
make_cf_data_structure <- function() {
  struct <- make_isofile_data_structure()
  
  # add data_table read option
  struct$read_options <- struct$read_options %>% 
    modifyList(list(data_table = FALSE))
  class(struct) <- c("continuous_flow", class(struct))
  return(struct)
}


# Class testing ====


#' Isoreader data structure functions
#' 
#' @description \code{is_isofile} tests if the object is an isofile 
#'
#' @param x an object to test whether it has the specific class
#' @rdname data_structure
#' @export
is_isofile <- function(x) {
  "isofile" %in% class(x)
}

#' @description \code{is_isofile_list} tests if the object is an isofile list (collection of isofiles)
#' @rdname data_structure
#' @export
is_isofile_list <- function(x) {
  "isofile_list" %in% class(x)
}

#' @description \code{is_iso_object} test if the object is an iso-object (isofile or isofile list)
#' @rdname data_structure
#' @export
is_iso_object <- function(x) {
  is_isofile(x) || is_isofile_list(x)
}

#' @description \code{is_dual_inlet} tests if an isofile or isofile list consists exclusively of dual inlet file objects
#' @rdname data_structure
#' @export
is_dual_inlet <- function(x) {
  if(!is_iso_object(x)) return(FALSE)
  all(sapply(as_isofile_list(x), is, "dual_inlet"))
}

#' @description \code{is_continuous_flow} tests if an isofile or isofile list consists exclusively of continuous flow file objects
#' @rdname data_structure
#' @export
is_continuous_flow <- function(x) {
  if(!is_iso_object(x)) return(FALSE)
  all(sapply(as_isofile_list(x), is, "continuous_flow"))
}


# Iso file list ----

#' @description \code{as_isofile_list} concatenates isofile and isofile list object(s) into one combined isofile list (equivalent to calling \code{c(...)}), flattens all passed lists into one list structure, all individual objects and objects within isofile lists have to be the same type of isofile, issues warnings if there are duplicate file ids and summarizes all problems in the isofile list
#' @param ... isofile and isofile_list objects to concatenate
#' @param discard_duplicates whether to discard encountered file id duplicates
#' @rdname data_structure
#' @export
as_isofile_list <- function(..., discard_duplicates = TRUE) {

  # dots passed in
  iso_objs <- list(...)
  
  # allow simple list to be passed in
  if (length(iso_objs) == 1 && !is_iso_object(..1) && is.list(..1)) iso_objs <- ..1
  
  if (length(iso_objs) == 0) {
    # empty list
    iso_list <- list()
    iso_problems <- get_problems_structure() %>% mutate(file_id = character())
  } else {
    # combine everything
    if(!all(is_iso <- sapply(iso_objs, is_iso_object))) {
      stop("can only combine isofile and isofile_list objects, encountered incompatible data type(s): ",
           unlist(lapply(iso_objs, class)[!is_iso]) %>% 
           { str_c(unique(.), collapse = ", ")}, call. = FALSE)
    }
    
    # combine iso objects
    iso_list <- lapply(iso_objs, function(obj) {
      if (is_isofile_list(obj)) as.list(obj) # iso lists already have named entries
      else setNames(list(obj), obj$file_info$file_id) # use file_id to name new files
    }) %>% 
    { do.call(c, .) }
    
    # check if al ellements are the same data type
    classes <- lapply(iso_list, class) 
    if (!all(sapply(classes, function(x) all(x == classes[[1]])))) {
      stop("can only combine isofile objects with the same data type, encountered: ", 
           unlist(classes) %>% { .[.!="isofile"] } %>% 
           { str_c(unique(.), collapse = ", ")}, call. = FALSE)
    }
    
    # check for name duplicates and register a warning if there are any
    if (any(dups <- duplicated(names(iso_list)) | duplicated(names(iso_list), fromLast = TRUE))) {
      msg <- if(discard_duplicates) "duplicate files encountered, only first kept" else "duplicate files kept, may interfere with data processing"
      for (idx in which(dups)) {
        iso_list[[idx]] <- register_warning(
          iso_list[[idx]], str_c(msg, ": ", names(iso_list)[idx]))
      }
      
      if (discard_duplicates) {
        iso_list[duplicated(names(iso_list))] <- NULL
      }
    }
    
    # propagate problems
    iso_problems <- lapply(iso_list, function(isofile) {
      get_problems(isofile) %>% 
        mutate_(.dots = list(file_id = ~isofile$file_info$file_id))
    }) %>% bind_rows()
  }
  
  # problems
  iso_problems <- iso_problems %>% 
    unique() %>% # remove duplicate entries
    { select_(., .dots = c("file_id", names(.))) }
  
  # generate structure
  structure(
    iso_list,
    class = c("isofile_list")
  ) %>% set_problems(iso_problems)
}


# Printing ----

#' Print summary of individual isofiles (dual inlet or continuous flow) or collection of isofiles.
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @rdname printing
#' @export
print.isofile_list <- function(x, ...) {
  sprintf("# data from %d isofiles:\n", length(x)) %>% 
    cat()
  sapply(x, print, show_problems = FALSE)
  if (n_problems(x) > 0) {
    cat("Problems:\n", sep = "")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' @param show_problems whether to show encountered problems
#' @rdname printing
#' @export
print.isofile <- function(x, ..., show_problems = TRUE) {
  data_type <- class(x) %>% { .[.!="isofile"][1] } %>% 
    str_to_title() %>% str_replace("_", " ")
  if (is.na(data_type)) data_type <- "Iso"
  sprintf("%s data '%s' (%s; %s) from %s%s\n", 
          data_type,
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

#' @rdname printing
#' @export
print.dual_inlet <- function(x, ..., show_problems = TRUE) {
  NextMethod("print", x, ..., show_problems = show_problems)
}

#' @rdname printing
#' @export
print.continuous_flow <- function(x, ..., show_problems = TRUE) {
  NextMethod("print", x, ..., show_problems = show_problems)
}

# print info
get_raw_data_info <- function(x) {
  if (x$read_options$raw_data) {
    sprintf(
      "%d recordings of %s",
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
#' @param all if set to TRUE, all cached files will be removed regardless of their version
#' @export
cleanup_isoreader_cache <- function(all = FALSE) {
  files <- list.files(setting("cache_dir"), pattern = "isofile_[^.]+\\.RData", full.names = TRUE)
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


# S3 operations =====

#' @export
as.list.isofile <- function(x, ...) {
  class(x) <- "list"
  return(x)
}

#' @export
as.list.isofile_list <- function(x, ...) {
  class(x) <- "list"
  return(x)
}

# subset isofile list - note that non-existant indices are silently dropped
#' @export
`[.isofile_list` <- function(x, i) {
  # subset as regular list
  l <- NextMethod("[")
  # remove NULL entries
  l <- unname(l[!map_lgl(l, is.null)])
  # make isofile list from the subset
  as_isofile_list(l)
}

#' @export
`[<-.isofile_list` <- function(x, i, value) {
  # regular replacement
  l <- NextMethod("[<-")
  # as_isofile_list with the replaced item
  as_isofile_list(unname(as.list(l)))
}

#' @export
`[[<-.isofile_list` <- function(x, i, value) {
  # regular replacement
  l <- NextMethod("[<-")
  # as_isofile_list with the replaced item
  as_isofile_list(unname(as.list(l)))
}

# combine isofile with other things
#' @export
c.isofile <- function(...) {
  as_isofile_list(...)
}

# combine isofile other things
#' @export
c.isofile_list <- function(...) {
  as_isofile_list(...)
}
