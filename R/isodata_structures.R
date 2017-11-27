# Structures ----

# basic data structure
make_iso_file_data_structure <- function() {
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
        file_subpath = NA_character_, # sub path in case file is an archieve
        file_datetime = NA # the run date and time of the file
      ),
      method_info = list(), # all methods information
      raw_data = data_frame(), # all mass data (Note: maybe not top-level b/c of scans?)
      vendor_data_table = data_frame() %>% # vendor computed data table (no units)
        { attr(., "units") <- NA; . }
    ),
    class = c("iso_file")
  ) %>% 
    initialize_problems_attribute()
}


# basic dual inlet data structure
make_di_data_structure <- function() {
  struct <- make_iso_file_data_structure()
  class(struct) <- c("dual_inlet", class(struct))
  return(struct)
}

# basic continuous flow data structure
make_cf_data_structure <- function() {
  struct <- make_iso_file_data_structure()
  
  # add data_table read option
  struct$read_options <- struct$read_options %>% 
    modifyList(list(data_table = FALSE))
  class(struct) <- c("continuous_flow", class(struct))
  return(struct)
}


# Class testing ====


#' Isoreader data structure functions
#' 
#' @description \code{iso_is_file} tests if the object is an iso_file 
#'
#' @param x an object to test whether it has the specific class
#' @rdname iso_data_structure
#' @export
iso_is_file <- function(x) {
  "iso_file" %in% class(x)
}

#' @description \code{iso_is_file_list} tests if the object is an iso_file list (collection of iso_files)
#' @rdname iso_data_structure
#' @export
iso_is_file_list <- function(x) {
  "iso_file_list" %in% class(x)
}

#' @description \code{iso_is_object} test if the object is an iso-object (iso_file or iso_file list)
#' @rdname iso_data_structure
#' @export
iso_is_object <- function(x) {
  iso_is_file(x) || iso_is_file_list(x)
}

#' @description \code{iso_is_dual_inlet} tests if an iso_file or iso_file list consists exclusively of dual inlet file objects
#' @rdname iso_data_structure
#' @export
iso_is_dual_inlet <- function(x) {
  if(!iso_is_object(x)) return(FALSE)
  all(sapply(iso_as_file_list(x), is, "dual_inlet"))
}

#' @description \code{iso_is_continuous_flow} tests if an iso_file or iso_file list consists exclusively of continuous flow file objects
#' @rdname iso_data_structure
#' @export
iso_is_continuous_flow <- function(x) {
  if(!iso_is_object(x)) return(FALSE)
  all(sapply(iso_as_file_list(x), is, "continuous_flow"))
}


# Iso file list ----

#' @description \code{iso_as_file_list} concatenates iso_file and iso_file list object(s) into one combined iso_file list (equivalent to calling \code{c(...)}), flattens all passed lists into one list structure, all individual objects and objects within iso_file lists have to be the same type of iso_file, issues warnings if there are duplicate file ids and summarizes all problems in the iso_file list
#' @param ... iso_file and iso_file_list objects to concatenate
#' @param discard_duplicates whether to discard encountered file id duplicates
#' @rdname iso_data_structure
#' @export
iso_as_file_list <- function(..., discard_duplicates = TRUE) {

  # dots passed in
  iso_objs <- list(...)
  
  # allow simple list to be passed in
  if (length(iso_objs) == 1 && !iso_is_object(..1) && is.list(..1)) iso_objs <- ..1
  
  if (length(iso_objs) == 0) {
    # empty list
    iso_list <- list()
    iso_get_problems <- get_problems_structure() %>% mutate(file_id = character())
  } else {
    # combine everything
    if(!all(is_iso <- sapply(iso_objs, iso_is_object))) {
      stop("can only combine iso_file and iso_file_list objects, encountered incompatible data type(s): ",
           unlist(lapply(iso_objs, class)[!is_iso]) %>% 
           { str_c(unique(.), collapse = ", ")}, call. = FALSE)
    }
    
    # combine iso objects
    iso_list <- lapply(iso_objs, function(obj) {
      if (iso_is_file_list(obj)) as.list(obj) # iso lists already have named entries
      else setNames(list(obj), obj$file_info$file_id) # use file_id to name new files
    }) %>% 
    { do.call(c, .) }
    
    # check if al ellements are the same data type
    classes <- lapply(iso_list, class) 
    if (!all(sapply(classes, function(x) all(x == classes[[1]])))) {
      str_interp("can only combine iso_file objects with the same data type (first: ${ref_dt}), encountered: ${wrong_dt}", 
                 list(ref_dt = classes[[1]][1], 
                      wrong_dt = classes %>% sapply(`[`, 1) %>% { .[.!=classes[[1]][1]] } %>% 
                        { str_c(unique(.), collapse = ", ")})) %>% 
        stop(call. = FALSE)
    }
    
    # check for name duplicates and register a warning if there are any
    if (any(dups <- duplicated(names(iso_list)) | duplicated(names(iso_list), fromLast = TRUE))) {
      msg <- if(discard_duplicates) "duplicate files encountered, only first kept" else "duplicate files kept, may interfere with data processing"
      for (idx in which(dups)) {
        iso_list[[idx]] <- register_warning(
          iso_list[[idx]], str_c(str_replace_na(c(msg, ": ", names(iso_list)[idx])), collapse = ""))
      }
      
      if (discard_duplicates) {
        iso_list[duplicated(names(iso_list))] <- NULL
      }
    }
    
    # propagate problems
    iso_get_problems <- lapply(iso_list, function(iso_file) {
      get_problems(iso_file) %>% 
        mutate_(.dots = list(file_id = ~iso_file$file_info$file_id))
    }) %>% bind_rows()
  }
  
  # problems
  iso_get_problems <- iso_get_problems %>% 
    unique() %>% # remove duplicate entries
    { select_(., .dots = c("file_id", names(.))) }
  
  # generate structure
  structure(
    iso_list,
    class = c("iso_file_list")
  ) %>% set_problems(iso_get_problems)
}


# Printing ----

#' Isofile printing
#' 
#' Print summary of individual iso_files (dual inlet or continuous flow) or collection of iso_files.
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @param n_max how many file lines to print maximally
#' @rdname iso_printing
#' @export
print.iso_file_list <- function(x, ...) {
  
  # what type of iso files
  data_type <- class(x[[1]]) %>% { .[.!="iso_file"][1] } %>% str_replace("_", " ")
  
  # print summary
  glue("Data from {length(x)} {data_type} iso files:") %>% cat("\n")
  print(iso_get_data_summary(x, quiet = TRUE))
  
  if (n_problems(x) > 0) {
    cat("\nProblem summary:\n", sep = "")
    print(iso_get_problems_summary(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' @param show_problems whether to show encountered problems
#' @rdname iso_printing
#' @export
print.iso_file <- function(x, ..., show_problems = TRUE) {
  data_type <- class(x) %>% { .[.!="iso_file"][1] } %>% 
    str_to_title() %>% str_replace("_", " ")
  if (is.na(data_type)) data_type <- "Iso"
  sprintf("%s iso file '%s': %s", #; file_info: %s method_info: %s; vendor_data_table: %s", 
          data_type,
          get_file_id(x),
          get_raw_data_info(x)
          #get_file_info_info(x),
          #get_method_info_info(x),
          #get_vendor_data_table_info(x)
  ) %>% cat("\n")
  if (show_problems && n_problems(x) > 0) {
    cat("Problems:\n")
    print(iso_get_problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' @rdname iso_printing
#' @export
print.dual_inlet <- function(x, ..., show_problems = TRUE) {
  NextMethod("print", x, ..., show_problems = show_problems)
}

#' @rdname iso_printing
#' @export
print.continuous_flow <- function(x, ..., show_problems = TRUE) {
  NextMethod("print", x, ..., show_problems = show_problems)
}


# Update structures =====

# set data structure file path
set_ds_file_path <- function(ds, file_path, file_id = basename(file_path), file_subpath = NA_character_) {
  if (!iso_is_file(ds)) stop("can only set path for iso_file data structures", call. = FALSE)
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

# S3 operations =====

#' @export
as.list.iso_file <- function(x, ...) {
  class(x) <- "list"
  return(x)
}

#' @export
as.list.iso_file_list <- function(x, ...) {
  class(x) <- "list"
  return(x)
}

# subset iso_file list - note that non-existant indices are silently dropped
#' @export
`[.iso_file_list` <- function(x, i) {
  # subset as regular list
  l <- NextMethod("[")
  # remove NULL entries
  l <- unname(l[!map_lgl(l, is.null)])
  # make iso_file list from the subset
  iso_as_file_list(l)
}

#' @export
`[<-.iso_file_list` <- function(x, i, value) {
  # regular replacement
  l <- NextMethod("[<-")
  # iso_as_file_list with the replaced item
  iso_as_file_list(unname(as.list(l)))
}

#' @export
`[[<-.iso_file_list` <- function(x, i, value) {
  # regular replacement
  l <- NextMethod("[<-")
  # iso_as_file_list with the replaced item
  iso_as_file_list(unname(as.list(l)))
}

# combine iso_file with other things
#' @export
c.iso_file <- function(...) {
  iso_as_file_list(...)
}

# combine iso_file other things
#' @export
c.iso_file_list <- function(...) {
  iso_as_file_list(...)
}
