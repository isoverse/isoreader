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
      file_info = dplyr::tibble(
        file_id = NA_character_, # unique identifer
        file_root = NA_character_, # root directory for file path
        file_path = NA_character_, # path to file (file extension is key for processing)
        file_subpath = NA_character_, # sub path in case file is an archieve
        file_datetime = NA_integer_ # the run date and time of the file
      ),
      method_info = list(), # all methods information
      raw_data = dplyr::tibble(), # all mass data (Note: maybe not top-level b/c of scans?)
      vendor_data_table = dplyr::tibble() %>% # vendor computed data table (no units)
        { attr(., "units") <- NA; . }
    ),
    class = c("iso_file")
  ) %>% 
    initialize_problems_attribute()
}


# basic dual inlet data structure
make_di_data_structure <- function() {
  struct <- make_iso_file_data_structure()
  struct$bgrd_data <- data_frame() # store background data
  class(struct) <- c("dual_inlet", class(struct))
  return(struct)
}

# basic continuous flow data structure
make_cf_data_structure <- function() {
  struct <- make_iso_file_data_structure()
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

#' @description \code{iso_as_file_list} concatenates iso_file and iso_file list object(s) into one combined iso_file list (equivalent to calling \code{c(...)}), flattens all passed lists into one list structure, all individual objects and objects within iso_file lists have to be the same type of iso_file, issues warnings if there are duplicate file ids and summarizes all problems in the iso_file list. If duplicates are allowed (\code{discard_duplicates = FALSE}), their file IDs will append a #1, #2, #3, etc. to preserve unique file IDs (important for many data aggregation operations). 
#' @param ... iso_file and iso_file_list objects to concatenate
#' @param discard_duplicates whether to automatically discard files with duplicate file IDs (i.e. duplicate file names). If \code{TRUE} (the default), only the first files are kept and any files with the same file ID are discarded. If \code{FALSE}, all duplicate files are kept but their file IDs are appended with suffix \code{#1}, \code{#2}, etc. 
#' @rdname iso_data_structure
#' @export
iso_as_file_list <- function(..., discard_duplicates = TRUE) {

  # dots passed in
  iso_objs <- list(...)
  
  # return iso file list right away if it's the only thing passed in
  if (length(iso_objs) == 1 && iso_is_file_list(..1)) return (..1)
  
  # allow simple list to be passed in
  if (length(iso_objs) == 1 && !iso_is_object(..1) && is.list(..1)) iso_objs <- ..1
  
  if (length(iso_objs) == 0) {
    # empty list
    iso_list <- list()
    all_problems <- get_problems_structure() %>% mutate(file_id = character()) %>% select(file_id, everything())
  } else {
    # check if everything is an iso object
    if(!all(is_iso <- map_lgl(iso_objs, iso_is_object))) {
      stop("can only process iso_file and iso_file_list objects, encountered incompatible data type(s): ",
           unlist(lapply(iso_objs[!is_iso], class)) %>% unique() %>% str_c(collapse = ", "), 
           call. = FALSE)
    }

    # flatten isofiles and isofile lists to make one big isofile list
    iso_list <- map(iso_objs, ~if(iso_is_file_list(.x)) { .x } else { list(.x) }) %>% unlist(recursive = FALSE)
    
    # reset file ids
    names(iso_list) <- map_chr(iso_list, ~.x$file_info$file_id)
    
    # check if al elements are the same data type
    classes <- map_chr(iso_list, ~class(.x)[1]) 
    if (!all(classes == classes[1])) {
      wrong_dt <- classes[classes != classes[1]] %>% unique %>% collapse(", ")
      glue("can only process iso_file objects with the same data type (first: {classes[1]}), encountered: {wrong_dt}") %>% 
        stop(call. = FALSE)
    }
    
    # check for file_id duplicates
    dups <- 
      data_frame(
        idx = 1:length(iso_list),
        file_id = names(iso_list)
      ) %>% 
      group_by(file_id) %>% 
      mutate(n = 1:n(), has_duplicates = any(n > 1)) %>% 
      ungroup() %>% 
      filter(has_duplicates)
    
    # process duplicates
    if (nrow(dups) > 0) {
      msg <- if(discard_duplicates) "duplicate files encountered, only first kept" else "duplicate files kept but with recoded file IDs"
      
      # work on duplicates
      for (i in 1:nrow(dups)) {
        # register warnings
        idx <- dups$idx[i]
        warn <- dups$n[i] == 1 # only show immediate warning for the first duplicate
        iso_list[[idx]] <- register_warning(
          iso_list[[idx]], sprintf("%s: %s", msg, dups$file_id[i]), warn = warn)
        # recode ID if keeping duplicates
        if (!discard_duplicates) {
          recode_id <- sprintf("%s#%d", iso_list[[idx]]$file_info$file_id, dups$n[i])
          iso_list[[idx]]$file_info$file_id <- recode_id
          names(iso_list)[idx] <- recode_id
        }
      }
      
      # finalize duplicates 
      if (discard_duplicates) {
        # discard all but first duplicate
        iso_list[filter(dups, n > 1)$idx] <- NULL
      } 
    }
    
    # propagate problems
    all_problems <- map(iso_list, ~get_problems(.x) %>% mutate(file_id = .x$file_info$file_id)) %>% bind_rows()
  }
  
  # problems
  if (nrow(all_problems)) {
    all_problems <- all_problems %>% 
      unique() %>% # remove duplicate entries
      select(file_id, everything())
  }
  
  # generate structure
  structure(
    iso_list,
    class = c("iso_file_list")
  ) %>% set_problems(all_problems)
}


# Printing ----

#' Isofile printing
#' 
#' Print summary of individual iso_files (dual inlet or continuous flow) or collection of iso_files.
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @rdname iso_printing
#' @export
print.iso_file_list <- function(x, ...) {
  
  # what type of iso files
  if (length(x) == 0) data_type <- "unknown"
  else data_type <- class(x[[1]]) %>% { .[.!="iso_file"][1] } %>% str_replace("_", " ")
  
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
  data_type <- class(x) %>% { .[.!="iso_file"][1] } %>% str_to_title() %>% str_replace("_", " ")
  if (is.na(data_type)) data_type <- "Iso"
  glue("{data_type} iso file '{x$file_info$file_id}': {get_raw_data_info(x)$raw_data}") %>% cat("\n")
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
set_ds_file_path <- function(ds, file_root, file_path, file_id = basename(file_path), file_subpath = NA_character_) {
  if (!iso_is_file(ds)) stop("can only set path for iso_file data structures", call. = FALSE)
  ds$file_info$file_root <- file_root
  ds$file_info$file_path <- file_path
  ds$file_info$file_id <- file_id
  ds$file_info$file_subpath <- file_subpath
  if (!file.exists(get_ds_file_path(ds))) 
    stop("file/folder does not exist: ", file_path, call. = FALSE)
  return(ds)
}

get_ds_file_path <- function(ds) {
  if (is.na(ds$file_info$file_root)) return(ds$file_info$file_path)
  else return(file.path(ds$file_info$file_root, ds$file_info$file_path))
}

# update read options in structure
update_read_options <- function(ds, read_options) {
  # remove read_ prefix in function parameters
  if(!is.list(read_options)) read_options <- as.list(read_options)
  names(read_options) <- names(read_options) %>% str_replace("^read_", "") 
  update <- read_options[names(read_options) %in% names(ds$read_options)]
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

# combine iso_file with other things
#' @export
c.iso_file_list <- function(...) {
  iso_as_file_list(...)
}
