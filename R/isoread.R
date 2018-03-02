#' Read isotope data file
#' 
#' This function from the original isoread package is deprecated, please use \link{iso_read_dual_inlet}, \link{iso_read_continuous_flow} and \link{iso_read_scan} instead.
#'
#' @param ... original isoread parameters
#' @export
isoread <- function(...) {
  stop(
    "Deprecated, use iso_read_dual_inlet(), iso_read_continuous_flow() or iso_read_scan() instead.",
    call. = FALSE)
}

#' Core function to read isotope data files
#' 
#' This function takes care of extracting basic information about iso_files, dealing with problems and making sure only valid fire formats are processed. 
#' This function is not typicaly called directly but indirectly by calling \link{iso_read_dual_inlet}, \link{iso_read_continuous_flow} and \link{iso_read_scan}. 
#' It is made available outside the package because it can be very useful for testing new file readers.
#' 
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read).
#' @param supported_extensions data frame with supported extensions and corresponding reader functions
#' @param data_structure the basic data structure for the type of iso_file
#' @param discard_duplicates whether to automatically discard duplicate file_ids (only first one is kept)
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=iso_info_messages]{iso_turn_info_message_on} and \link[=iso_info_messages]{iso_turn_info_message_off}
#' @param cache whether to cache iso_files. Note that previously exported R Data Archives (di.rda, cf.rda) are never cached since they are already essentially in cached form.
#' @param read_cache whether to reload from cache if a cached version exists. Note that it will only read from cache if the file was previously read with the exact same isoreader version and read options and has not been modified since.
#' @param ... additional parameters passed to the specific processing functions for the different file extensions
#' @return single iso_file object (if single file) or list of iso_files (iso_file_list)
iso_read_files <- function(paths, supported_extensions, data_structure, ..., discard_duplicates = TRUE, cache = default(cache), read_cache = default(cache), quiet = default(quiet)) {

  # set quiet for the current and sub-calls and reset back to previous setting on exit
  on_exit_quiet <- update_quiet(quiet)
  on.exit(on_exit_quiet())
  
  # supplied data checks
  col_check(c("extension", "fun"), supported_extensions)
  if(!is(data_structure, "iso_file")) stop("data structure must include class 'iso_file'", call. = FALSE)
  col_check(c("file_info"), data_structure)
  
  # global vars
  filepath <- ext <- NULL
  
  # read options update in data structure
  data_structure <- update_read_options(data_structure, ...)
  
  # expand & safety check paths (will error if non-suppored file types are included or same filename occurs multiple times)
  if (missing(paths) || is.null(paths) || is.na(paths)) stop("file path(s) required", call. = FALSE)
  filepaths <- expand_file_paths(paths, supported_extensions$extension)
  
  # generate read files overview
  files <- 
    data_frame(
      filepath = filepaths,
      cachepath = generate_cache_filepaths(filepath, data_structure$read_options),
      ext = get_file_ext(filepath),
      file_n = 1:length(filepaths)
    )
  
  # extension to reader map & safety checks
  ext_fun_map <- supported_extensions %>% { setNames(.$fun, str_c(".", .$id)) }
  ext_cache_map <- supported_extensions %>% { setNames(.$cache, str_c(".", .$id)) }
  
  if ( length(missing_readers <- setdiff(files$ext, names(ext_fun_map))) > 0)
    stop("unknown file id(s), cannot find reader function(s): ", str_c(missing_readers, collapse = ", "), call. = FALSE)
  
  files <- files %>% 
    mutate(
      reader_fun = ext_fun_map[ext],
      cacheable = ext_cache_map[ext]
    )
  
  # overview
  if (!default(quiet)) {
    message("Info: preparing to read ", nrow(files), " data file(s)...")
  }
  
  # read function
  read_iso_file <- function(filepath, cachepath, ext, reader_fun, cacheable, file_n) {
    
    # prepare iso_file object
    iso_file <- set_ds_file_path(data_structure, filepath)
    
    # evaluate read file event quosure if it exists
    read_file_event <- getOption("isoreader.read_file_event")
    if (!is.null(read_file_event) && is_quosure(read_file_event) && !quo_is_null(read_file_event)) {
      eval_tidy(get_expr(read_file_event))
    }
    
    # check for cache
    if (read_cache && cacheable && file.exists(cachepath)) {
      ## cache available  
      if (!default(quiet)) {
        glue("Info: reading file {file_n}/{nrow(files)} '{filepath}' from cache...") %>% message()
      }
      iso_file <- load_cached_iso_file(cachepath)
    } else {
      ## read file anew using extension-specific function to read file
      caching <- if (cache && cacheable) " and caching" else ""
      if (!default(quiet)) {
        glue("Info: reading{caching} file {file_n}/{nrow(files)} '{filepath}' with '{ext}' reader...") %>% message()
      }
      iso_file <- exec_func_with_error_catch(reader_fun, iso_file, ...)
      
      # cleanup any binary content depending on debug setting
      if (!default(debug)) iso_file$binary <- NULL
      
      # store in cached file
      if (cache && cacheable) cache_iso_file(iso_file, cachepath)
    }
    
    return(list(iso_file))
  }
  
  # read files
  iso_files <-
    with(
      files,
      mapply(
        read_iso_file,
        filepath = filepath,
        cachepath = cachepath,
        ext = ext,
        reader_fun = reader_fun,
        cacheable = cacheable,
        file_n = file_n
      )
    )
  
  # turn into iso_file list
  iso_files <- iso_as_file_list(unname(iso_files), discard_duplicates = discard_duplicates) 

  # report problems
  if (!default(quiet) && iso_has_problems(iso_files)) {
    message(sprintf("Info: encountered %.0f problems in total.", n_problems(iso_files)))
    print(problems(iso_files))
    cat("\n")
  }
  
  # return single or file or list
  if (length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

#' Re-read iso_files
#' 
#' Sometimes it is useful to reload isotope files from their original data files (e.g. after upgrading to a newer version of the isoreader package). 
#' The functions described below are intended to make this very easy. 
#' However, it is only possible for iso_file objects whose file paths still point to the original raw data files.
#' 
#' @details \code{iso_reread_files} will re-read all the original data files for the passed in \code{iso_files} object. Returns the reread iso_file objects.
#' @inheritParams iso_get_raw_data
#' @param ... additional read parameters that should be used for re-reading the iso_files, see \code{\link{iso_read_dual_inlet}} and \code{\link{iso_read_continuous_flow}} for details
#' @param stop_if_missing whether to stop re-reading if any of the original data files are missing (if FALSE, will warn about the missing files adding a warning to them, but also re-read those that do exist)
#' @note re-reading files with their original read parameters is not yet supported
#' @export
iso_reread_files <- function(iso_files, ..., stop_if_missing = FALSE, quiet = default(quiet)) {
  
  # checks
  if(!iso_is_object(iso_files)) stop("can only re-read iso_files", call. = FALSE)
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)
  
  # reread
  filepaths <- get_reread_filepaths(iso_files)
  files_exist <- filepaths %>% map_lgl(file.exists)
  
  # overview
  if (!default(quiet)) {
    message("Info: re-reading ", length(filepaths), " data file(s)...")
  }
  
  # safety check for non existent data files
  if (!all(files_exist)) {
    msg <- sprintf("%d file(s) do no longer exist at the referenced location and can not be re-read:\n - %s\n",
                   sum(!files_exist), str_c(filepaths[!files_exist], collapse = "\n - "))
    if (stop_if_missing) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE, immediate. = TRUE)
      iso_files[!files_exist] <- map(iso_files[!files_exist], register_warning, func = "iso_reread_files", 
                                     details = "file does not exist at its original location and can not be re-read",
                                     warn = FALSE)
      
    }
  }
  
  # reread files
  # @FIXME: if no read parameters are supplied, re-read with the original ones
  if (any(files_exist)) {
    args <- c(list(paths = filepaths[files_exist]), list(...))
    if (iso_is_continuous_flow(iso_files)) {
      # read continuous flow
      new_iso_files <- iso_as_file_list(do.call(iso_read_continuous_flow, args = args))
    } else if (iso_is_dual_inlet(iso_files)) {
      # read dual inlet
      new_iso_files <- iso_as_file_list(do.call(iso_read_dual_inlet, args = args))
    } else {
      stop("re-reading iso_files objects of type ", class(iso_files[[1]])[1], " is not yet supported", call. = FALSE)
    }
    
    # replace the ones that were re-read (and add new files in case there were any e.g. from updated iarc archives)
    overlap_ids <- names(iso_files)[names(iso_files) %in% names(new_iso_files)]
    new_ids <- names(new_iso_files)[!names(new_iso_files) %in% names(iso_files)]
    for(id in overlap_ids) iso_files[[id]] <- new_iso_files[[id]]
    if (length(new_ids) > 0) iso_files <- c(iso_files, new_iso_files[new_ids])
  }
  
  # return single (if passed in as single) 
  if (single_file && length(iso_files) == 1) return (iso_files[[1]])
  return(iso_files)
}

#' @details \code{iso_reread_archive} is a convenience function for refreshing saved iso_file collections. It will load a specific iso_files R Data Archive (\code{rda_filepath}), re-read all the data from the original data files and save the collection back to the same rda file. The iso_files are returned invisibly.
#' @rdname iso_reread_files
#' @param rda_filepaths the path(s) to the iso_files R data archive(s) to re-read (can be a single file or vector of files)
#' @export
iso_reread_archive <- function(rda_filepaths, ..., stop_if_missing = FALSE, quiet = default(quiet)) {
  
  file_types <- match_to_supported_file_types(rda_filepaths)
  
  # global vars
  extension <- NULL
  
  if (nrow(missing <- filter(file_types, is.na(extension))) > 0)
    stop("unrecognized file type(s): ", str_c(missing$filename, collapse = ", "), call. = FALSE)
  
  if (any(missing <- !file.exists(rda_filepaths))) 
    stop("file(s) do not exist: ", str_c(rda_filepaths[missing], collapse = ", "), call. = FALSE)
  
  reread_archive <- function(filepath, call) {
    if(!quiet) message("Info: loading R Data Archive ", basename(filepath), "...")
    suppressWarnings(do.call(call, args = list(paths = filepath, quiet=TRUE))) %>% 
      iso_reread_files(..., stop_if_missing = stop_if_missing, quiet=quiet) %>% 
      iso_export_to_rda(filepath = filepath, quiet=quiet)
    return(TRUE)
  }
  
  # note: cannot combine these in case some of them are dual inlet while others are continuous flow
  with(file_types, mapply(reread_archive, filepath, call))
  invisible(NULL)
}


