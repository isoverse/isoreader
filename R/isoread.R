#' Read isotope data file
#' 
#' Deprecated, use \link{read_dual_inlet}, \link{read_continuous_flow} and \link{read_scan} instead.
#'
#' @param ... original isoread parameters
#' @export
isoread <- function(...) {
  stop(
    "Deprecated, use read_dual_inlet(), read_continuous_flow() or read_scan() instead.",
    call. = FALSE)
}

#' Main function to read isotope data files
#' 
#' This function takes care of extracting basic information about isofiles, dealing with problems and making sure only valid fire formats are processed. This function is not typicaly called directly but indirectly by calling \link{read_dual_inlet}, \link{read_continuous_flow} and \link{read_scan}. It is exported because it can be very useful for testing new file readers.
#' 
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read).
#' @param supported_extensions data frame with supported extensions and corresponding reader functions
#' @param data_structure the basic data structure for the type of isofile
#' @param discard_duplicates whether to automatically discard duplicate file_ids (only first one is kept)
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=info_messages]{turn_info_message_on} and \link[=info_messages]{turn_info_message_off}
#' @param cache whether to cache isofiles. Note that previously exported R Data Archives (di.rda, cf.rda) are never cached since they are already essentially in cached form.
#' @param read_cache whether to reload from cache if a cached version exists. Note that it will only read from cache if the file was previously read with the exact same isoreader version and read options and has not been modified since.
#' @param ... additional parameters passed to the specific processing functions for the different file extensions
#' @return single isofile object (if single file) or list of isofiles (isofile_list)
#' @export
isoread_files <- function(paths, supported_extensions, data_structure, ..., discard_duplicates = TRUE, cache = setting("cache"), read_cache = setting("cache"), quiet = setting("quiet")) {

  # set quiet for the current and sub-calls and reset back to previous setting on exit
  on_exit_quiet <- update_quiet(quiet)
  on.exit(on_exit_quiet())
  
  # supplied data checks
  col_check(c("extension", "fun"), supported_extensions)
  if(!is(data_structure, "isofile")) stop("data structure must include class 'isofile'", call. = FALSE)
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
      ext = get_file_ext(filepath)
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
  if (!setting("quiet")) {
    message("Info: preparing to read ", nrow(files), " data file(s)...")
  }
  
  # read function
  read_isofile <- function(filepath, cachepath, ext, reader_fun, cacheable) {
    
    # prepare isofile object
    isofile <- set_ds_file_path(data_structure, filepath)
    
    # check for cache
    if (read_cache && cacheable && file.exists(cachepath)) {
      ## cache available  
      if (!setting("quiet")) sprintf("Info: reading file %s from cache", filepath) %>% message()
      isofile <- load_cached_isofile(cachepath)
    } else {
      ## read file anew using extension-specific function to read file
      caching <- if (cache && cacheable) " and caching" else ""
      if (!setting("quiet")) sprintf("Info: reading%s file %s with '%s' reader", caching, filepath, ext) %>% message()
      isofile <- exec_func_with_error_catch(reader_fun, isofile, ...)
      
      # cleanup any binary content depending on debug setting
      if (!setting("debug")) isofile$binary <- NULL
      
      # store in cached file
      if (cache && cacheable) cache_isofile(isofile, cachepath)
    }
    
    return(list(isofile))
  }
  
  # read files
  isofiles <- with(files, mapply(read_isofile, filepath = filepath, cachepath = cachepath, ext = ext, reader_fun = reader_fun, cacheable = cacheable))
  
  # turn into isofile list
  isofiles <- as_isofile_list(unname(isofiles), discard_duplicates = discard_duplicates) 

  # report problems
  if (!setting("quiet") && n_problems(isofiles) > 0) {
    message(sprintf("Info: encountered %.0f problems in total.", n_problems(isofiles)))
    print(problems(isofiles))
    cat("\n")
  }
  
  # return single or file or list
  if (length(isofiles) == 1) return (isofiles[[1]])
  return(isofiles)
}

#' Re-read isofiles
#' 
#' Sometimes it is useful to reload isotope files from their original data files (e.g. after upgrading to a newer version of the isoreader package). 
#' The functions described below are intended to make this very easy. 
#' However, it is only possible for isofile objects whose file paths still point to the original raw data files.
#' 
#' @details \code{reread_isofiles} will re-read all the original data files for the passed in \code{isofiles} object
#' @inheritParams aggregate_raw_data
#' @param ... additional read parameters that should be used for re-reading the isofiles, see \code{\link{read_dual_inlet}} and \code{\link{read_continuous_flow}} for details
#' @param stop_if_missing whether to stop re-reading if any of the original data files are missing (if FALSE, will warn about the missing files and keep them unchanged but re-read those that do exist)
#' @return isofiles object
#' @export
reread_isofiles <- function(isofiles, ..., stop_if_missing = FALSE, quiet = setting("quiet")) {
  
  # checks
  if(!is_iso_object(isofiles)) stop("can only re-read isofiles", call. = FALSE)
  single_file <- is_isofile(isofiles) # to make sure return is the same as supplied
  isofiles <- as_isofile_list(isofiles)
  
  # reread
  filepaths <- get_reread_filepaths(isofiles)
  files_exist <- filepaths %>% map_lgl(file.exists)
  
  # overview
  if (!setting("quiet")) {
    message("Info: re-reading ", length(filepaths), " data file(s)...")
  }
  
  # safety check for non existent data files
  if (!all(files_exist)) {
    msg <- sprintf("%d file(s) do no longer exist at the referenced location and can not be re-read:\n - %s\n",
                   sum(!files_exist), str_c(filepaths[!files_exist], collapse = "\n - "))
    if (stop_if_missing)
      stop(msg, call. = FALSE)
    else 
      warning(msg, call. = FALSE, immediate. = TRUE)
  }
  
  # reread files
  if (any(files_exist)) {
    args <- c(list(paths = filepaths[files_exist]), list(...))
    if (is_continuous_flow(isofiles)) {
      # read continuous flow
      new_isofiles <- do.call(read_continuous_flow, args = args)
    } else if (is_dual_inlet(isofiles)) {
      # read dual inlet
      new_isofiles <- do.call(read_dual_inlet, args = args)
    } else {
      stop("re-reading isofiles objects of type ", class(isofiles[[1]])[1], " is not yet supported", call. = FALSE)
    }
    
    # replace the ones that were re-read (and add new files in case there were any e.g. from updated iarc archives)
    overlap_ids <- names(isofiles)[names(isofiles) %in% names(new_isofiles)]
    new_ids <- names(new_isofiles)[!names(new_isofiles) %in% names(isofiles)]
    for(id in overlap_ids) isofiles[[id]] <- new_isofiles[[id]]
    if (length(new_ids) > 0) isofiles <- c(isofiles, new_isofiles[new_ids])
  }
  
  # return single (if passed in as single) 
  if (single_file && length(isofiles) == 1) return (isofiles[[1]])
  return(isofiles)
}




