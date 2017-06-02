#' Read isotope data file
#' 
#' Deprecated, use \link{isoread_dual_inlet}, \link{isoread_continuous_flow} and \link{isoread_scan} instead.
#'
#' @param ... original isoread parameters
#' @export
isoread <- function(...) {
  stop(
    "Deprecated, use isoread_dual_inlet(), isoread_continuous_flow() or isoread_scan() instead.",
    call. = FALSE)
}

#' Main function to read isotope data files
#' 
#' This function takes care of extracting basic information about isofiles, dealing with problems and making sure only valid fire formats are processed. This function is not typicaly called directly but indirectly by calling \link{isoread_dual_inlet}, \link{isoread_continuous_flow} and \link{isoread_scan}. It is exported because it can be very useful for testing new file readers.
#' 
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read).
#' @param supported_extensions data frame with supported extensions and corresponding reader functions
#' @param data_structure the basic data structure for the type of isofile
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=info_messages]{turn_info_message_on} and \link[=info_messages]{turn_info_message_off}
#' @param cache whether to cache isofiles and attempt to reload from cache (will only reload if a file was previously read with the same read options and has NOT been modified since)
#' @param ... additional parameters passed to the specific processing functions for the different file extensions
#' @export
isoread_files <- function(paths, supported_extensions, data_structure, ..., quiet = setting("quiet"), cache = TRUE) {

  # quiet
  on_exit_quiet <- update_quiet(quiet)
  on.exit(on_exit_quiet())
  
  # supplied data checks
  col_check(c("extension", "fun"), supported_extensions)
  if(!is(data_structure, "isofile")) stop("data structure must include class 'isofile'", call. = FALSE)
  col_check(c("file_info"), data_structure)
  
  # expand & safety check paths (will error if non-suppored file types are included
  # or same filename occurs multiple times)
  if (missing(paths)) stop("file path(s) required", call. = FALSE)
  filepaths <- retrieve_file_paths(paths, supported_extensions$extension)
  
  # overview
  if (!setting("quiet")) {
    message("Info: preparing to process ", length(filepaths), " data file(s)...")
  }
  
  # extension to reader map
  fun_map <- supported_extensions %>% { setNames(as.list(.$fun), str_c(".", .$extension)) }
  
  # read options update in data structure
  data_structure <- update_read_options(data_structure, ...)
  
  # read files
  isofiles <- list()
  all_problems <- data_frame()
  version_warning <- 0
  for (filepath in filepaths) {
    ext <- get_file_ext(filepath)
    
    # prepare isofile object
    isofile <- data_structure %>% set_ds_file_path(filepath)
    
    # check for cache
    cache_path <- generate_cache_file_path(isofile)
    if (cache && file.exists(cache_path)) {
      
      # file is cached and caching is turned on --> read from cached file
      if (!setting("quiet")) sprintf("Info: restoring file %s from cache", filepath) %>% message()
      rm("isofile") # remove object
      load(cache_path) # load object
      # make sure object in file was loaded properly
      if (!exists("isofile", inherits = FALSE) || !(is(isofile, "isofiles") || is(isofile, "isofile"))) 
        stop("cached file did not contain isofile(s)", call. = FALSE)
      # check for version warning
      cached_version <- if(is(isofile, "isofiles")) isofile[[1]]$version else isofile$version
      if (cached_version != packageVersion("isoreader")) 
        version_warning <- version_warning + 1
      
    } else {
      
      # read file anew using extension-specific function to read file
      if (!setting("quiet")) sprintf("Info: reading file %s with '%s' reader", filepath, ext) %>% message()
      isofile <- exec_func_with_error_catch(fun_map[[ext]], isofile, ...)
      # store in cached file
      if (cache) {
        if (!file.exists(setting("cache_dir"))) dir.create(setting("cache_dir"))
        save(isofile, file = cache_path)
      }
      
    }
    
    # report problems
    if (!setting("quiet") && n_problems(isofile) > 0) {
      cat("Warning: encountered", n_problems(isofile), "problems\n")
      print(problems(isofile))
      cat("\n")
    }
    
    # add to overall files and problems
    if (is(isofile, "isofiles")) {
      # multi file returned, problems already have filenames included
      all_problems <- bind_rows(all_problems, get_problems(isofile))
      isofiles <- c(isofiles, isofile)
    } else {
      # single file: set file_id as name in the list
      isofile_problems <- get_problems(isofile) %>% 
        mutate_(.dots = list(file_id = ~isofile$file_info$file_id))
      all_problems <- bind_rows(all_problems, isofile_problems)
      isofiles <- c(isofiles, setNames(list(isofile), isofile$file_info$file_id))
    }
  }

  # NOTE: consider implementing safety check to make sure that all isofiles that were generated still have the same top-level structure as the data structure originally provided
  
  # version warning check
  if (version_warning > 0) {
    version_problem <- register_warning(
      NA, str_c(version_warning, " of the reloaded cached files were created by a different version of the isoreader package. This may lead to processing problems.\nPlease run the function 'cleanup_isoreader_cache()' once to remove all version mismatched cached files."))
    all_problems <- bind_rows(all_problems, get_problems(version_problem))
  }
  
  if (length(isofiles) == 1) {
    # only one file read
    return(isofiles[[1]])
  } else {
    # multiple files
    class(isofiles) <- c("isofiles", class(isofiles))
    isofiles <- set_problems(isofiles, all_problems %>% { select_(., .dots = c("file_id", names(.))) })

    # check for name duplicates and register a warning if there are any
    if (any(dups <- duplicated(names(isofiles)))) {
      isofiles <- isofiles %>% register_warning(
        sprintf("encountered duplicate file IDs which may interfere with processing aggregated data properly: %s",
                names(isofiles)[dups] %>% str_c(collapse = ", "))
      )
    }
    
    # return all isofiles
    return(isofiles)
  }
}
