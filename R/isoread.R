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
#' @param read_mass_data whether to read the raw mass data from the file
#' @param read_data_table whether to read an preprocessed data tables from the file
#' @param read_file_info whether to read auxiliary file information (program, methods, etc.)
isoread_files <- function(paths, supported_extensions, data_structure, quiet = setting("quiet"),
                          read_mass_data = TRUE, read_data_table = TRUE, read_file_info = TRUE) {
  
  # supplied data checks
  col_check(c("extension", "fun"), supported_extensions)
  if(!is(data_structure, "isofile")) stop("data structure must include class 'isofile'", call. = FALSE)
  col_check(c("file_info"), data_structure)
  
  # expand & safety check paths (will error if non-suppored file types are included
  # or same filename occurs multiple times)
  if (missing(paths)) stop("file path(s) required", call. = FALSE)
  filepaths <- retrieve_file_paths(paths, supported_extensions$extension)
  
  # extension to reader map
  fun_map <- supported_extensions %>% { setNames(as.list(.$fun), str_c(".", .$extension)) }
  
  # read files
  isofiles <- list()
  all_problems <- data_frame()
  for (filepath in filepaths) {
    ext <- get_file_ext(filepath)
    if (!setting("quiet")) sprintf("Info: reading file %s with '%s' reader", filepath, ext) %>% message()
    
    # prepare isofile object
    isofile <- data_structure %>% set_ds_file_path(filepath)
    
    # use extension-specific function to read file
    isofile <- exec_func_with_error_catch(fun_map[[ext]], isofile)
    
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
        mutate(file_id = isofile$file_info$file_id) 
      all_problems <- bind_rows(all_problems, isofile_problems)
      isofiles <- c(isofiles, setNames(list(isofile), isofile$file_info$file_id))
    }
  }

  if (length(isofiles) == 1) {
    # only one file read
    return(isofiles[[1]])
  } else {
    # multiple files
    class(isofiles) <- c("isofiles", class(isofiles))
    isofiles <- set_problems(isofiles, select(all_problems, file_id, everything()))

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
