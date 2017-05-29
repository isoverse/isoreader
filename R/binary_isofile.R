#' Main function to read binary isotope data files
#' 
#' This function takes care of extracting basic information about isofiles, making sure only valid fire formats are processed, and reading and processing the binary files themselves. This function is not typicaly called directly but indirectly by calling \link{isoread_dual_inlet}, \link{isoread_continuous_flow} and \link{isoread_scan}. It is exported because it can be very useful for testing new file readers.
#' 
#' @param paths one or multiple file/folder paths. All files must have a supported file extension. All folders are expanded and searched for files with supported file extensions (which are then included in the read).
#' @param supported_extensions data frame with supported extensions and corresponding reader functions
#' @param data_structure the basic data structure for the type of isofile
#' @param quiet whether to display (quiet=FALSE) or silence (quiet = TRUE) information messages. Set parameter to overwrite global defaults for this function or set global defaults with calls to \link[=info_messages]{turn_info_message_on} and \link[=info_messages]{turn_info_message_off}
isoread_files <- function(paths, supported_extensions, data_structure, quiet = default("quiet")) {
  
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
  isofiles <- sapply(filepaths, function(filepath) {
    ext <- get_file_ext(filepath)
    if (!quiet) sprintf("Info: Reading file %s with '%s' reader", filepath, ext) %>% message()
    
    # prepare isofile object
    isofile <- data_structure
    isofile <- 
      # processes not chained to make sure errors are caught during each step
      tryCatch({
        # initialize problems attribute
        isofile <- initialize_problems_attribute(isofile)
        # store filename and path
        isofile$file_info$file_name <- basename(filepath)
        isofile$file_info$file_path <- filepath
        # use extension-specific function to read file
        isofile <- fun_map[[ext]](isofile)
        # return isofile
        isofile
      }, error = function(e){
        warning(e$message)
        isofile <- register_problem(isofile, filename = basename(filepath), 
                                    type = "uncaught error", message = e$message)
        return(isofile)
      })
    
    # report problems
    if (!quiet && n_problems(isofile) > 0) {
      cat("Encountered problems:\n")
      print(problems(isofile))
      cat("\n")
    }
    
    return(list(isofile) %>% 
             # set filepath as list name
             setNames(basename(filepath)))
  })

  if (length(isofiles) == 1) {
    # only one file read
    return(isofiles[[1]])
  } else {
    # multiple files
    class(isofiles) <- c("isofiles", class(isofiles))
    # combine all problems and also store in parent isofiles
    attr(isofiles, "problems") <-
      isofiles %>% 
      lapply(function(isofile) { 
        problems(isofile) %>% mutate(filename = isofile$file_info$file_name) 
      }) %>% bind_rows() %>% 
      { select_(., .dots = c("filename", names(.)[names(.)!="filename"])) }
  }
  
  return(isofiles)  
}


#' Print a collection of isofiles
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @export
print.isofiles <- function(x, ...) {
  sprintf("# data from %d isofiles:\n", length(x)) %>% 
    cat()
  sapply(x, print)
}

