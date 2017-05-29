# main controller function to read binary isotope data files
# @param supported_extensions data frame with the supported extensions and corresponding reader functions
# @param data_structure the basic data structure for the type of isofile
# @param quiet standard quiet parameter
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
    if (!quiet) sprintf("Info: Reading file %s with '%s' reader", filepath, ext)
    
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

