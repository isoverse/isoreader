# general file utils

# generic function to try to get file creation date from OS file info
# this does NOT always work, retrieving file creation info from inside a file
# is always preferable if this information is available
extract_os_file_creation_datetime <- function(ds) {
  
  # full path to file
  path <- get_ds_file_path(ds)
  
  # platform dependent createion time
  if (.Platform$OS.type == "windows") {
    # original datetime (doesn't work on unix, just gives last modification date)
    ds$file_info$file_datetime <- as_datetime(file.info(path)$ctime, tz = Sys.timezone())
  } else if (.Platform$OS.type == "unix") {
    # Linux and OS X, try it with stat, otherwise just throw a warning and use mdate
    
    # m date (meaning BSD stat didn't work)
    get_creation_date <- function(ds) {
      # last modification time the only info that's available
      if (default("datetime_warnings")) {
        # report warning if requested
        ds <- ds %>% register_warning(
          paste0(
            "file creation date cannot be accessed on this Linux system, using last modified time for file_datetime instead"
          ), 
          func = "extract_os_file_creation_datetime",
          warn = FALSE
        )
      }
      ds$file_info$file_datetime <- as_datetime(file.info(path)$mtime, tz = Sys.timezone())
      return(ds)
    }
    
    # use BSD stat
    ds <- 
      tryCatch({
        cmd <- paste0('stat -f "%DB" "', path, '"') # use BSD stat command
        ds$file_info$file_datetime <- 
          # retrieve birth date in seconds from start of epoch (%DB)
          system(cmd, intern=TRUE, ignore.stderr = TRUE) %>% as.integer() %>% 
          # convert to POSIXct
          as.POSIXct(origin = "1970-01-01", tz = "") %>% 
          # force local timezone
          as_datetime(tz = Sys.timezone())
        ds
      }, 
      error = function(e) { return(get_creation_date(ds)) }, 
      warning = function(e) { return(get_creation_date(ds)) }) 
  } else { 
    stop("don't know how to get file creation date on platform ", .Platform$OS.type)
  }
  return(ds)
}