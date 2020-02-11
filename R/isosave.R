## Save functions

#' Save data to R Data Storage (.rds)
#' 
#' This function saves the passed in iso_files to an R Data Storage (.rds) file, which is an efficient compressed data storage format. Data exported this way can be easily read back into isoreader using the standard \code{\link{iso_read_continuous_flow}} and \code{\link{iso_read_dual_inlet}} functions.
#' 
#' @inheritParams iso_get_raw_data
#' @param filepath the path (folder and filename) to the export file. The correct file extension is automatically added if not already in the filename, i.e. filename can be provided with or without extension.
#' @family export functions
#' @return returns the iso_files object invisibly for use in pipelines
#' @export
iso_save <- function(iso_files, filepath, quiet = default(quiet)) {
  
  # safety checks
  if(!iso_is_object(iso_files)) stop("can only export iso files or lists of iso files", call. = FALSE)
  filepath <- get_rds_export_filepath(iso_files, filepath)
  
  # save iso_files
  if (!quiet) {
    sprintf("Info: exporting data from %d iso_files into R Data Storage '%s'", 
            length(iso_as_file_list(iso_files)), 
            str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  }
  
  saveRDS(iso_files, file = filepath)
  return(invisible(iso_files))
}

# utility functions ====

# rds export filepath
get_rds_export_filepath <- function(iso_files, filepath) {
  if (iso_is_continuous_flow(iso_files))
    ext <- ".cf.rds"
  else if (iso_is_dual_inlet(iso_files))
    ext <- ".di.rds"
  else if (iso_is_scan(iso_files))
    ext <- ".scan.rds"
  else
    stop("R data storage export of this type of iso_files not supported", call. = FALSE)
  return(get_export_filepath(filepath, ext))
}
