## Export functions

#' Export data to R Data Archive (.rda)
#' 
#' This function exports the passed in isofiles to an R Data Archive (.rda) file, which is a fairly efficient compressed data storage format. Data exported this way can be easily read back into isoreader using the standard \code{\link{read_continuous_flow}} and \code{\link{read_dual_inlet}} functions or by simply double-clicking the .rda file which will open in an R console and restore the data into variable \code{isofiles}.
#' 
#' @inheritParams aggregate_raw_data
#' @param filepath the path (folder and filename) to the export file. The correct file extension is automatically added if not already in the filename, i.e. filename can be provided with or without extension.
#' @family export functions
#' @return returns the isofiles object invisibly for use in pipelines
#' @export
export_to_rda <- function(isofiles, filepath, quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only export iso files or lists of iso files", call. = FALSE)
  else if (is_continuous_flow(isofiles))
    ext <- ".cf.rda"
  else if (is_dual_inlet(isofiles))
    ext <- ".di.rda"
  else
    stop("R data archive export of this type of isofiles not supported", call. = FALSE) 
  
  # file name and folder
  if (missing(filepath)) stop("no filepath provided", call. = FALSE)
  filename <- basename(filepath)
  folder <- dirname(filepath)
  if (!file.exists(folder)) stop("the folder '", folder, "' does not exist", call. = FALSE)
  
  # save isofiles
  isofiles <- as_isofile_list(isofiles)
  filename_w_ext <- filename %>% str_replace(fixed(ext), "") %>% str_c(ext) # to make sure correct extension
  filepath <- file.path(folder, filename_w_ext)
  if (!setting("quiet")) 
    sprintf("Info: exporting data from %d files into '%s'", length(isofiles), str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  save(isofiles, file = filepath)
  return(invisible(isofiles))
}


#' Export data to Excel
#' 
#' Convenience function for exporting isoreader data to Excel.
#' 
#' @inheritParams export_to_rda
#' @param filename the name of the file to export (without the Rda ending)
#' @param folder the folder where to save the rda file
#' @family export functions
#' @return returns the isofiles object invisibly for use in pipelines
#' @export
export_to_excel <- function(isofiles, filenname, folder = ".", quiet = setting("quiet")) {
  
}