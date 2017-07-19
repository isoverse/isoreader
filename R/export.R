## Export functions

#' Export data to R Data Archive
#' 
#' @inheritParams isoplot_raw_data
#' @param filename the name of the file to export (without the Rda ending)
#' @param folder the folder where to save the Rda file
#' @family export functions
#' @return returns the isofiles object invisibly for use in pipelines
#' @export
isoexport_rda <- function(isofiles, filename, folder = ".", quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only export iso files or lists of iso files", call. = FALSE)
  else if (is_continuous_flow(isofiles))
    ext <- ".cf.Rda"
  else if (is_dual_inlet(isofiles))
    ext <- ".di.Rda"
  else
    stop("R data archive export of this type of isofiles not supported", call. = FALSE) 
  if (missing(filename)) stop("no filename provided", call. = FALSE)
  if (!file.exists(folder)) stop("the provided folder '", folder, "' does not exist", call. = FALSE)
  
  # save isofiles
  isofiles <- as_isofile_list(isofiles)
  filename_w_ext <- filename %>% str_replace(fixed(ext), "") %>% str_c(ext) # to make sure correct extension
  filepath <- file.path(folder, filename_w_ext)
  if (!setting("quiet")) 
    sprintf("Info: exporting data from %d files into '%s'", length(isofiles), str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  save(isofiles, file = filepath)
  return(invisible(isofiles))
}