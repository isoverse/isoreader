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
  filepath <- get_export_filepath(filepath, ext)
  
  # save isofiles
  if (!quiet) {
    sprintf("Info: exporting data from %d files into R Data Archive '%s'", 
            length(as_isofile_list(isofiles)), 
            str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  }
  
  save(isofiles, file = filepath)
  return(invisible(isofiles))
}


#' Export data to Excel
#' 
#' This function exports the passed in isofiles to Excel. The different kinds of data (raw data, file info, methods info, etc.) are exported to separate tabs within the excel file but they are only exported if the corresponding \code{include_} parameter is set to \code{TRUE} and only for data types for which this type of data is available and was read (see \code{\link{read_dual_inlet}}, \code{\link{read_continuous_flow}} for details on read parameters). 
#' 
#' @inheritParams export_to_rda
#' @param include_raw_data whether to include the raw data in the export (if available)
#' @param include_file_info whether to include the file info in the export (if available)
#' @param include_method_info whether to include methods infor in the export (if available)
#' @param include_vendor_data_table whether to include the vendor data table in the export (if available)
#' @family export functions
#' @return returns the isofiles object invisibly for use in pipelines
#' @export
export_to_excel <- function(isofiles, filepath, 
                            include_raw_data = TRUE, include_file_info = TRUE, include_method_info = TRUE, include_vendor_data_table = TRUE,
                            quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only export iso files or lists of iso files", call. = FALSE)
  else if (is_continuous_flow(isofiles))
    ext <- ".cf.xlsx"
  else if (is_dual_inlet(isofiles))
    ext <- ".di.xlsx"
  else
    stop("Excel export of this type of isofiles not yet supported", call. = FALSE) 
  # note: not sure yet how to best implement different data types such as scan here
  filepath <- get_export_filepath(filepath, ext)
  
  # save isofiles
  export_isofiles <- as_isofile_list(isofiles)
  if (!quiet) {
    sprintf("Info: exporting data from %d files into Excel '%s'", length(export_isofiles), 
            str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  }
  
  # make excel workbook
  wb <- createWorkbook()
  hs <- createStyle(textDecoration = "bold")
  if (include_raw_data) {
    addWorksheet(wb, "raw data")
    writeData(wb, "raw data", aggregate_raw_data(export_isofiles, quiet = TRUE),
              headerStyle = hs)
  }
  if (include_file_info) {
    addWorksheet(wb, "file info")
    writeData(wb, "file info", aggregate_file_info(export_isofiles, quiet = TRUE),
              headerStyle = hs)
  }
  if (include_method_info) {
    addWorksheet(wb, "method info")
    standards <- aggregate_standards_info(export_isofiles, quiet = TRUE)
    resistors <- aggregate_resistors_info(export_isofiles, quiet = TRUE)
    writeData(wb, "method info", standards, headerStyle = hs)
    writeData(wb, "method info", resistors, startRow = nrow(standards) + 3, headerStyle = hs)
  }
  if (include_vendor_data_table) {
    addWorksheet(wb, "vendor data table")
    writeData(wb, "vendor data table", aggregate_vendor_data_table(export_isofiles, quiet = TRUE),
              headerStyle = hs)
  }
  saveWorkbook(wb, filepath, overwrite = TRUE)
  
  return(invisible(isofiles))
}


#' Export to feather
#' 
#' This function exports the passed in isofiles to the Python and R shared feather file format. The different kinds of data (raw data, file info, methods info, etc.) are exported to separate feather files that are saved with the provided \code{filepath_prefix} as prefix. All are only exported if the corresponding \code{include_} parameter is set to \code{TRUE} and only for data types for which this type of data is available and was read (see \code{\link{read_dual_inlet}}, \code{\link{read_continuous_flow}} for details on read parameters). 
#' 
#' @inheritParams export_to_excel
#' @param filepath_prefix the path (folder and filename) prefix for the exported feather files. The correct suffix for different kinds of data and file extension is automatically added
#' @family export functions
#' @return returns the isofiles object invisibly for use in pipelines
#' @export
export_to_feather <- function(isofiles, filepath_prefix, 
                              include_raw_data = TRUE, include_file_info = TRUE, include_method_info = TRUE, include_vendor_data_table = TRUE,
                              quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only export iso files or lists of iso files", call. = FALSE)
  else if (is_continuous_flow(isofiles))
    ext <- ".cf.feather"
  else if (is_dual_inlet(isofiles))
    ext <- ".di.feather"
  else
    stop("Feather export of this type of isofiles not yet supported", call. = FALSE) 
  # note: not sure yet how to best implement different data types such as scan here
  
  # save isofiles
  filepath <- get_export_filepath(filepath_prefix, NULL)
  if (!quiet) {
    sprintf("Info: exporting data from %d files into %s files at '%s'", length(as_isofile_list(isofiles)), 
            ext, str_replace(filepath, "^\\.(/|\\\\)", "")) %>% message()
  }
  
  # make feather files in temporary dir
  if (include_raw_data) 
    write_feather(aggregate_raw_data(isofiles, quiet = TRUE), str_c(filepath, "_raw_data", ext))
  
  if (include_file_info) 
    write_feather(aggregate_file_info(isofiles, quiet = TRUE), str_c(filepath, "_file_info", ext))
  
  if (include_method_info) {
    write_feather(aggregate_standards_info(isofiles, quiet = TRUE), str_c(filepath, "_method_info-standards", ext))
    write_feather(aggregate_resistors_info(isofiles, quiet = TRUE), str_c(filepath, "_method_info-resistors", ext))
  }
  
  if (include_vendor_data_table) 
    write_feather(aggregate_vendor_data_table(isofiles, quiet = TRUE), str_c(filepath, "_vendor_data_table", ext))
  
  return(invisible(isofiles))
}

# convenience function for export file paths (extension checks and addition)
get_export_filepath <- function(filepath, ext) {
  # file name and folder
  if (missing(filepath)) stop("no filepath provided", call. = FALSE)
  filename <- basename(filepath)
  folder <- dirname(filepath)
  if (!file.exists(folder)) stop("the folder '", folder, "' does not exist", call. = FALSE)
  if (!is.null(ext))
    filename <- filename %>% str_replace(fixed(ext), "") %>% str_c(ext) # to make sure correct extension
  return(file.path(folder, filename)) 
}
