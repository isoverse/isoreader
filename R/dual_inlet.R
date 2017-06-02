#' Load dual inlet data
#' 
#' @inheritParams isoread_files
#' @param read_raw_data whether to read the raw mass/ion data from the file
#' @param read_file_info whether to read auxiliary file information (program, methods, etc.)
#' @family isoread functions for different types of IRMS data
#' @export
isoread_dual_inlet <- function(
  paths, read_raw_data = TRUE, read_file_info = TRUE,
  quiet = setting("quiet"), cache = TRUE) {
  
  isoread_files(
    paths,
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure(),
    read_raw_data = read_raw_data,
    read_file_info = read_file_info,
    quiet = quiet,
    cache = cache
  )
}

# get supported dual inlet file types and which functions they map to
get_supported_di_files <- function() {
  tribble(
    ~extension,    ~fun,              ~description,
    "did",         "isoread_did",     "Isodat Dual Inlet file format",
    "feather.zip", "isoread_feather", "Isoreader cached data format (for R+python)"
  )
}
