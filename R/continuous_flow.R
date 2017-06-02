#' Load continuous flow data
#' 
#' @inheritParams isoread_files
#' @param read_raw_data whether to read the raw mass/ion data from the file
#' @param read_file_info whether to read auxiliary file information (program, methods, etc.)
#' @param read_data_table whether to attempt to read any preprocessed data tables from the file
#' @family isoread functions for different types of IRMS data
#' @export
isoread_continuous_flow <- function(
  paths, read_raw_data = TRUE, read_file_info = TRUE, read_data_table = TRUE,
  quiet = setting("quiet"), cache = TRUE) {

  # process data
  isoread_files(
    paths,
    supported_extensions = get_supported_cf_files(),
    data_structure = make_cf_data_structure(),
    read_raw_data = read_raw_data,
    read_file_info = read_file_info,
    read_data_table = read_data_table,
    quiet = quiet,
    cache = cache
  )
}

# get supported continuous flow file types and which functions they map to
get_supported_cf_files <- function() {
  tribble(
    ~extension,    ~fun,                ~description,
    "cf",          "isoread_cf",        "Isodat Continuous Flow file format (older)",
    "dxf",         "isoread_dxf",       "Isodat Continuous Flow file format (newer)",
    "iarc",        "isoread_flow_iarc", "IonOS Continous Flow data archieve",
    "feather.zip", "isoread_feather",   "Isoreader cached data format (for R+python)"
  )
}