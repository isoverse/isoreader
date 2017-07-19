#' Load continuous flow data
#' 
#' @inheritParams isoread_dual_inlet
#' @family isoread functions for different types of IRMS data
#' @export
isoread_continuous_flow <- function(
  paths, read_raw_data = TRUE, read_file_info = TRUE, read_method_info = TRUE, read_vendor_data_table = FALSE,
  discard_duplicates = TRUE, quiet = setting("quiet"), cache = setting("cache")) {

  # process data
  isoread_files(
    paths,
    supported_extensions = get_supported_cf_files(),
    data_structure = make_cf_data_structure(),
    read_raw_data = read_raw_data,
    read_file_info = read_file_info,
    read_method_info = read_method_info,
    read_vendor_data_table = read_vendor_data_table,
    discard_duplicates = discard_duplicates,
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