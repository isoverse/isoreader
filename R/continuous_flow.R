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

# get supported continuous flow file types and which functions they map to, whether they can be cached (if caching requested), etc.
get_supported_cf_files <- function() {
  tribble(
    ~id,     ~extension,    ~fun,                ~cache,   ~description,
    "cf",    "cf",          "isoread_cf",        TRUE,     "Isodat Continuous Flow file format (older)",
    "dxf",   "dxf",         "isoread_dxf",       TRUE,     "Isodat Continuous Flow file format (newer)",
    "iarc",  "iarc",        "isoread_flow_iarc", TRUE,     "IonOS Continous Flow data archieve",
    #"zip",  "feather.zip", "isoread_feather",   FALSE,    "Isoreader cached data format (for R+python)", # potential future expansion
    "Rda",   "cf.Rda",      "isoread_rda",       FALSE,    "Isoreader cached data (for R)"
  )
}