#' Load continuous flow data
#' 
#' @inheritParams iso_read_dual_inlet
#' @family isoread functions for different types of IRMS data
#' @export
iso_read_continuous_flow <- function(
  ..., 
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), 
  read_method_info = default(read_method_info), read_vendor_data_table = default(read_vendor_data_table),
  discard_duplicates = TRUE, cache = default(cache), read_cache = default(cache), quiet = default(quiet)) {

  # process data
  iso_read_files(
    unlist(list(...), use.names = FALSE),
    supported_extensions = get_supported_cf_files(),
    data_structure = make_cf_data_structure(),
    read_raw_data = read_raw_data,
    read_file_info = read_file_info,
    read_method_info = read_method_info,
    read_vendor_data_table = read_vendor_data_table,
    discard_duplicates = discard_duplicates,
    cache = cache,
    read_cache = read_cache,
    quiet = quiet
  )
}

# get supported continuous flow file types and which functions they map to, whether they can be cached (if caching requested), etc.
get_supported_cf_files <- function() {
  tribble(
    ~id,     ~extension,    ~fun,                ~cache,   ~description,
    "cf",    "cf",          "iso_read_cf",        TRUE,     "Isodat Continuous Flow file format (older)",
    "dxf",   "dxf",         "iso_read_dxf",       TRUE,     "Isodat Continuous Flow file format (newer)",
    "iarc",  "iarc",        "iso_read_flow_iarc", TRUE,     "IonOS Continous Flow data archieve",
    #"zip",  "feather.zip", "iso_read_feather",   FALSE,    "Isoreader cached data format (for R+python)", # potential future expansion
    "rda",   "cf.rda",      "iso_read_rda",       FALSE,    "Isoreader R Data Archive"
  )
}