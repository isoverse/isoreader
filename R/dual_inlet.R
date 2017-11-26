#' Load dual inlet data
#' 
#' @inheritParams iso_read_files
#' @param read_raw_data whether to read the raw mass/ion data from the file
#' @param read_file_info whether to read auxiliary file information (file id, sequence information, etc.)
#' @param read_method_info whether to read methods information (standards, processing info)
#' @param read_vendor_data_table whether to read the vendor computed data table
#' @family isoread functions for different types of IRMS data
#' @export
iso_read_dual_inlet <- function(
  paths, 
  read_raw_data = default(read_raw_data), read_file_info = default(read_file_info), 
  read_method_info = default(read_method_info), read_vendor_data_table = default(read_vendor_data_table),
  discard_duplicates = TRUE, cache = default(cache), read_cache = default(cache), quiet = default(quiet)) {
  
  # process data
  iso_read_files(
    paths,
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure(),
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

# get supported dual inlet file types and which functions they map to, whether they can be cached (if caching requested), etc.
get_supported_di_files <- function() {
  tribble(
    ~id,    ~extension,    ~fun,              ~cache,   ~description,
    "did",  "did",         "isoread_did",     TRUE,     "Isodat Dual Inlet file format (newer)",
    "caf",  "caf",         "isoread_caf",     TRUE,     "Isodat Dual Inlet file format (older)",
    #"zip", "feather.zip", "isoread_feather", FALSE,     "Isoreader cached data format (for R+python)", # potential future expansion
    "rda",   "di.rda",      "isoread_rda",     FALSE,    "Isoreader R Data Archive"
  )
}
