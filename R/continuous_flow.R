#' Load continuous flow data
#' 
#' @inheritParams isoread_files
#' @export
isoread_continuous_flow <- function(
  paths, quiet = default("quiet"), 
  read_mass_data = TRUE, read_data_table = TRUE, read_file_info = TRUE) {

  isoread_files(
    paths,
    supported_extensions = get_supported_cf_files(),
    data_structure = make_cf_data_structure()
  )
}

# get supported continuous flow file types and which functions they map to
get_supported_cf_files <- function() {
  tribble(
    ~extension, ~fun,                ~description,
    "cf",       "isoread_cf",        "Isodat Continuous Flow file format (older)",
    "dxf",      "isoread_dxf",       "Isodat Continuous Flow file format (newer)",
    "iarc",     "isoread_flow_iarc", "IonOS Continous Flow data archieve" 
  )
}