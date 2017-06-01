#' Load dual inlet data
#' 
#' @inheritParams isoread_files
#' @export
isoread_dual_inlet <- function(paths, quiet = setting("quiet"), 
                               read_mass_data = TRUE, read_file_info = TRUE) {
  isoread_files(
    paths,
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure(),
    quiet = quiet
  )
}

# get supported dual inlet file types and which functions they map to
get_supported_di_files <- function() {
  tribble(
    ~extension, ~fun,        ~description,
    "did",      "isoread_did", "Isodat Dual Inlet file format"
  )
}
