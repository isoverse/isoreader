initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoreader.quiet = FALSE,
    isoreader.datetime_warnings = TRUE,
    isoreader.catch_errors = TRUE,
    isoreader.debug = FALSE,
    isoreader.cache = TRUE,
    isoreader.cache_dir = "cache",
    isoreader.read_raw_data = TRUE,
    isoreader.read_file_info = TRUE,
    isoreader.read_method_info = TRUE,
    isoreader.read_vendor_data_table = TRUE,
    isoreader.read_file_event = rlang::quo(NULL),
    isoreader.finish_file_event = rlang::quo(NULL),
    isoreader.file_readers = NULL
  )
  options(default_options)

  # set temporary options used during file processing
  temp_options <- list(
    isoreader_temp.parallel_process = NA_integer_,
    isoreader_temp.parallel_log_file = NULL,
    isoreader_temp.parallel_progress_file = NULL,
    isoreader_temp.progress_bar = NULL
  )
  options(temp_options)

  # register file readers
  iso_register_dual_inlet_file_reader(".did", "iso_read_did", "Dual Inlet file format (newer)", "Isodat", env = "isoreader")
  iso_register_dual_inlet_file_reader(".caf", "iso_read_caf", "Dual Inlet file format (older)", "Isodat", env = "isoreader")
  iso_register_dual_inlet_file_reader(".txt", "iso_read_nu", "Dual Inlet file format", "Nu", env = "isoreader")
  iso_register_dual_inlet_file_reader(".di.rda", "iso_read_rda", "R Data Archive (deprecated)", "isoreader", cacheable = FALSE, env = "isoreader")
  iso_register_dual_inlet_file_reader(".di.rds", "iso_read_rds", "R Data Storage", "isoreader", cacheable = FALSE, post_read_check = FALSE, env = "isoreader")
  iso_register_continuous_flow_file_reader(".cf", "iso_read_cf", "Continuous Flow file format (older)", "Isodat", env = "isoreader")
  iso_register_continuous_flow_file_reader(".dxf", "iso_read_dxf", "Continuous Flow file format (newer)", "Isodat", env = "isoreader")
  iso_register_continuous_flow_file_reader(".iarc", "iso_read_flow_iarc", "Continuous Flow data archive", "ionOS", env = "isoreader")
  iso_register_continuous_flow_file_reader(".cf.rda", "iso_read_rda", "R Data Archive (deprecated)", "isoreader", cacheable = FALSE, env = "isoreader")
  iso_register_continuous_flow_file_reader(".cf.rds", "iso_read_rds", "R Data Storage", "isoreader", cacheable = FALSE, post_read_check = FALSE, env = "isoreader")
  iso_register_scan_file_reader(".scn", "iso_read_scn", "Scan file format", "Isodat", env = "isoreader")
  iso_register_scan_file_reader(".scan.rds", "iso_read_rds", "R Data Storage", "isoreader", cacheable = FALSE, post_read_check = FALSE, env = "isoreader")
  invisible(options()[names(default_options)])

}

.onLoad <- function(libname, pkgname) {
  initialize_options()
  invisible()
}
