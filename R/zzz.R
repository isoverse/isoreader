initialize_options <- function() {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoreader.quiet = FALSE,
    isoreader.catch_errors = TRUE,
    isoreader.debug = FALSE,
    isoreader.cache = TRUE,
    isoreader.cache_dir = "cache",
    isoreader.read_raw_data = TRUE,
    isoreader.read_file_info = TRUE,
    isoreader.read_method_info = TRUE,
    isoreader.read_vendor_data_table = TRUE,
    isoreader.read_file_event = quo(NULL)
  )
  options(default_options)
}

.onLoad <- function(libname, pkgname) {
  initialize_options()
  invisible()
}
