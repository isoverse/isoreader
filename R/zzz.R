.onLoad <- function(libname, pkgname) {
  # set default package options (does not overwrite if any values already set)
  opt <- options()
  default_options <- list(
    isoreader.quiet = FALSE,
    isoreader.debug = FALSE
  )
  to_set <- !(names(default_options) %in% names(opt))
  if(any(to_set)) options(default_options[to_set])
  invisible()
}
