.onLoad <- function(libname, pkgname) {
  # set default package options (always resets options to force deliberate change of settings)
  default_options <- list(
    isoreader.quiet = FALSE,
    isoreader.debug = FALSE,
    isoreader.cache_dir = "cache"
  )
  options(default_options)
  invisible()
}
