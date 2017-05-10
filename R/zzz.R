.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_isoreader <- list(
    isoreader.quiet = FALSE
  )
  to_set <- !(names(opt_isoreader) %in% names(opt))
  if(any(to_set)) options(opt_isoreader[to_set])
  invisible()
}
