.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_isoreadr <- list(
    isoreadr.quiet = FALSE
  )
  to_set <- !(names(opt_isoreadr) %in% names(opt))
  if(any(to_set)) options(opt_isoreadr[to_set])
  invisible()
}
