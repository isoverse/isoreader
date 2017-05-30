#' Load dual inlet data
#' 
#' @inheritParams isoread_files
#' @export
isoread_dual_inlet <- function(paths, quiet = default("quiet"), 
                               read_mass_data = TRUE, read_file_info = TRUE) {
  isoread_files(
    paths,
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure()
  )
}

# get supported dual inlet file types and which functions they map to
get_supported_di_files <- function() {
  tribble(
    ~extension, ~fun,        ~description,
    "did",      isoread_did, "Isodat Dual Inlet file format"
  )
}

#' Print a dual inlet isofile
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @export
print.dual_inlet <- function(x, ...) {
  sprintf("Dual inlet data from '%s'\n", x$file_info$file_path) %>% 
    cat()
  if (n_problems(x) > 0) {
    cat("Encountered problems:\n")
    print(problems(x), ...)
    cat("\n")
  }
}
