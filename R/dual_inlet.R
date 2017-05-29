#' Load dual inlet data
#' 
#' @inheritParams convert_voltages_to_currents
#' @export
isoread_dual_inlet <- function(paths, quiet = default("quiet")) {
  isoread_files(
    paths,
    supported_extensions = get_supported_di_files(),
    data_structure = make_di_data_structure()
  )
}

# basic dual inlet data structure
make_di_data_structure <- function() {
  structure(
    list(
      file_info = list(),
      mass_data = data_frame()
    ),
    class = c("dual_inlet", "isofile")
  )
}

# get supported dual inlet file types and which functions they map to
get_supported_di_files <- function() {
  tribble(
    ~extension, ~fun,        ~description,
    "did",      isoread_did, "Isodat Dual Inlet file format"
  )
}

# read isodat did file
# @param ds the data structure to fill
isoread_did <- function(ds) {
  
  if(!is(ds, "isofile") || !is(ds, "dual_inlet")) 
    stop("data structure must have class 'isofile' and 'dual_inlet'", call. = FALSE)
  col_check(c("file_info", "mass_data"), ds)
  col_check(c("file_name", "file_path"), ds$file_info)
  
  return(ds)  
}

#' Print a dual inlet isofile
#' @export
print.dual_inlet <- function(x, ...) {
  sprintf("Dual inlet data from '%s'\n", x$file_info$file_path) %>% 
    cat()
  if (n_problems(x) > 0) {
    cat("Encountered problems:\n")
    print(problems(x))
  }
}
