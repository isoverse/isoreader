# basic dual inlet data structure
make_di_data_structure <- function() {
  structure(
    list(
      file_info = list(
        file_name = NULL,
        file_path = NULL
      ),
      mass_data = data_frame()
    ),
    class = c("dual_inlet", "isofile")
  )
}

# basic continuous flow data structure
make_cf_data_structure <- function() {
  structure(
    list(
      file_info = list(
        file_name = NULL,
        file_path = NULL
      ),
      mass_data = data_frame()
    ),
    class = c("continuous_flow", "isofile")
  )
}

# set data structure file path
set_ds_file_path <- function(ds, file_path) {
  if (!is(ds, "isofile")) stop("can only set path for isofile data structures", call. = FALSE)
  if (!file.exists(file_path)) stop("file/folder does not exist: ", file_path, call. = FALSE)
  ds$file_info$file_name <- basename(file_path)
  ds$file_info$file_path <- file_path
  return(ds)
}