# Structures ----

# basic data structure
make_iso_data_structure <- function() {
  structure(
    list(
      file_info = list(
        file_id = NA_character_, # unique identifer
        file_path = NA_character_, # path to file (file extension is key for processing)
        file_subpath = NA_character_ # sub path in case file is an archieve
      ),
      mass_data = data_frame() # all mass data
    ),
    class = c("isofile")
  ) %>% 
    initialize_problems_attribute()
}


# basic dual inlet data structure
make_di_data_structure <- function() {
  struct <- make_iso_data_structure()
  class(struct) <- c("dual_inlet", class(struct))
  return(struct)
}

# basic continuous flow data structure
make_cf_data_structure <- function() {
  struct <- make_iso_data_structure()
  class(struct) <- c("continuous_flow", class(struct))
  return(struct)
}

# Printing ----

#' Print a collection of isofiles
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @export
print.isofiles <- function(x, ...) {
  sprintf("# data from %d isofiles:\n", length(x)) %>% 
    cat()
  sapply(x, print, show_problems = FALSE)
  if (n_problems(x) > 0) {
    cat("\nAll encountered problems:\n")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' Print a dual inlet isofile
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @param show_problems whether to show encountered problems
#' @export
print.dual_inlet <- function(x, ..., show_problems = TRUE) {
  sprintf("Dual inlet data '%s' (%s%s)\n", 
          x$file_info$file_id,
          x$file_info$file_path,
          x$file_info$file_subpath %>% { if(!is.na(.)) str_c("|", .) else "" }
  ) %>% cat()
  if (show_problems && n_problems(x) > 0) {
    cat("Encountered problems:\n")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}

#' Print a continuous_flow isofile
#' @param x Object to show.
#' @param ... additional parameters passed to print.default
#' @param show_problems whether to show encountered problems
#' @export
print.continuous_flow <- function(x, ..., show_problems = TRUE) {
  sprintf("Continuous flow data '%s' (%d data points: %s) from %s%s\n", 
          x$file_info$file_id,
          x$mass_data %>% nrow(),
          x$mass_data %>% select(matches("^[iIvV]")) %>% names() %>% 
            { if(length(.) == 0) "0 ions" else str_c(., collapse = ", ") },
          x$file_info$file_path,
          x$file_info$file_subpath %>% { if(!is.na(.)) str_c("|", .) else "" }
  ) %>% cat()
  if (show_problems && n_problems(x) > 0) {
    cat("\nEncountered problems:\n")
    print(problems(x), ...)
    cat("\n")
  }
  invisible(x)
}


# set data structure file path
set_ds_file_path <- function(ds, file_path, file_id = basename(file_path), file_subpath = NA_character_) {
  if (!is(ds, "isofile")) stop("can only set path for isofile data structures", call. = FALSE)
  if (!file.exists(file_path)) stop("file/folder does not exist: ", file_path, call. = FALSE)
  ds$file_info$file_path <- file_path
  ds$file_info$file_id <- file_id
  ds$file_info$file_subpath <- file_subpath
  return(ds)
}