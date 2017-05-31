#' @importFrom readr problems
#' @export
readr::problems

#' @importFrom readr stop_for_problems
#' @export
readr::stop_for_problems

# isoreader problems structure
initialize_problems_attribute <- function(obj) {
  if (n_problems(obj) == 0) {
    attr(obj, "problems") <-
      data_frame(
        type = character(),
        func = character(),
        details = character()
      )
  }
  return(obj)
}

# register a problem during isoreader operations
# helper function to standardize problems for file reads
# with a filename, type and details
register_problem <- function(obj, type = NA_character_, details = NA_character_, ..., 
                                  func = deparse(sys.call(-1)), reset = FALSE) {
  if (func == "NULL") func <- NA_character_
  problem <- data_frame(type = type, func = func, details = details, ...)
  if (!reset && n_problems(obj) > 0) {
    attr(obj, "problems") <- suppressWarnings(bind_rows(probs(obj), problem))
  } else {
    attr(obj, "problems") <- problem
  }
  return(obj)
}

# register warning
register_warning <- function(obj, details = NA_character_, ..., 
                             func = deparse(sys.call(-1)), warn = TRUE) {
  if (warn) warning(details, call. = FALSE, immediate. = TRUE)
  register_problem(obj, type = "warning", details = details, func = func, ...)
}

# register error
register_error <- function(obj, details = NA_character_, ..., 
                           func = deparse(sys.call(-1)), warn = TRUE) {
  if (warn) warning("caught error '", details, "'", call. = FALSE, immediate. = TRUE)
  register_problem(obj, type = "error", details = details, func = func, ...)
}


# equivalent approach to readr (not exported there)
probs <- function(x) {
  attr(suppressWarnings(x), "problems")
}

# equivalent approach to readr (not exported there)
n_problems <- function(x) {
  probs <- probs(x)
  if (is.null(probs)) 0 else nrow(probs)
}

