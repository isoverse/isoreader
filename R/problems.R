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
        filename = character(),
        type = character(),
        func = character(),
        message = character()
      )
  }
  return(obj)
}

# register a problem during isoreader operations
# helper function to standardize problems for file reads
# with a filename, type and message
register_problem <- function(obj, filename = NA_character_, type = NA_character_, message = NA_character_, ..., 
                                  func = deparse(sys.call(-1)), 
                                  reset = FALSE) {
  if (func == "NULL") func <- NA_character_
  problem <- data_frame(filename = filename, type = type, func = func, message = message, ...)
  if (!reset && n_problems(obj) > 0) {
    attr(obj, "problems") <- suppressWarnings(bind_rows(probs(obj), problem))
  } else {
    attr(obj, "problems") <- problem
  }
  return(obj)
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

