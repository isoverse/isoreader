#' @importFrom readr problems
#' @export
readr::problems

#' @importFrom readr stop_for_problems
#' @export
readr::stop_for_problems

# register a problem
register_problem <- function(obj, ..., reset = FALSE) {
  problem <- data_frame(...)
  if (!reset && n_problems(obj) > 0) {
    attr(obj, "problems") <- suppressWarnings(bind_rows(probs(obj), problem))
  } else {
    attr(obj, "problems") <- problem
  }
  return(obj)
}

# register file problem
# helper function to standardize problems for file reads
# with a filename, type and message
register_file_problem <- function(obj, filename, type, message, func = deparse(sys.call(-1)), 
                                  reset = FALSE) {
  register_problem(obj, file = filename, type = type, func = func, message = message, reset = reset)
}

# same approach as readr
probs <- function(x) {
  attr(suppressWarnings(x), "problems")
}

# same approach as readr
n_problems <- function(x) {
  probs <- probs(x)
  if (is.null(probs)) 0 else nrow(probs)
}