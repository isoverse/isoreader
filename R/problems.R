#' @importFrom readr problems
#' @export
readr::problems

#' @importFrom readr stop_for_problems
#' @export
readr::stop_for_problems

# register a problem during isoreader operations
# helper function to standardize problems for file reads
# with a filename, type and details
# will propage the problem to all underlying files
# @obj isofile or isofile_list
register_problem <- function(obj, type = NA_character_, details = NA_character_, ..., 
                                  func = deparse(sys.call(-1))) {
  if (func == "NULL") func <- NA_character_
  problem <- data_frame(type = type, func = func, details = details, ...)
  if (is_isofile_list(obj)) {
    obj <- as.list(obj)
    for (i in 1:length(obj)) {
      existing_problems <- get_problems(obj[[i]])
      obj[[i]] <- set_problems(obj[[i]], suppressWarnings(bind_rows(existing_problems, problem)))
    } 
    obj <- as_isofile_list(obj)
  } else {
    obj <- obj %>% set_problems(
        suppressWarnings(bind_rows(get_problems(obj), problem)))
  }
  return(obj)
}

# register warning
register_warning <- function(obj, details = NA_character_, ..., 
                             func = find_parent_call("register_warning"), warn = TRUE) {
  force(func)
  if (warn) warning(details, call. = FALSE, immediate. = TRUE)
  register_problem(obj, type = "warning", details = details, func = func, ...)
}

# register error
register_error <- function(obj, details = NA_character_, ..., 
                           func = find_parent_call("register_error"), warn = TRUE) {
  force(func)
  if (warn) warning("caught error - ", details, call. = FALSE, immediate. = TRUE)
  register_problem(obj, type = "error", details = details, func = func, ...)
}


# set problems
set_problems <- function(obj, problems) {
  attr(obj, "problems") <- problems
  return(obj)
}

# get problems (for internal use only, external use is readr::problems)
get_problems <- function(x) {
  probs <- attr(suppressWarnings(x), "problems")
  if (is.null(probs)) {
    # return empty initialized problems structure
    return(get_problems_structure())
  } 
  return(probs)
}

# combine problems
# @return problems data frame (needs to be assigned with set_problems)
combined_problems <- function(...) {
  objs <- list(...)
  suppressWarnings(
    lapply(objs, get_problems) %>% 
      bind_rows()
  )
}

# isoreader problems structure
get_problems_structure <- function() {
  data_frame(
    type = character(),
    func = character(),
    details = character()
  )
}

# initialize problems attribute
initialize_problems_attribute <- function(obj) {
  if (n_problems(obj) == 0) {
    obj <- set_problems(obj, get_problems_structure())
  }
  return(obj)
}

# equivalent approach to readr (not exported there)
n_problems <- function(x) {
  probs <- attr(suppressWarnings(x), "problems")
  if (is.null(probs)) 0 else nrow(probs)
}

