#' @title Problem Functions
#' @description The following functions to check for and deal with problems are available.
#' 
#' \code{\link[readr]{problems}}
#' 
#' \code{\link[readr]{stop_for_problems}}
#' 
#' \code{\link{omit_files_with_problems}}
#' @name problem_functions
#' @rdname problem_function.Rd
#' @family problem_functions
NULL

#' @importFrom readr problems
#' @export
readr::problems

#' @importFrom readr stop_for_problems
#' @export
readr::stop_for_problems

#' Remove problematic files
#' 
#' Removes the files that have encountered problems, either errors, warnings or both and returns the remaining isofiles. For additional functions available to check for and deal with problems, see the \link{problem_functions}.
#' @inheritParams aggregate_raw_data
#' @param type what type of problem causes removal of the file: \code{"error"}, \code{"warning"} or \code{"both"}
#' @family problem functions
#' @export
omit_files_with_problems <- function(isofiles, type = c("error", "warning", "both"), quiet = setting("quiet")) {
  if (missing(isofiles) || !is_iso_object(isofiles)) stop("please provide a list of isofiles", call. = FALSE)
  if (missing(type)) type <- "both"
  if (length(type) > 1) stop("more than one type specified", call. = FALSE)
  if (!type %in% c("error", "warning", "both")) stop("unknown problem type specified: ", type, call. = FALSE)
  types <- if (type == "both") c("error", "warning") else type
  isofiles <- as_isofile_list(isofiles)
  
  # find trouble file ids
  trouble_files <- problems(isofiles) %>% 
    filter(type %in% types) %>% 
    { unique(.$file_id) }

  # exclude
  exclude <- names(isofiles) %in% trouble_files
  if (!quiet) {
    sprintf("Info: removing %d/%d files that have %ss", 
                      sum(exclude), length(isofiles), 
                      if (type == "both") "errors or warning" else type) %>% message()
  }
  return(isofiles[!exclude])
}

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

