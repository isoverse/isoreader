# retrieve package default values, internal function, not exported
default <- function(name) {
  value <- getOption(str_c("isoreader.", name))
  if (is.null(value)) stop("isoreader default '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package default value, internal function, not exported
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoreader.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoreader.", name)))
  return(invisible(value))
}

#' Turn on information messages
#' Turns on information messages in all subsequent function calls by changing the global default for the \code{quiet} parameter of most isoreader functions. This function can be called stand alone or within a pipeline to turn messages on at a certain point during the pipeline.
#' 
#' @param data a data frame - returned as is if provided (e.g. in the middle of a pipeline)
#' @family info messages
#' @export
turn_info_messages_on <- function(data) {
  set_default("quiet", FALSE)
  message("Info: information messages turned on")
  if (!missing(data)) return(data)
}

#' Turn off information messages
#' Turns off information messages in all subsequent function calls by changing the global default for the \code{quiet} parameter of most isoreader functions. This function can be called stand alone or within a pipeline to turn messages off at a certain point during the pipeline.
#' 
#' @param data a data frame - returned as is if provided (e.g. in the middle of a pipeline)
#' @family info messages
#' @export
turn_info_messages_off <- function(data) {
  set_default("quiet", TRUE)
  if (!missing(data)) return(data)
}