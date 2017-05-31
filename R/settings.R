# retrieve package default values, internal function, not exported
# @TODO: rename default to 'setting'
default <- function(name) {
  value <- getOption(str_c("isoreader.", name))
  if (is.null(value)) stop("isoreader default '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package default value, internal function, not exported
# @TODO: rename to 'set_setting'
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoreader.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoreader.", name)))
  return(invisible(value))
}

#' Turn information messages on/off
#' 
#' These functions turn information messages on/off in all subsequent function calls by changing the global default for the \code{quiet} parameter of most isoreader functions. These functions can be called stand alone or within a pipeline to turn messages on/off at a certain point during the pipeline.
#' 
#' @param data a data frame - returned as is if provided (e.g. in the middle of a pipeline)
#' @name info_messages
NULL

#' @rdname info_messages
#' @export
turn_info_messages_on <- function(data) {
  set_default("quiet", FALSE)
  message("Info: information messages turned on")
  if (!missing(data)) return(data)
}

#' @rdname info_messages
#' @export
turn_info_messages_off <- function(data) {
  set_default("quiet", TRUE)
  if (!missing(data)) return(data)
}


# turn debug messages on/off
# not exported, used for internal debugging
turn_debug_on <- function(data) {
  set_default("debug", TRUE)
  message("Info: debug mode turned on")
  if (!missing(data)) return(data)
}

turn_debug_off <- function(data) {
  set_default("debug", FALSE)
  message("Info: debug mode turned off")
  if (!missing(data)) return(data)
}