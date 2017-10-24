# retrieve package settings, internal function, not exported
setting <- function(name) {
  value <- getOption(str_c("isoreader.", name))
  if (is.null(value)) stop("isoreader setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_setting <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoreader.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoreader.", name)))
  return(invisible(value))
}

#' Turn caching on/off
#' 
#' These functions turn caching of data files (and reading from cache) on/off in all subsequent isoread calls by changing the global settings for the \code{cache} parameter. Can be called stand alone or within a pipeline.
#' 
#' @param data a data frame - returned as is if provided (e.g. in the middle of a pipeline)
#' @name caching
NULL

#' @rdname caching
#' @export
turn_caching_on <- function(data) {
  set_setting("cache", TRUE)
  if (!setting("quiet")) message("Info: caching turned on")
  if (!missing(data)) return(data)
}

#' @rdname caching
#' @export
turn_caching_off <- function(data) {
  set_setting("cache", FALSE)
  if (!setting("quiet")) message("Info: caching turned off")
  if (!missing(data)) return(data)
}



#' Turn information messages on/off
#' 
#' These functions turn information messages on/off in all subsequent function calls by changing the global settings for the \code{quiet} parameter of most isoreader functions. These functions can be called stand alone or within a pipeline to turn messages on/off at a certain point during the pipeline.
#' 
#' @param data a data frame - returned as is if provided (e.g. in the middle of a pipeline)
#' @name info_messages
NULL

#' @rdname info_messages
#' @export
turn_info_messages_on <- function(data) {
  set_setting("quiet", FALSE)
  message("Info: information messages turned on")
  if (!missing(data)) return(data)
}

#' @rdname info_messages
#' @export
turn_info_messages_off <- function(data) {
  set_setting("quiet", TRUE)
  if (!missing(data)) return(data)
}

# update quiet returns update function for on.exit
update_quiet <- function(quiet) {
  if (quiet != setting("quiet")) {
    quiet_setting <- setting("quiet")
    set_setting("quiet", quiet)
    return(function() set_setting("quiet", quiet_setting))
  } else {
    return(function() {})
  }
}

# turn debug messages on/off
# not exported, used for internal debugging
# @param catch_errors whether to still catch errors in debug mode or whether to throw them
turn_debug_on <- function(data, catch_errors = TRUE) {
  set_setting("debug", TRUE)
  set_setting("catch_errors", catch_errors)
  message("Info: debug mode turned on, error catching turned ", if(catch_errors) "on" else "off")
  if (!missing(data)) return(data)
}

turn_debug_off <- function(data) {
  set_setting("debug", FALSE)
  set_setting("catch_errors", TRUE)
  message("Info: debug mode turned off")
  if (!missing(data)) return(data)
}