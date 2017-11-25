# retrieve package settings, internal function, not exported
default <- function(name) {
  name <- enquo(name) %>% quos_to_text(variable = "setting")
  value <- getOption(str_c("isoreader.", name))
  if (is.null(value)) stop("isoreader setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoreader.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoreader.", name)))
  return(invisible(value))
}

#' Show the current default parameters
#' Shows a table with the default function parameters for this package.
#' @inheritParams turn_caching_on
#' @family settings functions
#' @export
show_isoreader_parameters <- function(data = NULL) {
  message("Info: isoreader package current default parameters")
  current <- 
    c("quiet", "cache", "cache_dir", "read_raw_data", "read_file_info", "read_method_info", "read_vendor_data_table") %>% 
    sapply(function(x) list(default(!!x))) %>% 
    {
      data_frame(parameter = names(.),
                    value = as.character(unlist(.)))
    } %>% 
    print()
    
  # for pipeline
  return(invisible(data))
}

#' Turn caching on/off
#' 
#' These functions turn caching of data files (and reading from cache) on/off in all subsequent isoread calls by changing the global settings for the \code{cache} parameter. Can be called stand alone or within a pipeline.
#' 
#' @param data a data frame - returned invisibly as is if provided (e.g. in the middle of a pipeline)
#' @name caching
#' @family settings functions
NULL

#' @rdname caching
#' @export
turn_caching_on <- function(data) {
  set_default("cache", TRUE)
  if (!default(quiet)) message("Info: caching turned on")
  if (!missing(data)) return(invisible(data))
}

#' @rdname caching
#' @export
turn_caching_off <- function(data) {
  set_default("cache", FALSE)
  if (!default(quiet)) message("Info: caching turned off")
  if (!missing(data)) return(invisible(data))
}

#' Set default read options
#' @inheritParams turn_caching_on
#' @param read_raw_data if provided, set as the default for `read_raw_data` parameters
#' @param read_file_info if provided, set as the default for `read_file_info` parameters
#' @param read_method_info if provided, set as the default for `read_method_info` parameters
#' @param read_vendor_data_table if provided, set as the default for `read_vendor_data_tabl` parameters
#' @export
#' @family settings functions
set_default_read_parameters <- function(data, read_raw_data, read_file_info, read_method_info, read_vendor_data_table, quiet = default(quiet)) {
  
  read_params <- list()
  
  if (!missing(read_raw_data)) read_params <- c(read_params, list(read_raw_data = read_raw_data))
  if (!missing(read_file_info)) read_params <- c(read_params, list(read_file_info = read_file_info))
  if (!missing(read_method_info)) read_params <- c(read_params, list(read_method_info = read_method_info))
  if (!missing(read_vendor_data_table)) read_params <- c(read_params, list(read_vendor_data_table = read_vendor_data_table))
  
  # safety check
  if (!all(ok <- map_lgl(read_params, is.logical))){
    glue("read parameters must be TRUE or FALSE, provided: {collapse(as.character(unlist(read_params[!ok])), ', ')}") %>% 
      stop(call. = FALSE)
  }
  
  # info message
  if(!quiet) {
    params <- sprintf("%s = %s", names(read_params), read_params)
    glue("Info: setting read parameter(s) '{collapse(params, \"', '\", last = \"' and '\")}'") %>% message()
  }

  # set values
  mapply(set_default, names(read_params), read_params)
    
  if (!missing(data)) return(invisible(data))
}

#' Turn information messages on/off
#' 
#' These functions turn information messages on/off in all subsequent function calls by changing the global settings for the \code{quiet} parameter of most isoreader functions. These functions can be called stand alone or within a pipeline to turn messages on/off at a certain point during the pipeline.
#' 
#' @inheritParams turn_caching_on
#' @name info_messages
NULL

#' @rdname info_messages
#' @family settings functions
#' @export
turn_info_messages_on <- function(data) {
  set_default("quiet", FALSE)
  message("Info: information messages turned on")
  if (!missing(data)) return(invisible(data))
}

#' @rdname info_messages
#' @export
turn_info_messages_off <- function(data) {
  set_default("quiet", TRUE)
  if (!missing(data)) return(invisible(data))
}

# update quiet returns update function for on.exit
update_quiet <- function(quiet) {
  if (quiet != default(quiet)) {
    quiet_setting <- default(quiet)
    set_default("quiet", quiet)
    return(function() set_default("quiet", quiet_setting))
  } else {
    return(function() {})
  }
}

# turn debug messages on/off
# not exported, used for internal debugging
# @param catch_errors whether to still catch errors in debug mode or whether to throw them
turn_debug_on <- function(data, catch_errors = TRUE) {
  set_default("debug", TRUE)
  set_default("catch_errors", catch_errors)
  message("Info: debug mode turned on, error catching turned ", if(catch_errors) "on" else "off")
  if (!missing(data)) return(data)
}

turn_debug_off <- function(data) {
  set_default("debug", FALSE)
  set_default("catch_errors", TRUE)
  message("Info: debug mode turned off")
  if (!missing(data)) return(data)
}