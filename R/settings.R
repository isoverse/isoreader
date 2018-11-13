# retrieve package settings, internal function, not exported
default <- function(name, allow_null = FALSE) {
  name <- enquo(name) %>% quos_to_text(variable = "setting")
  value <- getOption(str_c("isoreader.", name))
  if (!allow_null && is.null(value)) stop("isoreader setting '", name, "' does not exist", call. = FALSE)
  return(value)
}

# set package setting, internal function, not exported
set_default <- function(name, value, overwrite = TRUE) {
  if (overwrite || !str_c("isoreader.", name) %in% names(options()))
    options(list(value) %>% setNames(str_c("isoreader.", name)))
  return(invisible(value))
}

# helper function to transfer option settings between processors
get_all_options <- function() {
  all_opts <- options()
  all_opts[str_detect(names(all_opts), "^isoreader\\.")]
}

#' Get the current default parameters
#' 
#' Retrieve a table with all default function parameters for this package. 
#' To set read parameters, see \code{\link{iso_set_default_read_parameters}}. 
#' To set messaging and caching parameters see \code{\link{iso_info_messages}} and see \code{\link{iso_caching}}.
#' For a piping compatible version of this function, see \link{iso_show_default_reader_parameters}.
#' @family settings functions
#' @export
iso_get_default_reader_parameters <- function() {
    c("quiet", "cache", "cache_dir", "read_raw_data", "read_file_info", "read_method_info", "read_vendor_data_table") %>% 
    sapply(function(x) list(default(!!x))) %>% 
    {
      data_frame(parameter = names(.),
                 value = as.character(unlist(.)))
    }
}

#' Show the current default parameters
#' 
#' Shows a table with the default function parameters for this package.
#' @inheritParams iso_turn_reader_caching_on
#' @param func function to use for formatting the reader parameters table, e.g. \code{\link[knitr]{kable}}. 
#' Note that if the output is in RMarkdown chunks, the chunk option must have \code{results="asis"} for the table to be correctly formatted.
#' @param ... additional parameters to forward to the \code{func} function
#' @family settings functions
#' @export
iso_show_default_reader_parameters <- function(data = NULL, func = NULL, ...) {
  if (!default("quiet")) message("Info: isoreader package current default parameters")
  
  if (!is.null(func))
    print(do.call(func, args = c(list(x = iso_get_default_reader_parameters()), list(...))))
  else
    print(iso_get_default_reader_parameters())
    
  # for pipeline
  return(invisible(data))
}

#' Turn caching on/off
#' 
#' These functions turn caching of data files (and reading from cache) on/off in all subsequent isoread calls by changing the global settings for the \code{cache} parameter. Can be called stand alone or within a pipeline.
#' 
#' @param data a data frame - returned invisibly as is if provided (e.g. in the middle of a pipeline)
#' @name iso_caching
#' @family settings functions
NULL

#' @rdname iso_caching
#' @export
iso_turn_reader_caching_on <- function(data = NULL) {
  set_default("cache", TRUE)
  if (!default(quiet)) message("Info: caching turned on")
  if (!missing(data)) return(invisible(data))
}

#' @rdname iso_caching
#' @export
iso_turn_reader_caching_off <- function(data = NULL) {
  set_default("cache", FALSE)
  if (!default(quiet)) message("Info: caching turned off")
  if (!missing(data)) return(invisible(data))
}

#' Set default read options
#' @inheritParams iso_turn_reader_caching_on
#' @inheritParams iso_read_files
#' @param read_raw_data if provided, set as the default for `read_raw_data` parameters
#' @param read_file_info if provided, set as the default for `read_file_info` parameters
#' @param read_method_info if provided, set as the default for `read_method_info` parameters
#' @param read_vendor_data_table if provided, set as the default for `read_vendor_data_tabl` parameters
#' @export
#' @family settings functions
iso_set_default_read_parameters <- function(data = NULL, read_raw_data, read_file_info, read_method_info, read_vendor_data_table, quiet = default(quiet)) {
  
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
#' @inheritParams iso_turn_reader_caching_on
#' @name iso_info_messages
NULL

#' @rdname iso_info_messages
#' @family settings functions
#' @export
iso_turn_info_messages_on <- function(data = NULL) {
  set_default("quiet", FALSE)
  message("Info: information messages turned on")
  if (!missing(data)) return(invisible(data))
}

#' @rdname iso_info_messages
#' @export
iso_turn_info_messages_off <- function(data = NULL) {
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

#' Debugging functions
#' 
#' For troubleshooting. Not exported.
#' 
#' @inheritParams iso_turn_reader_caching_on
#' @name iso_debug_mode
NULL

#' @param catch_errors whether to still catch errors in debug mode or whether to throw them
#' @param cache whether to cache or read anything from cache
#' @rdname iso_debug_mode
iso_turn_debug_on <- function(data = NULL, catch_errors = TRUE, cache = FALSE) {
  set_default("debug", TRUE)
  set_default("catch_errors", catch_errors)
  set_default("cache", cache)
  glue(
    "Info: debug mode turned on, ",
    "error catching turned {if(catch_errors) 'on' else 'off'}, ",
    "caching turned {if(cache) 'on' else 'off'}") %>% 
    message()
  if (!missing(data)) return(data)
}

#' @rdname iso_debug_mode
iso_turn_debug_off <- function(data = NULL) {
  set_default("debug", FALSE)
  set_default("catch_errors", TRUE)
  set_default("cache", TRUE)
  message("Info: debug mode turned off")
  if (!missing(data)) return(data)
}

#' @param event_expr an expression to evaluate in the context of reading individual iso files (evaluated in the local environment at the beginning of a file read)
#' @rdname iso_debug_mode
set_read_file_event_expr <- function(event_expr = NULL) {
  set_default("read_file_event", enquo(event_expr))
}

#' @param event_expr an expression to evaluate in the context of reading individual iso files (evaluated in the local environment at the end of a file read)
#' @rdname iso_debug_mode
set_finish_file_event_expr <- function(event_expr = NULL) {
  set_default("finish_file_event", enquo(event_expr))
}