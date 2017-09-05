# unit conversion functions ----

#' Convert time units in continuous flow files
#' 
#' This function can be used to convert the time units of a collection of isofiles to a new common time unit. The original time units are inferred from the naming of the time column. New time units are very flexible and can be anything that \code{\link[lubridate]{duration}} understands, i.e. "s", "seconds", "min", "minutes", "hours", "days", etc. are all valid units.
#' 
#' @inheritParams plot_raw_data
#' @param to what time units to convert to
#' @export
#' @return the passed in isofile(s) with changed time units
convert_time <- function(isofiles, to, quiet = setting("quiet")) {
  
  # checks
  if(!is_continuous_flow(isofiles)) stop("can only convert time in continuous flow isofiles", call. = FALSE)
  if(missing(to)) stop("no time unit to convert to specified", call. = FALSE)
  single_file <- is_isofile(isofiles) # to make sure return is the same as supplied
  isofiles <- as_isofile_list(isofiles)
  
  if (!quiet) 
    sprintf("Info: converting time to '%s' for %d continuous flow data file(s)", to, length(isofiles)) %>% message()
  
  # make sure data is provided
  check_read_options(isofiles, "raw_data")
  
  # find time from units
  time_pattern <- "^time\\.(.*)$"
  time_from <- sapply(isofiles, function(isofile) {
    time_column <- str_subset(names(isofile$raw_data), time_pattern)
    if(length(time_column) == 0) return(NA_character_) # no time column (potentially data not read)
    if (length(time_column) != 1) 
      stop("unclear which column is the time column in '", isofile$file_info$file_id, 
          "', found: ", str_c(time_column, collapse = ", "), call. = FALSE)
    str_match(time_column, time_pattern) %>% {.[2]}
  })
  
  # for each file convert the time units
  for (i in 1:length(isofiles)) {
    if (!is.na(time_from[i])) {
      old_time <- str_c("time.", time_from[i])
      new_time <- str_c("time.", to)
      isofiles[[i]]$raw_data[[old_time]] <- scale_time(isofiles[[i]]$raw_data[[old_time]], to = to, from = time_from[i])
      isofiles[[i]]$raw_data <- isofiles[[i]]$raw_data %>% rename_(.dots = setNames(old_time, new_time))
    }
  }
  
  # return single (if passed in as single) 
  if (single_file) return (isofiles[[1]])
  return(isofiles)
}



#' Convert signal units in continuous flow and dual inlet files
#' 
#' This function can be used to convert the intensity units of a collection of isofiles to a scaled unit (e.g. from mV to V, or nA to mA) or different unit (e.g. from mV to nA or vice-versa). The original signal intensity units are inferred from the naming of the intensity columns. The new units must be a voltage (V) or current (A) but can have any valid SI prefix. Conversion from voltage to current and vice versa requires information about the resistors used in the op amp. This information is automatically retrieved during file read for file formats that contain resistors values (e.g. dxf and did) and are read with the \code{read_method_info=TRUE} parameter (see \code{\link{aggregate_resistors_info}} for details on how to access resistor values in read files). If resistor values are not set in a file, it will not allow automatic conversion between voltage and current. Instead, the \code{R} and \code{R_units} parameters can be used to provide specific resistor values. However, if \code{R} is set, these values will be used for all passed in \code{isofiles}. 
#' 
#' @inheritParams plot_raw_data
#' @param to what signal unit to convert to
#' @param R resistor value(s). If not specified, will use resitor values from individual isofiles. If specified, must be a named vector with Rm (m=mass) as names (e.g. \code{c(R45=0.3, R46=3)}), in units of \code{R_units}. If specified, will be used for ALL provided isofiles.
#' @param R_units determined what units resistor values (\code{R}) are in, if they are specified. Example \code{R_units = "GOhm"} designates that the resistor values provided in \code{R} parameter are in Giga-Ohm, i.e. 10^9 Ohm.
#' @export
#' @return the passed in isofile(s) with changed signal units
convert_signals <- function(isofiles, to, R, R_units = NA, quiet = setting("quiet")) {
  
  # checks
  if(!is_continuous_flow(isofiles) && !is_dual_inlet(isofiles)) stop("can only convert signals in continuous flow and dual inlet isofiles", call. = FALSE)
  if(missing(to)) stop("no unit to convert to specified", call. = FALSE)
  if(!missing(R) && is.na(R_units)) stop("resistor values (R) are specified but their units (R_units) are not", call. = FALSE)
  if(missing(R) && !is.na(R_units)) stop("resistor units (R_units) are specified but resistor values (R) are not", call. = FALSE)
  if(!missing(R) && ( !is.vector(R, "numeric") || is.null(names(R)) || any(names(R) == ""))) 
    stop("specified resistance values have to be a named numeric vector - e.g., c(R45=0.3)", call. = FALSE)
  single_file <- is_isofile(isofiles) # to make sure return is the same as supplied
  isofiles <- as_isofile_list(isofiles)
  auto_R <- missing(R)
  
  if (!quiet) {
    sprintf("Info: converting signals to '%s' for %d continuous flow data file(s) with %s", 
            to, length(isofiles), 
            if (auto_R) "automatic resistor values from individual isofiles"
            else str_c("specific resistor value(s): ", str_c(str_c(names(R),"=",R, R_units), collapse = ", "))
            ) %>% message()
  }
  
  # make sure data is available
  check_read_options(isofiles, "raw_data")
  
  # apply signal conversion
  func <- "convert_signals"
  signal_pattern <- sprintf("^[iv](\\d+)\\.(\\w+)$")
  to_units <- get_unit_scaling(to, c("V", "A"))
  R_name <- R.Ohm <- NULL # global vars
  isofiles <- isofiles %>% lapply(function(isofile) {
    
    # don't try to convert if raw data not present or otherwise empty
    if (!isofile$read_options$raw_data || nrow(isofile$raw_data) == 0) return(isofile)
    
    # column names
    col_names <- names(isofile$raw_data) %>% str_subset(signal_pattern)
    if (length(col_names) == 0) {
      return(register_warning(
        isofile, func = func, details = "could not find any voltage or current data columns"))
    }
    
    # see if resistors are required (i.e. any columns are v/i and 'to' is not the same)
    base_units <- col_names %>% str_match(signal_pattern) %>% {.[,3]} %>% 
      lapply(get_unit_scaling, base_units = c("V", "A")) %>% sapply(`[[`, "base_unit")
    scaling_only <- all(base_units == to_units$base_unit)
    
    # resistors
    if (scaling_only) {
      # scaling only
      isofile$raw_data <- scale_signals(isofile$raw_data, col_names, to = to, quiet = TRUE)
      return(isofile)
    } 
    
    if (!scaling_only && auto_R) {
      # find resistors from files
      if (!isofile$read_options$method_info) {
        return(register_warning(
          isofile, func = func,
          details = "cannot automatically determine resistor values, method info not available (read_method_info = FALSE)"))
      } else if (is.null(isofile$method_info$resistors) || nrow(isofile$method_info$resistors) == 0) {
        return(register_warning(
          isofile, func = func,
          details = "cannot automatically determine resistor values, no resistor data available"))
      } else if (!"mass" %in% names(isofile$method_info$resistors)) {
        return(register_warning(
          isofile, func = func,
          details = "cannot automatically determine resistor values, resistor data not linked to masses"))
      } else {
        R <- isofile$method_info$resistors %>% 
          mutate(R_name = str_c("R", mass)) %>% 
          select(R_name, R.Ohm) %>% deframe()
        R_units <- "Ohm"
      }
    }
    
    # convert signals
    isofile$raw_data <- scale_signals(isofile$raw_data, col_names, to = to, R = R, R_units = R_units, quiet = TRUE)
    return(isofile)
  }) %>% as_isofile_list()
  
  # report problems from this particular function
  convert_signal_problems <- problems(isofiles) %>% filter(func == func)
  if (!quiet && nrow(convert_signal_problems) > 0) {
    sprintf("Warning: encountered %d problem(s) during signal conversion (%d total problems):",
            nrow(convert_signal_problems), n_problems(isofiles)) %>% 
    message()
    print(convert_signal_problems)
    cat("\n")
  }
  
  # return single (if passed in as single) 
  if (single_file) return (isofiles[[1]])
  return(isofiles)
}



# processing functions ----

# scale time units (uses lubridate)
# @param to unit to scale to
# @param from unit to scale from (only needs to be supplied if time is not already a duration)
scale_time <- function(time, to, from = NULL) {
  if (is(time, "Duration") && !is.null(from)) {
    warning("time is supplied as a duration so from will be ignored!", call. = FALSE, immediate. = TRUE)
  } else if (!is(time, "Duration")) {
    if (is.null(from)) stop("supplied times is not a duration object and therefore requires specifying from unit", call. = FALSE)
    time <- duration(time, from)
  }
  time / duration(1, to)
}

# get the scaling factor for a simple si unit prefix
# @param unit the unit to find the suffix for (e.g. mm, kg, ms, nA, kV)
# @param suffix the expected suffix (e.g. m, g, s, A, V)
# @note does not currently process compound units (e.g. m/s) or powers (e.g. cm2 s-1)
get_si_prefix_scaling <- function(unit, suffix) {
  #safety checks
  if(missing(unit)) stop("no unit supplied", call. = FALSE)
  if(missing(suffix)) stop("no unit suffix specified", call. = FALSE)
  
  # supported prefixes
  prefix <- c(f = 1e-15, p = 1e-12, n = 1e-9, "\U00B5" = 1e-6, m = 1e-3, 1,
              k = 1e3, M = 1e6, G = 1e9, T = 1e12)

  # generate pattern
  prefix_pattern <- prefix %>% names() %>% str_c(collapse="|")
  pattern <- sprintf("^(%s)%s$", prefix_pattern, suffix)
  prefixes <- unit %>% str_match(pattern) %>% { .[,2] }
  if (any(is.na(prefixes))) {
    stop("Encountered unrecognized units: ", 
         unit[is.na(prefixes)] %>% str_c(collapse = ", "),
         ". Supported are for this suffix: ", 
         prefix %>% names() %>% str_c(suffix) %>% str_c(collapse = ", "),
         call. = FALSE)
  }

  # scaling
  prefix[sapply(prefixes, function(i) which(i == names(prefix)))] %>% unname()
}

# get unit scaling information (base unit and si prefix)
# @param unit unit to find scaling for
# @param base the valid base units(s) e.g. c("kg", "V")
get_unit_scaling <- function(unit, base_units) {
  if(missing(unit) || missing(base_units)) stop("missing parameters", call. = FALSE)
  if (length(unit) != 1) 
    stop("can only find scaling for one unit at a time, received ", str_c(unit, collapse = ", "), call. = FALSE)
  base_unit <- str_extract(unit, sprintf("(%s)$", str_c(base_units, collapse = "|")))
  if (is.na(base_unit))
    stop("encountered invalid unit, expected '", str_c(base_units, collapse = "' or '"),
         "' with a valid SI prefix but received: ", unit, call. = FALSE)
  list(
    base_unit = base_unit,
    si_scaling = get_si_prefix_scaling(unit, base_unit)
  )
}

# Scale signal (voltage or current)
# @param data the data frame
# @param signal_cols the signal columns to convert, must have format [vi](\\d+)\\.(\\w+) to specifiy both mass and units
# @param to unit to convert to
# @param R resistor value(s), named vector with Rm (m=mass) as names (e.g. c(R45=0.3, R46=3)), in units of R_units
# @param R_units what units resistances are in
# @param V_pattern regular expression pattern how to recognize voltage columns and detect the masses they belong to (default is v followed by a number, e.g. v45)
# @param I_prefix prefix for the newly created current columns, the suffix is automatically the current units
# @note consider exporting this function
scale_signals <- function(data, signal_cols, to, R = c(), R_units = "GOhm", quiet = setting("quiet")) {
  
  # safety checks
  if(missing(data) || !is.data.frame(data))  stop("data has to be supplied as a data frame to ", sys.call(0), call. = FALSE)
  if(missing(signal_cols) || is.null(signal_cols) || missing(to)) stop("signal_cols and to parameters required", call. = FALSE)
  if(!missing(R) && ( !is.vector(R, "numeric") || is.null(names(R)) || any(names(R) == ""))) 
    stop("resistance values have to be a named numeric vector - e.g., c(R45=0.3)", call. = FALSE)
  if (missing(R)) R <- rep(NA_real_, length(signal_cols))
  
  # signal columns
  v_prefix <- "v" # voltage columns prefix
  i_prefix <- "i" # current columns prefix
  signal_pattern <- sprintf("^[%s%s](\\d+)\\.(\\w+)$", v_prefix, i_prefix)
  if (any(wrong <- !str_detect(signal_cols, signal_pattern))) {
    stop("some signal columns do not fit the expected pattern for signal colum names: ",
         str_c(signal_cols[wrong], collapse =", "), call. = FALSE)
  }
  cols <- names(data)
  cols_idx <- sapply(signal_cols, function(col) which(col == cols) %>% {.[1]})
  if(any(wrong <- is.na(cols_idx))) {
    stop("some signal columns do not exist in the provided data frame: ",
         str_c(signal_cols[wrong], collapse = ", "), call. = FALSE)
  }
  
  # get all the masses and units
  cols_masses <- signal_cols %>% str_match(signal_pattern) %>% {.[,2]}
  cols_Rs <- str_c("R", cols_masses)
  cols_units <- signal_cols %>% str_match(signal_pattern) %>% {.[,3]} %>% 
    lapply(get_unit_scaling, base_units = c("V", "A"))
  cols_base_units <- sapply(cols_units, `[[`, "base_unit")
  to_units <- get_unit_scaling(to, c("V", "A"))
  R_units <- get_unit_scaling(R_units, "Ohm")
  
  # check that all columns that require V to I or vice versa have resistor values
  VtoI <- if (to_units$base_unit == "A") cols_base_units == "V" else rep(FALSE, length(cols_idx))
  ItoV <- if (to_units$base_unit == "V") cols_base_units == "A" else rep(FALSE, length(cols_idx))
  column_prefix <- if (to_units$base_unit == "A") i_prefix else if (to_units$base_unit == "V") v_prefix else stop("can't happen")
  if (length(missing <- setdiff(c(cols_Rs[VtoI], cols_Rs[ItoV]), names(R)))) {
    stop("not all resistors required for voltage/current conversion were provided, missing: ",
         str_c(missing, collapse = ", "), call. = FALSE)
  }
  
  # data scaling
  signal_scaling <- 
    data_frame(
      col_name = signal_cols,
      col_scale = sapply(cols_units, `[[`, "si_scaling"),
      col_idx = cols_idx,
      mass = cols_masses,
      VtoI = VtoI,
      ItoV = ItoV,
      scale = !VtoI & !ItoV,
      to_col_name = str_c(column_prefix, mass, ".", to),
      to_scale = to_units$si_scaling,
      R = R[cols_Rs],
      R_scale = R_units$si_scaling
    )
  
  # show scaling if in debug mode
  if (setting("debug")) {
    message("DEBUG MSG: Using the following conversion table for signal scaling:")
    print(signal_scaling)
  }
    
  # convert
  for (i in 1:nrow(signal_scaling)) {
    # find value multiplier
    multiplier <- with(signal_scaling[i, ], {
      if (ItoV) col_scale*R*R_scale/to_scale
      else if (VtoI) col_scale/(R*R_scale * to_scale)
      else col_scale/to_scale
    })
    col_idx <- signal_scaling$col_idx[i]
    new_name <- signal_scaling$to_col_name[i]
    data[,col_idx] <- data[,col_idx] * multiplier
    names(data)[col_idx] <- new_name
  }
  
  return(data)
}

