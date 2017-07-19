# unit conversion functions ----

#' Convert time units in continuous flow files
#' 
#' This function can be used to convert the time units of a collection of isofiles to a new common time unit. The original time units are inferred from the naming of the time column. New time units are very flexible and can be anything that \code{\link[lubridate]{duration}} understands, i.e. "s", "seconds", "min", "minutes", "hours", "days", etc. are all valid units.
#' 
#' @inheritParams isoplot_raw_data
#' @param to what time units to convert to
#' @return the passed in isofiles with changed time units
convert_time <- function(isofiles, to, quiet = setting("quiet")) {
  
  # checks
  if(!is_continuous_flow(isofiles)) stop("can only convert time in continuous flow isofiles", call. = FALSE)
  if(missing(to)) stop("no time unit to convert to specified", call. = FALSE)
  
  if (!setting("quiet")) 
    sprintf("Info: converting time to '%s' for %d continuous flow data file(s)", to, length(isofiles)) %>% message()
  
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


#' Convert voltages to currents
#' Automatically finds the matching voltage columns that fit the resistor mass names and converts to currents in the supplied I_units. All voltage columns should be in units of V_units.
#' @param data the data frame
#' @param R resistor value(s), named vector with Rm (m=mass) as names (e.g. c(R45=0.3, R46=3)), in units of R_units
#' @param V_units what units voltages are in (default mV, i.e. 10^-3)
#' @param I_units what units currents are in (default nA, i.e. 10^-9)
#' @param R_units what units resistances are in (default GOhm, i.e. 10^9)
#' @param V_pattern regular expression pattern how to recognize voltage columns and detect the masses they belong to (default is v followed by a number, e.g. v45)
#' @param I_prefix prefix for the newly created current columns, the suffix is automatically the current units
#' @inheritParams isoread_files
#' @note make sure that existing currents are not overwritten (e.g. in data frame that comes jointly from isodat and elementar)
#' @note consider automatically guessing V_units from the voltage columns?
#' @export
convert_voltages_to_currents <- function(data, R, V_units = "mV", I_units = "nA", R_units = "GOhm", 
                                         V_pattern = "^[vV](\\d+)", I_prefix = "i", quiet = setting("quiet")) {
  # safety checks
  if(missing(data) || !is.data.frame(data))  stop("data has to be supplied as a data frame to ", sys.call(0), call. = FALSE)
  if(missing(R) || !is.vector(R, "numeric") || is.null(names(R)) || any(names(R) == "")) 
    stop("resistance values have to be a named numeric vector - e.g., c(R45=0.3)", call. = FALSE)
  V_si_prefix <- get_si_prefix_scaling(V_units, "V")
  I_si_prefix <- get_si_prefix_scaling(I_units, "A")
  R_si_prefix <- get_si_prefix_scaling(R_units, "Ohm")
  
  # find voltage columns
  V_cols <- names(data)[names(data) %>% str_detect(V_pattern)]
  if (length(V_cols) == 0) stop("no voltage columns found (pattern=", V_pattern, ")", call. = FALSE)
  masses <- V_cols %>% str_match(V_pattern) %>% {.[,2]}
  I_cols <- str_c(I_prefix, masses, ".", I_units)
  R_cols <- str_c("R", masses)
  
  # check if any resistance values don't match an existing voltage column
  if ( length(unmatched_R <- setdiff(names(R), R_cols)) > 0 )
    warning("not all provided R values have matching voltage columns: ", 
            str_c(unmatched_R, collapse = ", "), call. = FALSE, immediate. = TRUE)
  
  # convert values
  V_cols <- V_cols[R_cols %in% names(R)] # only consider those that actually have R supplied
  I_cols <- I_cols[R_cols %in% names(R)]
  R_cols <- R_cols[R_cols %in% names(R)]
  prefix_conversion <- V_si_prefix / (R_si_prefix * I_si_prefix)
  data[,I_cols] <- data[,V_cols] / rep(R[R_cols], each = nrow(data)) * prefix_conversion
  
  # info message
  if (!setting("quiet")) {
    sprintf("Info: converted %d voltage columns (%s [%s]) to currents (%s [%s]) with resistances %s %s", 
            length(V_cols), str_c(V_cols, collapse = ", "), V_units, str_c(I_cols, collapse = ", "), I_units,
            str_c(R[R_cols], collapse = ", "), R_units) %>% 
      message()
  }
  
  return(data)
}

