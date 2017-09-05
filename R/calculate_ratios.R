
#' Calculate ratios
#' 
#' Calculate ratios from raw data. Note that these are recorded ratios and are not normalized against any standards yet. The ratios are calculated straight from the raw data without any unit conversions, i.e. if the raw data is in mV, the ratio is mV/mV, if in nA the ratio is nA/nA, etc. These raw ratios are subsequently NOT identical to absolute ratios, in fact they are usually not even close (especially if raw data recorded as voltages with different resistors). If raw data is first converted to identical current units (\code{\link{convert_signals}}), the ratios may be close to their true values (+instrument fractionation), however, isotope ratios should always be calibrated against reference ratios measured in the same data file.
#' 
#' @inheritParams aggregate_raw_data
#' @param ratios which ratios to calculate (e.g. c("45/44", "46/44")), will only be calculated in files that have the necessary mass column
#' @return ithe passed in isofile(s) with ratios added
#' @export
calculate_ratios <- function(isofiles, ratios, quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only calculate ratios for iso files", call. = FALSE)
  if(missing(ratios) || is.null(ratios)) stop("no ratios provided for ratio calculations", call. = FALSE)
  if(!is.logical(quiet)) stop("quiet must be TRUE or FALSE - make sure to pass ratios as a vector, not separate arguments", call. = FALSE)
  single_file <- is_isofile(isofiles) # to make sure return is the same as supplied
  isofiles <- as_isofile_list(isofiles)
  
  # ratios
  ratio_pattern <- "^(\\d+)/(\\d+)$"
  if (!all(ok <- str_detect(ratios, ratio_pattern))) {
    stop("invalid ratio(s): ", str_c(ratios[!ok], collapse = ", "), call. = FALSE)
  }
  ratio_columns <- ratios %>% 
    str_match(ratio_pattern) %>% 
    { data_frame(column = str_c("r",.[,1]), ratio = .[,1], top = .[,2], bot = .[,3]) }

  # information
  if (!quiet) {
    str_interp("Info: calculating ratio(s) in $[d]{n} data file(s): ${ratios}", 
               list(n = length(isofiles), ratios = str_c(ratios, collapse =", "))) %>% message()
  }
  
  # make sure data is provided
  check_read_options(isofiles, "raw_data")
  
  # calculate ratios for all isofiles
  mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
  calculate_isofile_ratios <- function(isofile) {
    
    if (isofile$read_options$raw_data && nrow(isofile$raw_data) > 0) {
      # generate mass lookup
      mass_lookup <- names(isofile$raw_data) %>%
        str_subset(mass_column_pattern) %>%
        str_match(mass_column_pattern) %>%
        { setNames(.[,1], .[,2]) }
      
      # generate ratios
      for (i in 1:nrow(ratio_columns)) {
        if (!is.na(top <- mass_lookup[ratio_columns$top[i]]) && !is.na(bot <- mass_lookup[ratio_columns$bot[i]])) 
          isofile$raw_data[[ratio_columns$column[i]]] <- isofile$raw_data[[top]] / isofile$raw_data[[bot]]
      }
    } 
    
    return(isofile)
  }
  
  # apply calculations
  isofiles <- isofiles %>% lapply(calculate_isofile_ratios) %>% as_isofile_list()
    
  # return single (if passed in as single) 
  if (single_file) return (isofiles[[1]])
  return(isofiles)
}