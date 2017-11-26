
#' Calculate ratios
#' 
#' Calculate ratios from raw data. Note that these are recorded ratios and are not normalized against any standards yet. The ratios are calculated straight from the raw data without any unit conversions, i.e. if the raw data is in mV, the ratio is mV/mV, if in nA the ratio is nA/nA, etc. These raw ratios are subsequently NOT identical to absolute ratios, in fact they are usually not even close (especially if raw data recorded as voltages with different resistors). If raw data is first converted to identical current units (\code{\link{iso_convert_signals}}), the ratios may be close to their true values (+instrument fractionation), however, isotope ratios should always be calibrated against reference ratios measured in the same data file.
#' 
#' @inheritParams iso_aggregate_raw_data
#' @param ratios which ratios to calculate (e.g. c("45/44", "46/44")), will only be calculated in files that have the necessary mass column
#' @return ithe passed in iso_file(s) with ratios added
#' @export
iso_calculate_ratios <- function(iso_files, ratios, quiet = default(quiet)) {
  
  # safety checks
  if(!iso_is_object(iso_files)) stop("can only calculate ratios for iso files", call. = FALSE)
  if(missing(ratios) || is.null(ratios)) stop("no ratios provided for ratio calculations", call. = FALSE)
  if(!is.logical(quiet)) stop("quiet must be TRUE or FALSE - make sure to pass ratios as a vector, not separate arguments", call. = FALSE)
  single_file <- iso_is_file(iso_files) # to make sure return is the same as supplied
  iso_files <- iso_as_file_list(iso_files)
  
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
               list(n = length(iso_files), ratios = str_c(ratios, collapse =", "))) %>% message()
  }
  
  # make sure data is provided
  check_read_options(iso_files, "raw_data")
  
  # calculate ratios for all iso_files
  mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
  calculate_iso_file_ratios <- function(iso_file) {
    
    if (iso_file$read_options$raw_data && nrow(iso_file$raw_data) > 0) {
      # generate mass lookup
      mass_lookup <- names(iso_file$raw_data) %>%
        str_subset(mass_column_pattern) %>%
        str_match(mass_column_pattern) %>%
        { setNames(.[,1], .[,2]) }
      
      # generate ratios
      for (i in 1:nrow(ratio_columns)) {
        if (!is.na(top <- mass_lookup[ratio_columns$top[i]]) && !is.na(bot <- mass_lookup[ratio_columns$bot[i]])) 
          iso_file$raw_data[[ratio_columns$column[i]]] <- iso_file$raw_data[[top]] / iso_file$raw_data[[bot]]
      }
    } 
    
    return(iso_file)
  }
  
  # apply calculations
  iso_files <- iso_files %>% lapply(calculate_iso_file_ratios) %>% iso_as_file_list()
    
  # return single (if passed in as single) 
  if (single_file) return (iso_files[[1]])
  return(iso_files)
}