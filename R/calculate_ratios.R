
#' Calculate ratios from primary data
#' 
#' @inheritParams aggregate_raw_data
#' @param ratios which ratios to calculate (e.g. c("45/44", "46/44")), will only be calculated in files that have the necessary mass column
#' @return isofiles with ratios added
#' @export
calculate_ratios <- function(isofiles, ratios, quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only calculate ratios for iso files", call. = FALSE)
  if(missing(ratios) || is.null(ratios)) stop("no ratios provided for ratio calculations", call. = FALSE)
  isofiles_out <- as_isofile_list(isofiles)
  
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
               list(n = length(isofiles_out), ratios = str_c(ratios, collapse =", "))) %>% message()
  }
  
  # make sure data is provided
  check_read_options(isofiles, "raw_data")
  
  # calculate ratios for all isofiles
  mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
  calculate_isofile_ratios <- function(isofile) {
    
    if (isofile$read_options$raw_data) {
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
  isofiles_out <- as_isofile_list(isofiles) %>% 
    lapply(calculate_isofile_ratios)
    
  # return single (if passed in as single) 
  if (is_isofile(isofiles)) return (isofiles_out[[1]])
  return(isofiles_out)
}