
#' Calculate ratios from primary data
#' 
#' @inheritParams isoread_files
#' @param isofiles
#' @param ratios which ratios to calculate (e.g. c("45/44", "46/44")), will only be calculated in files that have the necessary mass column
#' @return isofiles with ratios added
#' @export
calculate_ratios <- function(isofiles, ratios, quiet = setting("quiet")) {
  
  # safety checks
  if(!is_iso_object(isofiles)) stop("can only calculate ratios for iso files", call. = FALSE)
  if(missing(ratios) || is.null(ratios)) stop("no ratios provided for ratio calculations", call. = FALSE)
  
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

# calculation functions for plotting =====

# helper function to process mass and ratio requests for plotting functions
# peforms all the necessary safety checks
# @return a data frame with all requested masses and ratios
wupsie_get_mass_and_ratio_definitions <- function(raw_data, masses, ratios) {
  
  # global vars
  mass <- column <- ratio <- top <- bot <- label <- NULL
  
  # masses
  mass_column_pattern <- "^[vi](\\d+)\\.(.*)$"
  mass_columns <- names(raw_data) %>% 
    str_subset(mass_column_pattern) %>% 
    str_match(mass_column_pattern) %>%  
    { data_frame(column = .[,1], mass = .[,2], units = .[,3]) }
  mass_lookup <- select(mass_columns, mass, column) %>% deframe()
  
  # get alll masses if none provided
  if (!is.null(masses) &&  is.na(masses)) masses <- mass_columns$mass
  else masses <- as.character(masses)
  
  # safety check
  if(length(masses) == 0 && length(ratios) == 0) stop("must specify at least one mass or ratio", call. = FALSE)
  
  # ratios
  ratio_pattern <- "^(\\d+)/(\\d+)$"
  if (!all(ok <- str_detect(ratios, ratio_pattern))) {
    stop("invalid ratio(s): ", str_c(ratios[!ok], collapse = ", "), call. = FALSE)
  }
  if (length(ratios) > 0) {
    ratio_columns <- ratios %>% 
      str_match(ratio_pattern) %>% 
      { data_frame(column = str_c("ratio.",.[,1]), ratio = .[,1], top = .[,2], bot = .[,3], units = "") }
  } else {
    ratio_columns <- data_frame(column = "", ratio = "", top = "", bot = "", units = "")[0,]
  }
  
  # more safety checks
  all_needed_masses <- unique(c(masses, ratio_columns$top, ratio_columns$bot))
  if ( length(missing <- setdiff(all_needed_masses, mass_columns$mass)) > 0 ) {
    stop("mass(es) not available in the provided isofiles: ", str_c(missing, collapse = ", "), call. = FALSE)
  }
  
  # all data columns to plot (mass and ratio)
  bind_rows(
    mass_columns %>% 
      filter(mass %in% masses) %>% 
      rename(label = mass) %>% 
      mutate(type = "mass"),
    ratio_columns %>% 
      rename(label = ratio) %>% 
      mutate(type = "ratio",
             top = mass_lookup[top],
             bot = mass_lookup[bot])
  ) %>% mutate(
    label_with_units = ifelse(nchar(units) > 0, str_c(label, " [", units, "]"), label)
  )
}

# calculate derived ratios in mass data
# @param ratio_columns is a data frame with columns 'column', 'top' and 'bot' for calculating the resulting ratio of top/bot and storing it in the new column 'column'
blabla_calculate_ratios <- function(raw_data, ratio_columns) {
  if(!all(c("column", "top", "bot") %in% names(ratio_columns))) stop("columns missing", call. = FALSE)
  if(any(is.na(ratio_columns$column)) || any(is.na(ratio_columns$top)) || any(is.na(ratio_columns$bot)))
    stop("missing values", call. = FALSE)
  
  # generate ratios
  if (nrow(ratio_columns) > 0) {
    for (i in 1:nrow(ratio_columns)) {
      raw_data[[ratio_columns$column[i]]] <- raw_data[[ratio_columns$top[i]]] / raw_data[[ratio_columns$bot[i]]]
    }
  }
  return(raw_data)
}
