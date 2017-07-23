#' Aggregate raw data
#' 
#' @param isofiles collection of isofile objects
#' @family data aggregation functions
#' @export
aggregate_raw_data <- function(isofiles) {
  isofiles <- as_isofile_list(isofiles)
  check_read_options(isofiles, "raw_data")
  
  file_id <- NULL # global vars
  lapply(isofiles, function(isofile) {
    as_data_frame(isofile$raw_data) %>% 
      mutate(file_id = isofile$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
}

#' Aggregate file info
#'
#' Note file info entries with multiple values are concatenated for this general purpose function.
#' To get access to a specific multi-value file info, access using $file_info[['INFO_NAME']]
#'
#' @inheritParams aggregate_raw_data
#' @family data aggregation functions
#' @export
aggregate_file_info <- function(isofiles) {
  isofiles <- as_isofile_list(isofiles)
  check_read_options(isofiles, "file_info")
  
  lapply(isofiles, function(isofile) {
    lapply(isofile$file_info, function(entry) {
      if (length(entry) > 1) str_c(entry, collapse = "; ") else entry
    })  %>% as_data_frame()
  }) %>% bind_rows()
}

#' Aggregate standards from methods info
#'
#' Aggregates the isotopic standard information recovered from the provided isofiles. Can aggregate just the standards' delta values or combine the delta values with the recovered ratios (if any). Use paramter \code{with_ratios} to exclude/include the ratios.
#'
#' @inheritParams aggregate_raw_data
#' @param with_ratios whether to include ratios or just standard delta values
#' @family data aggregation functions
#' @export
aggregate_standards_info <- function(isofiles, with_ratios = TRUE) {
  isofiles <- as_isofile_list(isofiles)
  check_read_options(isofiles, "method_info")
  
  lapply(isofiles, function(isofile) {
    if(with_ratios) {
      stds <- left_join(
        isofile$method_info$standards,
        isofile$method_info$reference_ratios,
        by = "reference")
    } else {
      stds <- isofile$method_info$standards
    }
    
    file_id <- NULL # global vars
    stds %>% 
      mutate(file_id = isofile$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
}


#' Aggregate vendor computed table data
#' 
#' @inheritParams aggregate_raw_data
#' @param with_units whether to include units in the column headers or not
#' @family data aggregation functions
#' @export
aggregate_vendor_data_table <- function(isofiles, with_units = TRUE) {
  isofiles <- as_isofile_list(isofiles)
  check_read_options(isofiles, "vendor_data_table")
  column <- column_with_units <- file_id <- NULL # global vars
  lapply(isofiles, function(isofile) {
    df <- isofile$vendor_data_table
    if (nrow(df) == 0) return(df)
    if (with_units && is.null(attr(df, "units")))  {
      warning("isofile ", isofile$file_info$file_id, " does not have unit information in its vendor data table", call. = FALSE, immediate. = TRUE)
    } else if (with_units) {
      cols_with_units <- attr(df, "units") %>% select(column, column_with_units) %>% deframe()
      names(df) <- cols_with_units[names(df)]
    }
    df %>% 
      mutate(file_id = isofile$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
}

# check if read options are compatible
check_read_options <- function(isofiles, option) {
  option_values <- map(isofiles, "read_options") %>% map_lgl(option)
  if (!all(option_values)) {
    warning(sum(!option_values), "/", length(isofiles), 
            " files were read without extracting the ", str_replace_all(option, "_", " "), 
            " (parameter '", str_c("read_", option), 
            "=FALSE') and will have missing values in the aggregated data",
            call. = FALSE, immediate. = TRUE)
  }
}
