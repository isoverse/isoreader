# File info and data retrieval functions 

# Specific file info calls ======

#' Get file information
#' 
#' Retrieve basic file information form an individual isotope file (isofile) object. All of these can also be recoverd for an entire set of files using \code{\link{aggregate_file_info}} and specifiying which info to recover, for example, \code{include = c("file_id", "file_path", "file_datetime")}
#' 
#' @details \code{get_file_id()}: retrieve the file ID (this is typially the file name)
#' @param isofile an isofile to retrieve basic file information from
#' @rdname file_info
#' @aliases get_file_info
#' @family data retrieval functions
#' @export
get_file_id <- function(isofile) {
  check_iso_file_param(isofile)
  return(isofile$file_info$file_id)
}

#' @details \code{get_file_path()}: retrieve the file path (this is the path to the file in case of single file formats such as .dxf or .did and the path to the archieve file in case of collective file formats such as .iarc)
#' @rdname file_info
get_file_path <- function(isofile) {
  check_iso_file_param(isofile)
  return(isofile$file_info$file_path)
}

#' @details \code{get_file_subpath()}: retrieve the file subpath (this only exists for collective file formats such as .iarc and is the name of the metadata file inside the .iarc archive). Returns NA for isofile without subpath.
#' @rdname file_info
get_file_subpath <- function(isofile) {
  check_iso_file_param(isofile)
  return(isofile$file_info$file_subpath)
}

#' @details \code{get_file_datetime()}: retrieve the run date and time in \code{\link[base]{POSIXct}} format
#' @rdname file_info
get_file_datetime <- function(isofile) {
  check_iso_file_param(isofile)
  return(isofile$file_info$file_datetime)
}

# internal convenience function
check_iso_file_param <- function(isofile) {
  if(missing(isofile)) stop("no isofile provided to retrieve file information from", call. = FALSE)
  if(!is_isofile(isofile)) stop("can only retrieve file information from an isofile object, not from '", class(isofile)[1], "'", call. = FALSE)
}

# Specific data aggregation calls =====

#' Aggregate file info
#'
#' Combine file information from multiple isofiles. By default all information is included but specific items can be specified using the \code{include} parameter. The file id is always included. File information beyond \code{file_id} and \code{file_path} is only available if the isofiles were read with parameter \code{read_file_info=TRUE}.
#'
#' @inheritParams aggregate_raw_data
#' @param select which file information to select. All by default.
#' @family data retrieval functions
#' @note File info entries with multiple values are concatenated for this aggregation function. To get access to a specific multi-value file info entry, access using \code{isofile$file_info[['INFO_NAME']]} on the isofile object directly.
#' @export
aggregate_file_info <- function(isofiles, select = all_info(), quiet = setting("quiet")) {
  isofiles <- as_isofile_list(isofiles)
  if (!quiet) sprintf("Info: aggregating file info from %d data file(s)", length(isofiles)) %>% message()
  check_read_options(isofiles, "file_info")
  
  # retrieve info
  info <- lapply(isofiles, function(isofile) {
    lapply(isofile$file_info, function(entry) {
      if (length(entry) > 1) str_c(entry, collapse = "; ") else entry
    })  %>% as_data_frame()
  }) %>% bind_rows()
  
  # safety check (probably not necessary because of isofile combination checks but consequences would be too problematic not to check)
  if (any(duplicated(info$file_id))) {
    stop("duplicate file ids are not permitted as they can lead to unexpected consequences in data processing", call. = FALSE)
  }
  
  # get include information
  all_info <- function() names(info)
  select_cols <- select %>% { .[. %in% all_info()] }
  if (!is.character(select_cols)) 
    stop("'select' parameter must be omitted or a character vector of file info entry names", call. = FALSE)
  if (length(missing <- setdiff(select, select_cols)) > 0) {
    warning("some requested file info entries do not exist in any of the provided isofiles and are omitted: '",
            str_c(missing, collapse = "', '"), "'", call. = FALSE, immediate. = TRUE)
  }
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  
  return(info[select_cols])
}

#' Aggregate raw data
#' 
#' Aggregate the raw ion data from the provided isofiles. Can aggregate either in a wide table (for easy overview) or a gathered long table (for plotting and further data processing). The raw data is only available if the isofiles were read with parameter \code{read_raw_data=TRUE}.
#' 
#' @inheritParams isoread_files
#' @param isofiles collection of isofile objects
#' @param gather whether to gather data into long format after aggregation (e.g. for plotting)
#' @param include_file_info if provided, will include the requested file information (see \code{\link{aggregate_file_info}}) with the raw data
#' @family data retrieval functions
#' @export
aggregate_raw_data <- function(isofiles, gather = FALSE, include_file_info = c(), quiet = setting("quiet")) {
  isofiles <- as_isofile_list(isofiles)
  if (!quiet) { 
    sprintf("Info: aggregating raw data from %d data file(s)%s", length(isofiles),
            get_info_message_concat(include_file_info, prefix = ", including file info ")) %>% message()
  }
  check_read_options(isofiles, "raw_data")
  
  data <- 
    lapply(isofiles, function(isofile) {
      data <- as_data_frame(isofile$raw_data) %>% 
        mutate(file_id = isofile$file_info$file_id) %>% 
        select(file_id, everything())
      return(data)
    }) %>% bind_rows()
  
  # check for rows
  if (nrow(data) == 0) return(data)
  
  # if gathering
  if (gather) {
    column <- value <- extra_parens <- category <- NULL # global vars
    masses_ratios_re <- "^([vir])(\\d+/?\\d*)(\\.(.+))?$"
    data <- data %>% 
      # gather all masses and ratios
      gather(column, value, matches(masses_ratios_re)) %>% 
      # extract unit information
      extract(column, into = c("category", "dataset", "extra_parens", "units"), regex = masses_ratios_re) %>% 
      select(-extra_parens) %>% 
      # remove unknown data
      filter(!is.na(value)) %>% 
      # assign category
      mutate(category = ifelse(category == "r", "ratio", "mass"))
  } 
  
  # if file info
  if (!is.null(include_file_info)) {
    info <- aggregate_file_info(isofiles, include_file_info, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}


#' Aggregate standards from methods info
#'
#' Aggregates the isotopic standard information recovered from the provided isofiles. Can aggregate just the standards' delta values or combine the delta values with the recovered ratios (if any). Use paramter \code{with_ratios} to exclude/include the ratios. This information is only available if the isofiles were read with parameter \code{read_method_info=TRUE}.
#'
#' @inheritParams aggregate_raw_data
#' @param with_ratios whether to include ratios or just standard delta values
#' @family data retrieval functions
#' @export
aggregate_standards_info <- function(isofiles, with_ratios = FALSE, include_file_info = c(), quiet = setting("quiet")) {
  isofiles <- as_isofile_list(isofiles)
  if (!quiet) { 
    sprintf("Info: aggregating standards info from %d data file(s)%s", length(isofiles),
            get_info_message_concat(include_file_info, prefix = ", including file info ")) %>% message()
  }
  
  check_read_options(isofiles, "method_info")
  
  # aggregate standards info
  data <- lapply(isofiles, function(isofile) {
    if(with_ratios) {
      stds <- left_join(
        isofile$method_info$standards,
        isofile$method_info$reference_ratios,
        by = "reference")
    } else {
      stds <- isofile$method_info$standards
    }
    
    # check if there is any data
    if(is.null(stds) || nrow(stds) == 0) return(data_frame())
    
    # return with file_id included
    stds %>% 
      mutate(file_id = isofile$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
  
  # if file info
  if (!is.null(include_file_info)) {
    info <- aggregate_file_info(isofiles, include_file_info, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}

#' Aggregate resistors from methods info
#'
#' Aggregates the resistor information recovered from the provided isofiles. This information is only available if the isofiles were read with parameter \code{read_method_info=TRUE} and only linked to specific masses if the isofiles were additionally read with parametr \code{read_raw_data=TRUE}.
#'
#' @inheritParams aggregate_raw_data
#' @family data retrieval functions
#' @export
aggregate_resistors_info <- function(isofiles, include_file_info = c(), quiet = setting("quiet")) {
  isofiles <- as_isofile_list(isofiles)
  if (!quiet) { 
    sprintf("Info: aggregating resistors info from %d data file(s)%s", length(isofiles),
            get_info_message_concat(include_file_info, prefix = ", including file info ")) %>% message()
  }
  
  check_read_options(isofiles, "method_info")
  
  # aggregate standards info
  data <- lapply(isofiles, function(isofile) {
    Rs <- isofile$method_info$resistors
    
    # check if there is any data
    if(is.null(Rs) || nrow(Rs) == 0) return(data_frame())
    
    # return with file_id included
    Rs %>% 
      mutate(file_id = isofile$file_info$file_id) %>% 
      select(file_id, everything())
  }) %>% bind_rows()
  
  # if file info
  if (!is.null(include_file_info)) {
    info <- aggregate_file_info(isofiles, include_file_info, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  return(data)
}

#' Aggregate vendor computed table data
#' 
#' Aggregate data from the vendor-computed data table. This information is only available if the isofiles were read with parameter \code{read_vendor_data_table=TRUE}.
#' 
#' @inheritParams aggregate_raw_data
#' @param with_units whether to include units in the column headers (if there are any) or not
#' @param select which vendor table columns select. All by default.
#' @family data retrieval functions
#' @export
aggregate_vendor_data_table <- function(isofiles, with_units = TRUE, select = all_columns(), include_file_info = c(), 
                                        quiet = setting("quiet")) {
  isofiles <- as_isofile_list(isofiles)
  if (!quiet) { 
    sprintf("Info: aggregating vendor data table %s from %d data file(s)%s", 
            if (with_units) "with units" else "without units",
            length(isofiles),
            get_info_message_concat(include_file_info, prefix = ", including file info ")) %>% message()
  }
  check_read_options(isofiles, "vendor_data_table")
  
  # check for missing with units
  if (with_units && (no_units <- sum(sapply(isofiles, function(isofile) is.null(attr(isofile$vendor_data_table, "units"))))) > 0) {
    sprintf("%d/%d files do not have unit information for their vendor data table and will have missing units",
            no_units, length(isofiles)) %>% warning(call. = FALSE, immediate. = TRUE)
  }
  
  # get vendor data
  column <- units <- NULL # global vars
  data <- lapply(isofiles, function(isofile) {
    df <- isofile$vendor_data_table
    
    # see if there is any data at all
    if (nrow(df) == 0) return(df)
    
    # use units 
    if (with_units && !is.null(attr(df, "units")) && !is.na(attr(df, "units")))  {
      cols_with_units <- attr(df, "units")[c("column", "units")] %>% 
        mutate(units = ifelse(!is.na(units) & nchar(units) > 0, str_c(column, " ", units), column)) %>% 
        deframe()
      names(df) <- cols_with_units[names(df)]
    }
    
    # include file id
    df %>% 
      mutate(file_id = isofile$file_info$file_id) %>% 
      dplyr::select(file_id, everything())
  }) %>% bind_rows()
  
  # check for any rows
  if (nrow(data) == 0) return(data)
  
  # get select information
  all_columns <- function() names(data)
  select_cols <- select %>% { .[. %in% all_columns()] }
  if (length(missing <- setdiff(select, select_cols)) > 0) {
    warning("some requested vendor data table columns do not exist in any of the provided isofiles and are omitted: '",
            str_c(missing, collapse = "', '"), "'", call. = FALSE, immediate. = TRUE)
  }
  if (!"file_id" %in% select_cols) 
    select_cols <- c("file_id", select_cols) # file info always included
  data <- data[select_cols]
  
  # include file info
  if (!is.null(include_file_info)) {
    info <- aggregate_file_info(isofiles, include_file_info, quiet = TRUE)
    data <- right_join(info, data, by = "file_id")
  }
  
  return(data)
}

# check if read options are compatible
check_read_options <- function(isofiles, option) {
  isofiles <- as_isofile_list(isofiles)
  option_values <- map(isofiles, "read_options") %>% map_lgl(option)
  if (!all(option_values)) {
    warning(sum(!option_values), "/", length(isofiles), 
            " files were read without extracting the ", str_replace_all(option, "_", " "), 
            " (parameter '", str_c("read_", option), 
            "=FALSE') and will have missing values",
            call. = FALSE, immediate. = TRUE)
  }
}
